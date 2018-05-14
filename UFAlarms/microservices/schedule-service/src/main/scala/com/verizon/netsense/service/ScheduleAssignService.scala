package com.verizon.netsense.service

import java.util.{Calendar, UUID}

import akka.actor.ActorSystem
import akka.kafka.ProducerMessage.Message
import akka.kafka.scaladsl.{Consumer, Producer}
import akka.kafka.{ProducerMessage, Subscriptions}
import akka.stream._
import akka.stream.scaladsl.{Broadcast, Flow, GraphDSL, Keep, Merge, Partition, RestartSink, RestartSource, RunnableGraph, Sink}
import com.fasterxml.jackson.databind.JsonMappingException
import com.verizon.ScheduleTuples.STSTuple
import com.verizon.netsense.config.LSSConfigLoader
import com.verizon.netsense.connector.KafkaConnection
import com.verizon.netsense.constants.Constants
import com.verizon.netsense.entity.{ISMessageError, ISMessageSuccess, ISResponseError, ISResponseSuccess}
import com.verizon.netsense.exceptions.CustomExceptions._
import com.verizon.netsense.helper.{LightStateHelper, TimeHelper}
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.model.{LightStatusObj, UnAppliedScheduleLoopTrigger, _}
import com.verizon.netsense.util.{Common, DateTimeUtil, TokenUtil}
import com.verizon.netsense.utils.{DeviceModels, Logging, ObjectMapperUtil, TimeConverter}
import nl.grons.metrics.scala.Timer
import org.apache.kafka.clients.producer.ProducerRecord
import org.joda.time.{DateTime, Duration}

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

/**
  * Created by ssaurav on 1/2/18.
  */
case class ScheduleAssignService()(implicit system: ActorSystem, mat: Materializer, ec: ExecutionContext, dbLayer: DbLayer)
  extends Instrumented
    with Logging
    with Common with TimeConverter with TimeHelper with TokenUtil {

  //For metrics
  private[this] val kafkaSourceTimer: Timer = metrics.timer("kafka-source-consumer")
  private[this] val kafkaSinkTimer: Timer = metrics.timer("kafka-sink-producer")

  private[this] val LFSMsgUnmarshalltimer: Timer = metrics.timer("is-lfs-unmarshal")
  private[this] val DDSchUpdateMsgUnmarshalltimer: Timer = metrics.timer("lss-dd-sch-update-unmarshal")
  private[this] val SchLoopTimer: Timer = metrics.timer("lss-midnight-loop-unmarshal")
  private[this] val SchLoginReqTimer: Timer = metrics.timer("lss-loginreq-unmarshal")
  private[this] val SchNodeUpdateTimer: Timer = metrics.timer("lss-nodeupdate-unmarshal")
  private[this] val LSSSTSCmdKafkaFlowTimer: Timer = metrics.timer("lss-cmd-sts-kafka-sink-producer")
  private[this] val LSSSTSSchKafkaFlowTimer: Timer = metrics.timer("lss-sch-sts-kafka-flow-producer")
  private[this] val LSSISCmdSinkTimer: Timer = metrics.timer("lss-cmd-is-kafka-sink-producer")
  private[this] val LSSGetTokenLatencyTimer: Timer = metrics.timer("lss-get-token-light-timer")
  private[this] val LSSGetNodeHierarchyFromNeo4jTimer: Timer = metrics.timer("lss-get-orghierarchy-neo4j-timer")

  lazy val supportedNodeModels = Vector(DeviceModels.CNEXT.toString)

  //Source Kafka
  val kafkaConnection = new KafkaConnection(system)
  kafkaConnection.configExternalConsumerSettings(LSSConfigLoader.kafkaGroupId)

  def kafkaConsumerSource(topicName: String) = Consumer
    .plainSource(kafkaConnection.consumerSettings, Subscriptions.topics(topicName))
    .map { msg =>
      kafkaSourceTimer.time {
        val str: String = new String(msg.value(), "UTF-8")
        log.info("Kafka Source message: " + str)
        msg.value()
      }
    }.withAttributes(ActorAttributes.supervisionStrategy(sourceDecider))

  var sourceRetryCount: Long = 0
  def kafkaRestartableConsumerSource(topicName: String) =
    RestartSource.withBackoff(
      minBackoff = 1.seconds,
      maxBackoff = 5.seconds,
      randomFactor = 0.2
    ) { () => {
      if (sourceRetryCount == 0) {
        log.info("Starting Consumer for Kafka " + kafkaConnection.kafkaHost
          +  " groupId: " + kafkaConnection.groupId)
      } else {
        log.error("Restarting Consumer for Kafka " + kafkaConnection.kafkaHost
          +  " groupId: " + kafkaConnection.groupId)
      }
      kafkaConsumerSource(topicName)
    }

    }.withAttributes(ActorAttributes.supervisionStrategy(sourceDecider))

  def eventUnmarshallFlow[T](implicit manifest: Manifest[T]) = Flow[Array[Byte]]
    .mapAsync(parallelism)(x => Try(ObjectMapperUtil.fromJsonAsync[T](x)).toEither match {
      case Left(l) => throw l
      case Right(r) => r.recover {
        case ex: JsonMappingException => throw ex
        case ex: Exception            => throw ex
      }
    }).withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  lazy val wrapLightScheduleToSTS = Flow[LightingSchedule]
    .map { x =>
      val timeInMillis = System.currentTimeMillis()
      log.debug(s"LightingSchedule before Solar/DST calculation with time $timeInMillis: " + x.toJSON)
      (x.copy(scheduleid = Some(convertScheduleIdToToken(x.scheduleid
        .getOrElse(throw new NoScheduleFoundException(x.toJSON)), timeInMillis))),
        ScheduleTransformer(timeInMillis).transform(x))
    }
    .map { sch =>
      log.debug("STSModel post DST " + sch._2.toJSON)
      LSSSTSSchKafkaFlowTimer.time()
      val producerRecord = new ProducerRecord[Array[Byte], Array[Byte]](LSSConfigLoader.kafkaScheduleServiceToDevice,
        sch._2.toJsonArray)
      new ProducerMessage.Message[Array[Byte], Array[Byte], STSTuple](record = producerRecord, passThrough = sch)
    }
    .viaMat(Producer.flow(kafkaConnection.producerSettings))(Keep.both)
    .map(x => x.message.passThrough)
    .map(x => LightingSchedule.toLightControlObj(x._1))
    .map {x => log.debug("LightStatusObj sent for light control " + x.toJSON); x }
    .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  def constructProducerRecord(deviceEvents: DeviceEvents, topic: String) = {
    new ProducerRecord[Array[Byte], Array[Byte]](topic, deviceEvents.toJsonArray)
  }

  def constructProducerRecordForCommand(ctrlSTS: Option[LightControlSTSModel], topic: String, lso: LightStatusObj)
  : Message[Array[Byte], Array[Byte], LightStatusObj] = {
    ctrlSTS match {
      case Some(lightControlSTSModel) =>
        val producerRecord = new ProducerRecord[Array[Byte], Array[Byte]](topic, lightControlSTSModel.toJsonArray)
        new ProducerMessage.Message[Array[Byte], Array[Byte], LightStatusObj](record = producerRecord, passThrough = lso)
      case None => new ProducerMessage.Message[Array[Byte], Array[Byte], LightStatusObj](record = null, passThrough = lso)
    }
  }

  lazy val kafkaControlMsgPartitioner: (Message[Array[Byte], Array[Byte], LightStatusObj]) => (Int) = {
    case msg if msg.record != null && msg.record.value() != null && msg.record.value().nonEmpty => 0
    case _ => 1
  }

  lazy val lightControlCmdHandler = Flow[LightStatusObj]
    .map { e =>

      val producerMessage: Message[Array[Byte], Array[Byte], LightStatusObj] = e match {

      // Respond to IS LFS request with success response
      case lso@LightStatusObj(Some(Constants.LIGHTING_FORCE_STATE_STR), Some(nodeids), _, _, _, _,
      Some(Light(_, Some(level), _, _, _, _, _, _, _)),
      Some(LightControlRequestHeaders(_, Some(reqId), _, _, true, _))) if nodeids.nonEmpty  =>
        log.debug(s"Override sent to  nodes $nodeids <= Source: IS $reqId ")
        constructProducerRecordForCommand(Some(LightControlSTSModel(id = reqId, payload = LFSPayload(Array(level)).toMsgPack,
          name = Constants.LIGHTING_FORCE_STATE_STR, nodeids = nodeids)),
          LSSConfigLoader.kafkaLightForceStateToSTSTopic, lso)

      // LightingSetAuto Sent by Platform LightingScheduleEvent
      case lso@LightStatusObj(Some(Constants.LIGHTING_SET_AUTO_STR), Some(nodeIds), _, _, _, _, _,
      Some(LightControlRequestHeaders(_, _, _, Some(schId), true, _))) =>
        constructProducerRecordForCommand(Some(LightControlSTSModel(id = schId, payload = Array[Byte](),
          name = Constants.LIGHTING_SET_AUTO_STR, nodeids = nodeIds)),
          LSSConfigLoader.kafkaLightForceStateToSTSTopic, lso)


      // LightingSetAuto Sent by IS as LightingForceState
      case lso@LightStatusObj(Some(Constants.LIGHTING_SET_AUTO_STR), Some(nodeIds), _, _, _, _, _,
      Some(LightControlRequestHeaders(_, reqId, _, _, true, _))) =>
        constructProducerRecordForCommand(Some(LightControlSTSModel(id = reqId.getOrElse(UUID.randomUUID().toString),
          payload = Array[Byte](),
          name = Constants.LIGHTING_SET_AUTO_STR,
          nodeids = nodeIds)), LSSConfigLoader.kafkaLightForceStateToSTSTopic, lso)

      case lso@LightStatusObj(Some(requestType), _, _, _, _, _, _,
      Some(LightControlRequestHeaders(_, reqId, _, _, false, _)))
        if ScheduleCASEL.predicateForLightControlType(requestType) =>
        constructProducerRecordForCommand(None, LSSConfigLoader.kafkaLightForceStateToSTSTopic, lso)

      case lso => throw new NoResponseSentToISException(lso.toJSON)
    }
      producerMessage
    }
    .withAttributes(ActorAttributes.supervisionStrategy(flowDeciderTest))

  lazy val lightControlISErrorKafkaHandler = Flow[Message[Array[Byte], Array[Byte], LightStatusObj]]
    .map(_.passThrough)

  lazy val lightControlCmdKafkaSink = Flow[Message[Array[Byte], Array[Byte], LightStatusObj]]
    .map{x => LSSSTSCmdKafkaFlowTimer.time(x); x}
    .viaMat(Producer.flow(kafkaConnection.producerSettings))(Keep.right)
    .map(_.message.passThrough)
    .withAttributes(ActorAttributes.supervisionStrategy(flowDeciderTest))

  lazy val lightControlISKafkaSink = Flow[LightStatusObj]
      .map{x => LSSISCmdSinkTimer.time(x); x}
    .map {
      case lso@LightStatusObj(Some(Constants.LIGHTING_FORCE_STATE_STR | Constants.LIGHTING_SET_AUTO_STR),
      _, _, _, _, _, _, Some(LightControlRequestHeaders(Some(msgId), Some(reqId), Some(timeStamp), _, true, _))) =>
        ISMessageSuccess(msgId, ISResponseSuccess(reqId, success = true, timestamp = timeStamp, message = "OK"))

      case lso@LightStatusObj(Some(Constants.LIGHTING_FORCE_STATE_STR | Constants.LIGHTING_SET_AUTO_STR), Some(nodeIds),
      _, _, _, _, _, Some(LightControlRequestHeaders(Some(msgId), Some(reqId), Some(timeStamp), _, false,
      Some(errorMsg)))) =>
        ISMessageError(msgId, ISResponseError(reqId, success = false, timestamp = timeStamp, error = errorMsg,
          status = 500))
      case lso => throw new NoResponseSentToISException(lso.toJSON)
    }
    .map(x => new ProducerRecord[Array[Byte], Array[Byte]](LSSConfigLoader.kafkaConfigtoISTopic,
    x.messageid.getBytes(), x.toJsonArray))
    .to(restartableProducerSink)
    .withAttributes(ActorAttributes.supervisionStrategy(sinkDecider))

  //Sink Kafka
  lazy val kafkaSink = Producer.plainSink(kafkaConnection.producerSettings)

  lazy val restartableProducerSink = RestartSink.withBackoff(
    minBackoff = 1.seconds,
    maxBackoff = 5.seconds,
    randomFactor = 0.2
  ) { () =>
    kafkaSink
  }.withAttributes(ActorAttributes.supervisionStrategy(sinkDecider))

  lazy val getScheduleToBeSendForNode = Flow[EssentialNodeHierarchyFields].mapAsync(parallelism) { x =>
    val scheduleId: String = x.scheduleid.getOrElse(throw
      new UnableToGetScheduleDataException(x.toJSON))
    val scheduleList = LSSGetNodeHierarchyFromNeo4jTimer.time(
      Try(dbLayer.getScheduleToSendFromDbById(scheduleId)).toEither match {
      case Left(l) => throw l
      case Right(r) => r.recover {
        case ex: Exception => throw new UnableToGetScheduleDataException("to be sent to node " + x.toJSON); throw ex
      }
    })
    scheduleList.flatMap { e => e.headOption match {
      case Some(lightingScheduleEvent: LightingSchedule) =>
        Future.successful(LightingSchedule.constructLightScheduleFromDbResp(x,
          e.headOption.getOrElse(throw new UnableToGetScheduleDataException(scheduleId))))
      case None => throw new NoNodesFoundToSendScheduleException("for node hierarchy " + x.toJSON)
    }
    }.map { e => log.debug("response for getScheduleToBeSentToNode " + e); e }
  }.withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  lazy val getSendScheduleToNodeForMidNight = Flow[NodeSiteListByTimeZone]
    .map(x => x.createEssentialNodeOrgHierarchy(x))
    .map { e => log.debug("Node Hierarchies " + e); e }

  /*
  * Flow to resolve Node Data from Graph Db for Org hierarchy for Node
  * */
  lazy val getDataFromNodeFlow = Flow[RestPack]
    .mapAsync(parallelism)(ResolvedNodeData.resolveNodeFromGraphDb).recover {
    case ex: Exception => throw new UnableToGetOrgHierarchyFoundException
  }
    .map {
      case a: NodeOrgHierarchyData if (a.scheduleid.isEmpty && a.siteid.isEmpty) ||
        (a.siteid.contains(Constants.UNKNOWN) && a.scheduleid.contains(Constants.DEFAULT))
        || (a.scheduleid.isEmpty || a.scheduleid == null)
      => NodeOrgHierarchyData.applyDefaultConfig(a)
      case data => data
    }
    .map(_.createEssentialNodeOrgHierarchy)
    .map{ x => log.debug("after resolving node hierarchy " + x.toJSON); x }
    .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  lazy val wrapLightScheduleToLightStatusObjFlow = Flow[ScheduleCASEL]
    .map(ScheduleCASEL.extractLightStatusObj).map { x => log.debug("light status before lfs " + x); x }

  /**
    * Flow to Validate the schedule tokens received from the systeminfo messages of NGCN nodes
    * and send the schedule again if a token mismatch is seen
  * */
  lazy val nodeTokenValidatorFlow = Flow[NodeUpdate].mapAsync(parallelism) { e =>
    lazy val constructLoginRequest = NodeUpdate.constructLoginRequest(e)
    NodeUpdate.extractTokensFromPayload(e) match {
      case a@(Some(nodeId), Some(tokenVector)) if nodeId.nonEmpty && tokenVector.nonEmpty =>
        val lastUpdatedTokenFromNode = getScheduleTokenFromTokenArray(tokenVector)
        log.debug("last sent token received from system info " + lastUpdatedTokenFromNode)
        lastUpdatedTokenFromNode match {
          case Some(scheduleToken) =>
            LSSGetTokenLatencyTimer.time(dbLayer.getLatestLightMode(nodeId).recover {
              case ex: Exception => throw new UnableToUpdateLightStatus(e.toJSON)
            }.map {
              case Some(Light(_nodeid, _, _, _, _, _, Some(scheduleIdInDb), Some(scheduleTimeInDb), _)) =>
                if (convertToScheduleKeyValuePair(scheduleIdInDb, scheduleTimeInDb).contains(scheduleToken)) {
                  log.debug(s"Token matched.. skip applying schedule with scheduleId: $scheduleToken for request " + e.toJSON)
                  None
                } else {
                  log.warn(s"Token mismatch found!!.. reapplying schedule to node ${e.sid} " +
                    s"schedule_token in db $scheduleIdInDb schedule_token from system_info $scheduleToken " + e.toJSON)
                  Some(constructLoginRequest)
                }
              case _ =>
                log.warn(s"Light record not found in DB with schedule_token.. reapplying schedule to node " + e.toJSON)
                Some(constructLoginRequest)
            })
          case None =>
            log.warn(s"Node update missing token values resending schedule to node " + e.toJSON)
            Future.successful(Some(constructLoginRequest))
        }
      case b@(Some(nodeid), Some(tokenList)) if nodeid.nonEmpty && tokenList.isEmpty =>
        Future.successful(Some(constructLoginRequest))
      case _ => throw new InvalidTokenFromNodeException(e.toJSON)
    }
  }.filter(_.isDefined) // Send schedules to the nodes as the token mismatch found
    .map(_.get)
    .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))


  lazy val validateScheduleCaselRequest: (LightingSchedule) => (LightingSchedule) = {
    case lightSch@LightingSchedule(events: Vector[ScheduleEvent], network: Option[Network], nodes, lat: String,
    lon: String, scheduleId) if scheduleId.isDefined && nodes.isDefined && nodes.get.nonEmpty => lightSch

    case lightSch@LightingSchedule(events: Vector[ScheduleEvent], network: Option[Network], nodes, lat: String,
    lon: String, scheduleId) if scheduleId.isEmpty => throw new UnableToGetScheduleFromCasel(lightSch.toJSON)

    case lightSch@LightingSchedule(events: Vector[ScheduleEvent], network: Option[Network], nodes, lat: String,
    lon: String, scheduleId) if nodes.isDefined || nodes.get.isEmpty => throw new UnableToGetScheduleFromCasel(
      lightSch.toJSON)

    case lightSch => throw new UnableToGetScheduleFromCasel(lightSch.toJSON)
  }

  lazy val validateScheduleCASELFlow = Flow[ScheduleCASEL].map { x =>
    ScheduleCASEL.extractScheduleProps(x)
  }.map {
    case Some(lightSchedule) => lightSchedule
    case _ => throw new UnableToGetScheduleFromCasel()
  }.map(validateScheduleCaselRequest)
    .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  /**
    * Flow helps to update timestamp `Schedule` after applying the schedule to a node
  * */
  lazy val updateNeo4jLastSent = Flow[LightStatusObj]
    .mapAsync(parallelism) {
      case lso@LightStatusObj(Some(Constants.LIGHTING_SCHEDULE_EVENT_STR), _, _, _, _, _, _,
      Some(LightControlRequestHeaders(_, _, _, schToken, _, _))) =>
        val scheduleIdTimeTuple = extractScheduleIdTime(schToken) match {
          case (Some(schId), Some(schTime)) => (schId, schTime)
          case (Some(schId), _) => throw new NoScheduleFoundGraphDb(lso.toJSON)
          {
            override val underlyingMessage = "No ScheduleId found to update graphdb "
          }
          case (_, Some(schTime)) => throw new MalformedScheduleDataException(lso.toJSON) {
              override val underlyingMessage = "No ScheduleTime defined in the token, unable to update last_delivered on graphDb "
            }
          case (_, _) => throw new MalformedScheduleDataException(lso.toJSON) {
              override val underlyingMessage = "Schedule Token malformed, unable to update last_delivered on graphDb "
            }
        }
        dbLayer.updateLastSentScheduleTimeInGraphDb(scheduleIdTimeTuple._2,
          DateTimeUtil.convertMicrosToISO(scheduleIdTimeTuple._1 * 1000))
      case lightStatusObj => Future.successful(lightStatusObj)
    }.to(Sink.ignore)

  lazy val parseReceivedTimeStamp: (ScheduleLoopTrigger) => (DateTime) = {
    case schTrigger@(UnAppliedScheduleLoopTrigger(timeStamp)) => Try(DateTime.parse(timeStamp)).toEither match {
      case Left(l) => throw new UnableToParseTimeStampException(schTrigger.toJSON, l)
      case Right(r) => r
    }
    case schTrigger => throw new UndefinedTriggerTimeStampException(schTrigger.toJSON)
  }

  lazy val getTimeZonesOfMidnightByTimeStampFlow = Flow[ScheduleLoopTrigger]
    .map(parseReceivedTimeStamp).map{ e => log.debug("received timestamp " + e); e}
    // 899998 = 14 min 59 sec 998 millis to avoid the duplicate schedule triggering at YYYY-MM-DD'T'HH:15:00.000Z
    .map(x => resolveTimeStamp(x, Duration.millis(899998)))
    .map{ e => log.debug("midnight timezones " + e); e}
    .async
    .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  lazy val getSchedulesForMidnightTimezoneFlow = Flow[Vector[String]]
    .mapAsync(parallelism)(x => Try(dbLayer.getScheduleToBeSendByTimeZone(x, supportedNodeModels)).toEither match {
      case Left(l) => throw l
      case Right(r) => r.recover {
        case ex: EmptyResultSetException => throw new EmptyResultSetException(x.toString, ex)
        case ex: Exception               => throw new UnableToGetDataFromGraph(x.toString, ex)
      }
    }).map {e => log.debug("resolved sites timezones that have midnight " + e); e}
    .mapConcat(identity)
    .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  val assignScheduleGraph = RunnableGraph.fromGraph(g = GraphDSL.create() { implicit b =>
    import GraphDSL.Implicits._

    //Source
    val LoginRequestSource = b.add(kafkaRestartableConsumerSource(LSSConfigLoader.kafkaConfigtoLSSTopic)).out

    val DDScheduleSource = b.add(kafkaRestartableConsumerSource(LSSConfigLoader.kafkaDDToScheduleService)).out

    val ScheduleLoopSource = b.add(kafkaRestartableConsumerSource(LSSConfigLoader.kafkaScheduleLoopTriggerTopic)).out

    val LFSSource = b.add(kafkaRestartableConsumerSource(LSSConfigLoader.kafkaLightForceStateTopic)).out

    val NodeUpdateSource = b.add(kafkaRestartableConsumerSource(LSSConfigLoader.kafkaNodeUpdateTopic)).out

    // Flows
    val LFSUnMarshallFlow = b.add(eventUnmarshallFlow[ScheduleCASEL].map{x => LFSMsgUnmarshalltimer.time(x)})

    val NodeUpdateUnMarshallFlow = b.add(eventUnmarshallFlow[NodeUpdate].map{x => SchNodeUpdateTimer.time(x)})

    val NodeTokenValidator = b.add(nodeTokenValidatorFlow)

    val LightStatusObjWrapperFlow = b.add(wrapLightScheduleToLightStatusObjFlow)

    val ScheduleLoopUnMarshallFlow = b.add(eventUnmarshallFlow[ScheduleLoopTrigger].map{x => SchLoopTimer.time(x)})

    val SchLoopGetMidnightTZFlow = b.add(getTimeZonesOfMidnightByTimeStampFlow)

    val GetSchForMidnightTZ = b.add(getSchedulesForMidnightTimezoneFlow)

    val GetSchToBeSentForMidnightTZ = b.add(getSendScheduleToNodeForMidNight)

    val LoginReqUnMarshallFlow = b.add(eventUnmarshallFlow[RestPack].map{x => SchLoginReqTimer.time(x)})

    val ScheduleUnMarshallFlow = b.add(eventUnmarshallFlow[ScheduleCASEL].map{x => DDSchUpdateMsgUnmarshalltimer.time(x)})

    val ValidateScheduleCASELFlow = b.add(validateScheduleCASELFlow)

    val GetDataFromNodeFlow = b.add(getDataFromNodeFlow)

    val GetScheduleToBeSendForNode = b.add(getScheduleToBeSendForNode)

    val ScheduleToSTSWrapperFlow = b.add(wrapLightScheduleToSTS)

    val LightControlKafkaHandler = b.add(lightControlCmdHandler)

    val LightControlISErrorKafkaHandler = b.add(lightControlISErrorKafkaHandler)

    // Sink

    val LogLastSentSchedule = b.add(updateNeo4jLastSent).in

    val LightControlISKafkaSink = b.add(lightControlISKafkaSink).in

    val LightControlCmdProducerFlow = b.add(lightControlCmdKafkaSink)

    val LightModeSubGraph = b.add(LightStateHelper(dbLayer, kafkaConnection).lightModeSubGraph)

    val LightControlMerge = b.add(Merge[LightStatusObj](2))

    // Broadcasts & Merges
    val BroadCastLightState = b.add(Broadcast[LightStatusObj](2))

    val MergeLightingSchedule = b.add(Merge[LightingSchedule](2))

    val MergeLightStatusISResponse = b.add(Merge[LightStatusObj](2))

    val MergeLoginReq = b.add(Merge[RestPack](2))

    val MergeNodeOrgHierarchy = b.add(Merge[EssentialNodeHierarchyFields](2))

    val LightControlMsgPartition = b.add(Partition[Message[Array[Byte], Array[Byte], LightStatusObj]](2, kafkaControlMsgPartitioner))


    DDScheduleSource ~> ScheduleUnMarshallFlow ~> ValidateScheduleCASELFlow ~> MergeLightingSchedule

    LoginRequestSource ~> LoginReqUnMarshallFlow ~> MergeLoginReq ~> GetDataFromNodeFlow ~> MergeNodeOrgHierarchy ~>
      GetScheduleToBeSendForNode ~> MergeLightingSchedule ~> ScheduleToSTSWrapperFlow ~> BroadCastLightState
    BroadCastLightState ~> LightControlMerge ~> LightModeSubGraph ~> LightControlKafkaHandler ~> LightControlMsgPartition.in
    BroadCastLightState ~> LogLastSentSchedule

    LightControlMsgPartition.out(0) ~> LightControlCmdProducerFlow ~> MergeLightStatusISResponse ~> LightControlISKafkaSink
    LightControlMsgPartition.out(1) ~> LightControlISErrorKafkaHandler ~> MergeLightStatusISResponse

    ScheduleLoopSource ~> ScheduleLoopUnMarshallFlow ~> SchLoopGetMidnightTZFlow ~> GetSchForMidnightTZ ~>
      GetSchToBeSentForMidnightTZ ~> MergeNodeOrgHierarchy

    LFSSource ~> LFSUnMarshallFlow ~> LightStatusObjWrapperFlow ~> LightControlMerge

    NodeUpdateSource ~> NodeUpdateUnMarshallFlow ~> NodeTokenValidator ~> MergeLoginReq

    ClosedShape
  })

}
