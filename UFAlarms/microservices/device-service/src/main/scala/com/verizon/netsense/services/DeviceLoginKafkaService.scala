package com.verizon.netsense.services

import akka.Done
import akka.actor.ActorSystem
import akka.event.{Logging => ALogging}
import akka.kafka.scaladsl.{Consumer, Producer}
import akka.kafka.{ConsumerSettings, Subscriptions}
import akka.stream.scaladsl.{Broadcast, Flow, GraphDSL, Keep, RestartSource, RunnableGraph, Sink}
import akka.stream.{ActorAttributes, ActorMaterializer, ClosedShape, Supervision}
import com.datastax.driver.core.exceptions.{NoHostAvailableException, WriteTimeoutException}
import com.datastax.driver.core.utils.UUIDs
import com.fasterxml.jackson.databind.JsonMappingException
import com.verizon.netsense.connector.KafkaConnection
import com.verizon.netsense.database.{DeviceConnectionStatusDbService, NodeStatusDbService}
import com.verizon.netsense.entity._
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.utils.{ConfigLoader, Deserializer, TimeConverter}
import nl.grons.metrics.scala.Timer
import org.apache.kafka.clients.consumer.{ConsumerConfig, ConsumerRecord}
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.KafkaException
import org.apache.kafka.common.serialization.ByteArrayDeserializer
import org.joda.time.DateTime
import org.json4s.DefaultFormats
import org.slf4j.LoggerFactory

import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Failure, Success}

/**
  * Created by davor on 5/11/17.
  */
class DeviceLoginKafkaService(val connection: KafkaConnection)(implicit val system: ActorSystem) extends Instrumented
  with TimeConverter {

  lazy val toLSSTopic = ConfigLoader.config.getString("kafka.device-to-lss-topic")

  val log = LoggerFactory.getLogger(this.getClass)

  lazy val CNEXT_NODE_TYPE = "cnext"

  lazy val lwtTopicExcludeNodeStatusCheck = "UNSOL/lwt/"
   
  implicit val ec = scala.concurrent.ExecutionContext.Implicits.global

  implicit val materializer = ActorMaterializer()(system)

  implicit val formats = DefaultFormats

  val parallelism = 1000

  private[this] val kafkaSourceTimer: Timer           = metrics.timer("kafka-source-consumer")
  private[this] val kafkaSinkTimer: Timer             = metrics.timer("kafka-sink-producer")
  private[this] val kafkaLSSLoginReqSinkTimer: Timer  = metrics.timer("kafka-lss-loginreq-sink-producer")
  private[this] val unPackerFlowTimer: Timer          = metrics.timer("messagepack-unpacker-flow-timer")
  private[this] val msgUnPackerFailureTimer: Timer    = metrics.timer("messagepack-unpacker-unpacker-Exc")

  val flowDecider: Supervision.Decider = {
    msgUnPackerFailureTimer.time {
      case ex: JsonMappingException => log.error("Unable to unpack the element", ex); Supervision.Resume
      case ex: NullPointerException => log.error("Null element in stream " + ex.getMessage); Supervision.Restart
      case e: Throwable             => log.error("Exception", e); Supervision.Restart
      case _                        => log.error("Unhandled exception in stream"); Supervision.Resume
    }
  }

  val sinkDecider: Supervision.Decider = {
    case ex: WriteTimeoutException => log.error("Unable to write the event to cassandra", ex); Supervision.Resume
    case ex: NoHostAvailableException =>
      log.error("Unable to establish cassandra connection ", ex); Supervision.Restart
    case e: Throwable => log.error("Exception", e); Supervision.Restart
    case _            => log.error("Unhandled exception in stream"); Supervision.Resume
  }

  lazy val producerSinkDecider: Supervision.Decider = {
    case ex: NullPointerException => log.error("Null element in stream " + ex.getMessage); Supervision.Restart
    case ex: KafkaException =>
      log.error("KafkaException in Producer: " + ex.getClass.getName + ": " + ex.getMessage); Supervision.Restart
    case ex =>
      log.error("Unhandled exception in stream sink: " + ex.getClass.getName + ": " + ex.getMessage)
      ex.printStackTrace()
      Supervision.Restart
  }

  lazy val sourceDecider: Supervision.Decider = {
    case ex: NullPointerException => log.error("Null element in stream " + ex.getMessage); Supervision.Restart
    case ex: KafkaException =>
      log.error("KafkaException in Consumer: " + ex.getClass.getName + ": " + ex.getMessage); Supervision.Restart
    case ex =>
      log.error("Unhandled exception in stream source: " + ex.getClass.getName + ": " + ex.getMessage)
      ex.printStackTrace()
      Supervision.Restart
  }

  lazy val attributes =
    ActorAttributes
      .logLevels(onElement = ALogging.DebugLevel, onFinish = ALogging.DebugLevel, onFailure = ALogging.DebugLevel)
      .and(
        ActorAttributes
          .supervisionStrategy(sourceDecider)
      )


  val consumerSettings = ConsumerSettings(system = system,
    keyDeserializer = new ByteArrayDeserializer,
    valueDeserializer = new ByteArrayDeserializer)
    .withBootstrapServers(connection.bootStrapServers)
    .withGroupId(connection.groupId)
    .withProperty(ConsumerConfig.AUTO_OFFSET_RESET_CONFIG, "latest")
    .withProperty(ConsumerConfig.ENABLE_AUTO_COMMIT_CONFIG, "true")
    .withProperty(ConsumerConfig.AUTO_COMMIT_INTERVAL_MS_CONFIG, "5000")
    .withProperty(ConsumerConfig.MAX_POLL_INTERVAL_MS_CONFIG, "30000")
    .withProperty(ConsumerConfig.MAX_POLL_RECORDS_CONFIG, "2000")
    .withProperty(ConsumerConfig.SESSION_TIMEOUT_MS_CONFIG, "10000")
    .withProperty(ConsumerConfig.HEARTBEAT_INTERVAL_MS_CONFIG, "3000")
    .withProperty(ConsumerConfig.METADATA_MAX_AGE_CONFIG, "60000")
    .withProperty(ConsumerConfig.RECONNECT_BACKOFF_MS_CONFIG, "10000")
    .withProperty(ConsumerConfig.PARTITION_ASSIGNMENT_STRATEGY_CONFIG, "org.apache.kafka.clients.consumer.RoundRobinAssignor")
    .withProperty(ConsumerConfig.CONNECTIONS_MAX_IDLE_MS_CONFIG, "604800000")
    .withWakeupTimeout(30 seconds)
    .withMaxWakeups(60000)

  lazy val kafkaSource = Consumer
    .plainSource(consumerSettings, Subscriptions.topics(ConfigLoader.config.getString("kafka.from-device-topic")))
    .map { msg =>
      kafkaSourceTimer.time()
      msg
    }
    .withAttributes(ActorAttributes.supervisionStrategy(sourceDecider))

  var sourceRetryCount: Long = 0

  def incRetryCounter(): Unit = sourceRetryCount = sourceRetryCount + 1

  lazy val kafkaRestartableConsumerSource = RestartSource.withBackoff(
    minBackoff = 1.seconds,
    maxBackoff = 5.seconds,
    randomFactor = 0.2
  ) { () =>
    if (sourceRetryCount == 0) {
      incRetryCounter()
      log.info("Starting Kafka Consumer fromKafka: " + connection.bootStrapServers)
    }
    else {
      {
        incRetryCounter()
        log.error("Reconnecting kafka consumer on failure " + sourceRetryCount + " broker: " + connection.bootStrapServers)
      }
    }
    kafkaSource
  }.withAttributes(attributes)


  def kafkaLoginRespSink: Sink[(Option[RestPackRequestJSON], Array[Byte]), Future[Done]] =
    Flow[(Option[RestPackRequestJSON], Array[Byte])]
      .filter(_._1.get.p.contains("loginReq"))
      .mapAsync(parallelism) { x =>
        log.debug(s"Send LoginResp")
        kafkaSinkTimer.time()
        val loginReply = LoginReply(System.currentTimeMillis() * 1000)
        val payload = x._1 map { pre =>
          val response = RespPayloadMQTT(
            "POST",
            s"v1/${pre.sid}/in/POST/REPLY/${pre.uuid}/loginReq",
            pre.sid,
            org.joda.time.DateTime.now().toDateTimeISO.toString(),
            200,
            "",
            pre.uuid,
            LoginReply.toMsgPack(loginReply)
          )
          log.debug(s"Login reply payload: ${loginReply.toJSON}")
          log.debug(s"Sending login reply: ${response.toJSON}")
          log.debug(s"Sending reply to Kafka topic: ${ConfigLoader.config.getString("kafka.to-device-topic")}")
          response
        }
        Future(
          new ProducerRecord[Array[Byte], Array[Byte]](ConfigLoader.config.getString("kafka.to-device-topic"),
            payload.get.toMsgPack)
        )
      }
      .toMat(Producer.plainSink(connection.producerSettings))(Keep.right)
      .withAttributes(ActorAttributes.supervisionStrategy(producerSinkDecider))

  def kafkaLWTAlertSink: Sink[(Option[RestPackRequestJSON], Array[Byte]), Future[Done]] =
    Flow[(Option[RestPackRequestJSON], Array[Byte])]
      // Filters all LWT alerts for modules other than Gatekeeper (nmm,remoteio,vpn etc.)
      //Device service exclude these alerts while updating Connection Status/Node Status.
      .filter(_._1.get.p.contains(lwtTopicExcludeNodeStatusCheck) == false)
      .mapAsync(parallelism) { x =>
        kafkaSinkTimer.time()
        if (x._1.get.p.contains("UNSOL/lwt")) {
          log.debug(s"LWT Message: Raise alarm")

          val payload = x._1 map { pre =>
            //val loginLoad = LoginReq.fromJson(pre.l)
            RestPackRequestJSON(
              pre.a,
              pre.p,
              pre.sid,
              pre.d,
              pre.uuid,
              pre.f,
              pre.l
            )
          }
          Future( new ProducerRecord[Array[Byte], Array[Byte]](ConfigLoader.config.getString("kafka.lwt-alarm-topic"),
            payload.get.toJsonArray))
        }
        else {
          log.debug(s"LWT Message: Clear alarm")
          val alarmPayload = Map[String, Any]("s" -> 0,"a" -> "lwt","m"->"","c"->1)
          val payload = x._1 map { pre =>
            //val loginLoad = LoginReq.fromJson(pre.l)
            RestPackRequestJSON(
              "UNSOL",
              s"v1/${pre.sid}/out/UNSOL/lwt",
              pre.sid,
              pre.d,
              pre.uuid,
              "",
              alarmPayload
            )
          }
          Future(new ProducerRecord[Array[Byte], Array[Byte]](ConfigLoader.config.getString("kafka.lwt-alarm-topic"),
              payload.get.toJsonArray)
          )
        }
      }
      .toMat(Producer.plainSink(connection.producerSettings))(Keep.right)
      .withAttributes(ActorAttributes.supervisionStrategy(producerSinkDecider))


  lazy val filterSelectedNodeTypeEvents: (LSSLoginRequest) => (Boolean) = {
    case LSSLoginRequest(_, _, _, _, _, LSSLoginPayload(_, _, model)) if model != null && model.nonEmpty =>
      model.equalsIgnoreCase(CNEXT_NODE_TYPE)
    case _ => false
  }

  def kafkaLSSLoginSink: Sink[(Option[RestPackRequestJSON], Array[Byte]), Future[Done]] =
    Flow[(Option[RestPackRequestJSON], Array[Byte])]
    .filter(_._1.get.p.contains("loginReq"))
    .map { x =>
      kafkaLSSLoginReqSinkTimer.time()
      val loginLoad = LoginReq.fromJson(new String(Deserializer.baseMapper.writeValueAsBytes(x._1
        .getOrElse(throw new RuntimeException("Unable to get the payload of the loginreq")).l)))
      val loginPreamble = LoginReq.fromJsonPreamble(new String(Deserializer.baseMapper.writeValueAsBytes(x._1
        .getOrElse(throw new RuntimeException("Unable to get the envelope of the loginreq " + loginLoad.toJSON)))))
      val lSSLoginRequest: LSSLoginRequest = LSSLoginRequest.fromLoginPreamble(loginPreamble, loginLoad)
      lSSLoginRequest
    }.filter(filterSelectedNodeTypeEvents)
    .map { lSSLoginRequest =>
      log.debug("Sending LoginReq DS => LSS " + lSSLoginRequest.toJSON + " topic " + toLSSTopic)
      new ProducerRecord[Array[Byte], Array[Byte]](toLSSTopic, lSSLoginRequest.uuid.getBytes(),
        lSSLoginRequest.toJsonArray)}
    .toMat(Producer.plainSink(connection.producerSettings))(Keep.right)
    .withAttributes(ActorAttributes.supervisionStrategy(producerSinkDecider))

  def kafkaConfigReqSink: Sink[(Option[RestPackRequestJSON], Array[Byte]), Future[Done]] =
    Flow[(Option[RestPackRequestJSON], Array[Byte])]
      .filter(_._1.get.p.contains("loginReq"))
      .mapAsync(parallelism) { x =>
        log.debug(s"Send config response")
        kafkaSinkTimer.time()
        val loginLoad = LoginReq.fromJson(new String(Deserializer.baseMapper.writeValueAsBytes(x._1.get.l)))
        Future(new ProducerRecord[Array[Byte], Array[Byte]](ConfigLoader.config.getString("kafka.to-config-topic"),
          ConfigRequest.toJson(ConfigRequest(loginLoad.nid,
            loginLoad.tok,
            loginLoad.dev))))
      }
      .toMat(Producer.plainSink(connection.producerSettings))(Keep.right)
      .withAttributes(ActorAttributes.supervisionStrategy(producerSinkDecider))

  def consumerFlowForRequestCheck = Flow[(Option[RestPackRequestJSON], Array[Byte])]
    .filter(_._1.get.p.contains(lwtTopicExcludeNodeStatusCheck) == false)
    .map {
      e =>
        log.debug(s"Request Type check")
        val sid = e._1.get.sid
        val index = sid.indexOf("_client_ext")
        val nodeId = if(index > 0) sid.substring(0, index) else sid
        (nodeId, if (e._1.get.p.contains("loginReq")) true else false)
    }.withAttributes(ActorAttributes.supervisionStrategy(sinkDecider))

  val deviceConnectionStatusFlow = Flow[(String, Boolean)]
    .map {
      e =>
        DeviceConnectionStatusDbService.storeConnectionStatus(e._1, e._2, UUIDs.timeBased())
        (e._1, e._2)
    }
    .withAttributes(ActorAttributes.supervisionStrategy(sinkDecider))

  val nodeStatusSinkForDbCall = Flow[((String, Boolean))]
    .map{
      e => {
        if (e._2) {
          NodeStatusDbService.getNodeStatusByNodeId(e._1).onComplete {
            case Success(result) =>
              result match {
                case Some(_) =>
                  NodeStatusDbService.storeNodeStatus(e._1, result.get.orgId, result.get.siteId, if (e._2) 1 else 0, result.get.ligStat, result.get.senStat)
                    .onComplete {
                      case Success(s) => // Do Nothing
                      case Failure(ex) => log.error(s"Node Status Insertion for nodeId: ${e._1} failed with exception: " + ex)
                    }
                case None =>
                  NodeStatusDbService.storeNodeStatus(e._1, "Unknown", "Unknown", if (e._2) 1 else 0, "off", "0")
                    .onComplete {
                      case Success(s) => // Do Nothing
                      case Failure(ex) => log.error(s"Node Status Insertion for nodeId: ${e._1} failed with exception: " + ex)
                    }
              }
            case Failure(error) =>
              log.debug(s"ERROR: ${error.getMessage}")
              NodeStatusDbService.storeNodeStatus(e._1, "Unknown", "Unknown", if (e._2) 1 else 0, "off", "0")
                .onComplete {
                  case Success(s) => // Do Nothing
                  case Failure(ex) => log.error(s"Node Status Insertion for nodeId: ${e._1} failed with exception: " + ex)
                }
          }
        }
        else {
          NodeStatusDbService.getNodeStatusByNodeId(e._1).onComplete {
            case Success(result) =>
              result match {
                case Some(_) =>
                  NodeStatusDbService.storeNodeStatus(e._1, result.get.orgId, result.get.siteId, if (e._2) 1 else 0, result.get.ligStat, result.get.senStat)
                    .onComplete {
                      case Success(s) => // Do Nothing
                      case Failure(ex) => log.error(s"Node Status Insertion for nodeId: ${e._1} failed with exception: " + ex)
                    }
                case None => log.debug(s"Ignore unknown node LWT message")
              }
            case Failure(error) =>
              log.debug(s"ERROR: ${error.getMessage}")
              NodeStatusDbService.storeNodeStatus(e._1, "Unknown", "Unknown", if (e._2) 1 else 0, "off", "0")
                .onComplete {
                  case Success(s) => // Do Nothing
                  case Failure(ex) => log.error(s"Node Status Insertion for nodeId: ${e._1} failed with exception: " + ex)
                }
          }
        }
      }
    }
    .toMat(Sink.ignore)(Keep.right)
    .withAttributes(ActorAttributes.supervisionStrategy(sinkDecider))

  val nodeStatusSinkForDbCheck = Flow[(Option[RestPackRequestJSON], Array[Byte])]
    .filter(_._1.get.p.contains("loginReq"))
    .mapAsync(1){
      e => {
        log.debug(s"Check DB")
        NodeStatusDbService.getNodeStatusByNodeId(e._1.get.sid).map {
          case Some(_) =>
            log.debug(s"Check DB")
            (e._1, true)
          case None => (e._1, false)
        }
      }
    }
    .withAttributes(ActorAttributes.supervisionStrategy(sinkDecider))


  def kafkaDatadealerLoginReqSink: Sink[(Option[RestPackRequestJSON], Array[Byte]), Future[Done]] =
    Flow[(Option[RestPackRequestJSON], Array[Byte])]
      .filter(_._1.get.p.contains("loginReq"))
      .mapAsync(parallelism) { x =>
        log.debug(s"Send loginReq message")
        kafkaSinkTimer.time()
        val loginLoad = LoginReq.fromJson(new String(Deserializer.baseMapper.writeValueAsBytes(x._1.get.l)))

        val pl = CaselRequestNodePropsPayload(java.util.UUID.randomUUID().toString,
          "",
          (new DateTime()).toString,
          "loginReq",
          "",
          "CAN_CREATE_NODE",
          "",
          Some(Map("nodeid" ->loginLoad.nid,
                    "model" -> loginLoad.dev,
                    "bssid" -> loginLoad.bssid,
                    "softwareVersion" -> loginLoad.cid,
                    "remoteNetwork" -> loginLoad.ssid,
                    "mac" -> loginLoad.mac,
                    "ip" -> loginLoad.ip,
                    "modemRevEd" -> loginLoad.modemRevEd,
                    "channel" -> loginLoad.ch,
                    "auth" -> loginLoad.sec
                    )))

        val message = CaselRequestMessage(java.util.UUID.randomUUID().toString, "", pl)

        log.debug(s"Message: ${message.toJSON}")
        log.debug(s"Topic: ${ConfigLoader.config.getString("kafka.auto-commission-topic")}")

        Future(new ProducerRecord[Array[Byte], Array[Byte]](ConfigLoader.config.getString("kafka.auto-commission-topic"), message.toJsonArray))

      }
      .toMat(Producer.plainSink(connection.producerSettings))(Keep.right)
      .withAttributes(ActorAttributes.supervisionStrategy(producerSinkDecider))

  def kafkaDeviceStatusSink: Sink[(String, Boolean), Future[Done]] =
    Flow[(String, Boolean)]
      .mapAsync(parallelism) { x =>
        log.debug(s"Send Device Status message")
        kafkaSinkTimer.time()

        val message = RestPackRequestJSON("POST", // Action GET, POST, etc
          "device-service/POST/connectionStatus", // Topic
          "device-service",
          (new DateTime()).toString, // Date
          java.util.UUID.randomUUID().toString, // UUID
          "",
          Map("nodeid" -> x._1, "status" -> x._2) // LoginReq Payload
        )

        log.debug(s"Message: ${message.toJSON}")

        Future(new ProducerRecord[Array[Byte], Array[Byte]](ConfigLoader.config.getString("kafka.connection-status-topic"), message.toJsonArray))

      }
      .toMat(Producer.plainSink(connection.producerSettings))(Keep.right)
      .withAttributes(ActorAttributes.supervisionStrategy(producerSinkDecider))


  val messageUnpackTransformFlow = Flow[ConsumerRecord[Array[Byte], Array[Byte]]]
    .mapAsync(parallelism) { x =>
      log.debug(s"Message unpack")
      log.debug(s"Message: ${RestPackRequestJSON.fromJSON(x.value())}")
      log.debug(s"Path: ${RestPackRequestJSON.fromJSON(x.value()).p}")
      unPackerFlowTimer.time(); Future.successful((Some(RestPackRequestJSON.fromJSON(x.value())), x.value()))
    }
    .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  val runnableGraphCommit = RunnableGraph
    .fromGraph(g = GraphDSL.create() { implicit b =>
      import GraphDSL.Implicits._
      val Source = b.add(kafkaRestartableConsumerSource).out

      // Flows
      val broadcast         = b.add(Broadcast[(Option[RestPackRequestJSON], Array[Byte])](6))
      val messageUnpackFlow = b.add(messageUnpackTransformFlow)

      // Sinks
      val LoginRespKafkaSink    = b.add(kafkaLoginRespSink).in

      val ConfigKafkaSink = b.add(kafkaConfigReqSink).in

      val LSSKafkaSink = b.add(kafkaLSSLoginSink).in

      val LWTRespKafkaSink    = b.add(kafkaLWTAlertSink).in

      val DDSink = b.add(kafkaDatadealerLoginReqSink).in

      val broadcastRequestCheck         = b.add(Broadcast[(String, Boolean)](2))

      val deviceConnection = b.add(deviceConnectionStatusFlow)

      val nodeStatus = b.add(nodeStatusSinkForDbCall).in

      val DeviceStatusRequestCheck = b.add(consumerFlowForRequestCheck)
      val DeviceStatusSink = b.add(kafkaDeviceStatusSink).in

      Source ~> messageUnpackFlow ~> broadcast
      broadcast ~> DDSink
      broadcast ~> LoginRespKafkaSink
      broadcast ~> LWTRespKafkaSink
      broadcast ~> ConfigKafkaSink
      broadcast ~> LSSKafkaSink
      broadcast ~> DeviceStatusRequestCheck ~> broadcastRequestCheck
                                               broadcastRequestCheck ~> DeviceStatusSink
                                               broadcastRequestCheck ~> deviceConnection ~> nodeStatus
      ClosedShape
    }).run()

}
