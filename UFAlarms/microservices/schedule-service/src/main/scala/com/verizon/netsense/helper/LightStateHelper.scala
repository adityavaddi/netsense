package com.verizon.netsense.helper

import java.util.concurrent.TimeUnit

import akka.actor.{Actor, ActorSystem, DeadLetter, Props, Timers}
import akka.kafka.scaladsl.Producer
import akka.stream.scaladsl.{Flow, GraphDSL, Merge, Partition, RestartSink, Source}
import akka.stream.{ActorAttributes, FlowShape, Materializer, OverflowStrategy}
import com.verizon.netsense.config.LSSConfigLoader
import com.verizon.netsense.connector.KafkaConnection
import com.verizon.netsense.constants.Constants._
import com.verizon.netsense.exceptions.CustomExceptions.{NoNodesFoundToSendScheduleException, UnableToGetLightStatusFromDbException, UnableToUpdateLightStatus}
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.model._
import com.verizon.netsense.service.DbLayer
import com.verizon.netsense.util.Common
import com.verizon.netsense.utils.Logging
import nl.grons.metrics.scala.Timer
import org.apache.kafka.clients.producer.ProducerRecord
import org.joda.time.{DateTime, DateTimeZone}

import scala.concurrent.Future
import scala.concurrent.duration._

/**
  * Created by maidapr on 2/7/18.
  */
case class LightStateHelper(dbLayer: DbLayer, kafkaConnection: KafkaConnection)(implicit system: ActorSystem, mat: Materializer)
  extends TimeHelper with Logging with Instrumented with Common {

  implicit val ec = scala.concurrent.ExecutionContext.Implicits.global

  private[this] val LSSLightUpdateFlowTimer: Timer = metrics.timer("lss-cassandra-light-update-flow-timer")
  private[this] val LSSLightUpdateFailureFlowTimer: Timer = metrics.timer("lss-cassandra-light-update-failure-flow-timer")
  private[this] val LSSLightUpdateReadFlowTimer: Timer = metrics.timer("lss-cassandra-light-read-flow-timer")

  //Sink Kafka
  lazy val kafkaSink = Producer.plainSink(kafkaConnection.producerSettings)

  lazy val restartableProducerSink = RestartSink.withBackoff(
    minBackoff = 1.seconds,
    maxBackoff = 5.seconds,
    randomFactor = 0.2
  ) { () =>
    kafkaSink
  }.withAttributes(ActorAttributes.supervisionStrategy(sinkDecider))

  lazy val overrideResetActor =
    Source.actorRef[ScheduleCASEL](Int.MaxValue, OverflowStrategy.dropHead)
      .map { msg =>
        log.debug("Received LightingSetAuto request post timer " + msg.toJSON)
        new ProducerRecord[Array[Byte], Array[Byte]](LSSConfigLoader.kafkaLightForceStateTopic, msg.toJsonArray)
      }
      .to(restartableProducerSink).run() // Send a LightingSetAuto Command to Device

  lazy val LightSetAutoActor = system.actorOf(Props(new LightSetAutoTimer))

  class LightSetAutoTimer extends Actor with Timers {
    log.info("Starting LightSetAutoTimer")
    override def receive: Receive = {
      case msg: ScheduleCASEL => overrideResetActor ! msg
      case msg: LightStatusObj => LightStatusObj.unapplyLightAutoFields(msg) match {
        case Some((nodeId, timeout)) => log.debug("setting LightingtSetAuto timer " + msg)
          timers.startSingleTimer(nodeId, LightStatusObj.toLightControlSetAutoCASEL(msg),
            FiniteDuration(timeout, TimeUnit.SECONDS))
        case _ => log.error("No LightingSetAuto defined for Override reset " + msg)
      }
    }

    override def preStart = {
      system.eventStream.subscribe(this.self, classOf[LightSetAutoTimer])

    }
    override def postStop = {
      log.info("Stopping LightingSetAuto Timer")
      system.eventStream.unsubscribe(this.self)
    }
  }

  lazy val seggregateOverrideResetCmds: (LightStatusObj) => (Int) = {

    // LightingScheduleEvent
    case a@LightStatusObj(Some(LIGHTING_SCHEDULE_EVENT_STR), Some(nodeIds), _, true, _, _, _, Some(requestHeaders)) => 0
    // LightForceState
    case b@LightStatusObj(Some(LIGHTING_FORCE_STATE_STR), Some(nodeIds), Some(timeout), false, _, _, _, Some(requestHeaders)) => 1

    case lightStatusObj => throw new UnableToUpdateLightStatus(lightStatusObj.toJSON)
  }

  lazy val segreggateLightCommand: (LightStatusObj) => (Int) = {

    // LightingScheduleEvent
    case a@LightStatusObj(Some(LIGHTING_SCHEDULE_EVENT_STR), Some(nodeIds), _, true, _, _, _, Some(requestHeaders)) => 0
    // LightForceState
    case b@LightStatusObj(Some(LIGHTING_FORCE_STATE_STR), Some(nodeIds), Some(timeout), false, _, _, _, Some(requestHeaders)) => 1
    // LightingSetAuto
    case c@LightStatusObj(Some(LIGHTING_SET_AUTO_STR), Some(nodeIds), _, true, true, _, _, Some(requestHeaders)) => 2

    case lightStatusObj => 3
  }

  lazy val proliferateCommandsByNodes: (LightStatusObj) => (Vector[LightStatusObj]) = (ls) =>
    ls.nodeids match {
      case Some(nodeIds) => nodeIds.map(x => ls.copy(light = Some(ls.light.get.copy(nodeid = x))))
      case _ => throw new NoNodesFoundToSendScheduleException("while updating the light state " + ls.toJSON)
    }


  lazy val responseContainsError: (Vector[LightStatusObj]) => (Vector[LightStatusObj]) = (lsos) => {
    lsos.span(x => x.lightControlRequestHeaders.get.success)._2
  }

  lazy val lightSetAutoHandlerFlow = Flow[LightStatusObj].map {
    case lso@LightStatusObj(Some(LIGHTING_SET_AUTO_STR), Some(nodeids), _, _, _, _, _,
    _) if nodeids != null && nodeids.nonEmpty =>
      lso
    case lso => throw new UnableToUpdateLightStatus("as the nodes are empty for set auto " + lso.toJSON)
  }.withAttributes(ActorAttributes.supervisionStrategy(flowDecider))


  lazy val sendOverrideTimerSetMsgToActor: (LightStatusObj) => (Unit) = {
    case lso@LightStatusObj(_, _, _, _, _, _, _, Some(LightControlRequestHeaders(_, _, _, _, true, _))) =>
      LightSetAutoActor ! LightStatusObj.toLightControlSetAuto(lso)
    case lso => log.warn("Not setting the override as the force command weren't sent ")
  }

  lazy val lightStateForceFlow = Flow[LightStatusObj].map { x =>
    log.debug("Setting the override reset " + x)
    proliferateCommandsByNodes(x).foreach(sendOverrideTimerSetMsgToActor)
    x
  }.withAttributes(ActorAttributes.supervisionStrategy(flowDecider))


  lazy val updateLightStatusFlow = Flow[LightStatusObj].map(proliferateCommandsByNodes).mapAsync(parallelism) {
    x => Future.traverse(x) {
      case lso@LightStatusObj(_, Some(nodeids), _, _, _, _, Some(light), _)

        if light.nodeid != null && light.nodeid.nonEmpty =>
        log.debug("updating the light mode " + light.toJSON)
        dbLayer.storeLatestLightMode(light).map {
          case storeOp if storeOp.wasApplied() => LSSLightUpdateFlowTimer.time(); lso
          case _ => log.error("Failed to store the light mode in Cassandra")
            LSSLightUpdateFailureFlowTimer.time()
            lso.copy(lightControlRequestHeaders = Some(lso.lightControlRequestHeaders.get.copy(success = false)))
        }
      case lso =>
        log.error("Unable to update the database, Command missing required fields " + lso.toJSON)
        Future.successful(lso.copy(lightControlRequestHeaders =
        Some(lso.lightControlRequestHeaders.get.copy(success = false))))
    }.recover {
      case ex: Exception => Vector(x.head.copy(lightControlRequestHeaders =
        Some(x.head.lightControlRequestHeaders.get.copy(success = false,
          error = Some("Unable to update the light status " + ex)))))
    }
  }.map{

    // Skip replying error response to IS topic if it a Lighting schedule event
    case lsos: Vector[LightStatusObj] if lsos.nonEmpty && lsos.head.eventType.contains(LIGHTING_SCHEDULE_EVENT_STR) =>
      lsos.head

    // Reply to Interface service with appropriate light node failures
    case lsos: Vector[LightStatusObj] if lsos.nonEmpty && responseContainsError(lsos).nonEmpty
    =>
      log.debug("light(s) status not fully updated")
      lsos.head.copy(lightControlRequestHeaders =
      Some(lsos.head.lightControlRequestHeaders.get.copy(error = Some("Unable to update the state of the nodes " +
        responseContainsError(lsos).flatMap(x => x.nodeids).flatten.mkString(",")), success = false)))

    // Success reply for on completion of all light status writes without fail
    case lsos: Vector[LightStatusObj] if lsos.nonEmpty =>
      log.debug("light(s) status updated in db without fail " + lsos.head.nodeids)
      lsos.head
    case lsos => throw new NoNodesFoundToSendScheduleException(lsos.head.toJSON)
  }.withAttributes(ActorAttributes.supervisionStrategy(flowDecider))



  // For LightingScheduleEvent
  lazy val checkAndUpdateLightModeFlow = Flow[LightStatusObj].map(proliferateCommandsByNodes)
    .mapConcat(identity).mapAsync(parallelism) {
    case lso@LightStatusObj(_, Some(nodeids), _, _, _, _, Some(_light@Light(nodeId, _, _, _, _, _, _, _, _)), _) if nodeids.nonEmpty =>
      lazy val defaultLightMode = Light(nodeid = lso.nodeids.get.head, driver = None,
        harvest_trigger = false, isscheduled = true,
        policy = SCHEDULE_POLICY, priority = Some(SCHEDULE_PRIORITY), schedule_id = _light.schedule_id,
        _light.schedule_time, startdt = None)
      dbLayer.getLatestLightMode(nodeId).map {

        // Calc timeout for the override reset and clear expired overrides by sending LightingSetAuto
        case a@Some(light@(Light(_nodeId, _, _, false, _, _, _, _, Some(startDt)))) if calcTimeout(startDt) < -1 =>
          val immediateTimeoutValue = 1
          log.debug("Light has expired overrides, clearing overrides by sending LightingSetAuto" + light.toJSON)
          lso.copy(nodeids = Some(Vector(nodeId)), eventType = Some(LIGHTING_FORCE_STATE_STR),
            timeout = Some(immediateTimeoutValue), isScheduled = false, clearEnabled = true,
            light = Some(defaultLightMode))

        // Store Default Light Mode with isschedule set to true harvest_trigger set to false
        case None =>
          log.debug("Light not having existing override or schedule, storing isscheduled to true " + defaultLightMode.toJSON)
          lso.copy(nodeids = Some(Vector(nodeId)), eventType = Some(LIGHTING_SCHEDULE_EVENT_STR),
              isScheduled = true, harvestingEnabled = false, light = Some(defaultLightMode))

        case Some(lightStatusObj) =>
          log.debug("Light record found to be working in schedule setting isscheduled true " + lightStatusObj)
          lso.copy(nodeids = Some(Vector(nodeId)), eventType = Some(LIGHTING_SCHEDULE_EVENT_STR),
            light = Some(lightStatusObj.copy(schedule_id = _light.schedule_id, schedule_time = _light.schedule_time)),
          isScheduled = true, harvestingEnabled = false)
      }.recover {
        case ex: Exception => throw new UnableToGetLightStatusFromDbException(lso.toJSON)
      }

    case ls => throw new NoNodesFoundToSendScheduleException("while updating the light state " + ls.toJSON)
  }.map{x => LSSLightUpdateReadFlowTimer.time(); x}
    .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  /*
  *
  * LightingForceState message will directly overrides the current state of the light to the level with startdt
  * LightingSetAuto will set the isschedule 'true' policy 'scheduled' and harvest_trigger to 'false'
  * LightingScheduleEvent will check for the current light status and makes an entry with e isschedule 'true'
  * policy 'scheduled' and harvest_trigger to 'false' just as `LightingSetAuto`
  *
  * */
  lazy val lightModeSubGraph = Flow.fromGraph(g = GraphDSL.create() { implicit b =>
    import GraphDSL.Implicits._

    val LightForceCommandPartition = b.add(Partition[LightStatusObj](4, segreggateLightCommand))

    val OverrideResetPartition = b.add(Partition[LightStatusObj](2, seggregateOverrideResetCmds))

    val CheckAndUpdateLightModeFlow = b.add(checkAndUpdateLightModeFlow)

    val LightingForceStateFlow = b.add(lightStateForceFlow)

    val LightSetAutoFlow = b.add(lightSetAutoHandlerFlow)

    val UpdateScheduleTokenFlow = b.add(updateLightStatusFlow)

    val UpdateLFSLightStatusFlow = b.add(updateLightStatusFlow)

    val UpdateLightStatusFlow = b.add(updateLightStatusFlow)

    val MergeLightStatusFlow = b.add(Merge[LightStatusObj](2))

    val LightStatusObjOutMerge = b.add(Merge[LightStatusObj](3))

    val MergeLFS = b.add(Merge[LightStatusObj](2))

    LightForceCommandPartition.in

    // LightingScheduleEvent
    LightForceCommandPartition.out(0) ~> CheckAndUpdateLightModeFlow ~> OverrideResetPartition.in

    OverrideResetPartition.out(0) ~> UpdateScheduleTokenFlow ~> LightStatusObjOutMerge
    OverrideResetPartition.out(1) ~> MergeLFS

    // LightingForceState
    LightForceCommandPartition.out(1) ~> MergeLFS ~> LightingForceStateFlow ~> UpdateLFSLightStatusFlow ~>
      MergeLightStatusFlow ~> LightStatusObjOutMerge

    // LightingSetAuto
    LightForceCommandPartition.out(2) ~> LightSetAutoFlow ~> UpdateLightStatusFlow ~> MergeLightStatusFlow

    LightForceCommandPartition.out(3) ~> LightStatusObjOutMerge

    FlowShape(LightForceCommandPartition.in, LightStatusObjOutMerge.out)
  })


}