package com.verizon.netsense.services.alert.flow

import java.time.Instant
import java.util.UUID

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.scaladsl.{Flow, Sink}
import akka.stream.{ActorAttributes, ActorMaterializer, ActorMaterializerSettings}
import com.fasterxml.jackson.core.JsonParseException
import com.verizon.netsense.services.alert.customexceptions._
import com.verizon.netsense.services.alert.database.PhantomConnector
import com.verizon.netsense.services.alert.helper.KafkaConfig.{alarmTopic, alertTopic, coreAlarmTopic}
import com.verizon.netsense.services.alert.helper.{AlarmValidator, KafkaSettings, SupervisionDecider}
import com.verizon.netsense.services.alert.model._
import com.verizon.netsense.services.alert.util.ObjectMapperUtil._
import com.verizon.netsense.utils.Logging
import nl.grons.metrics.scala.Timer
import org.apache.kafka.clients.consumer.ConsumerRecord
import org.apache.kafka.clients.producer.ProducerRecord

import scala.concurrent.{ExecutionContextExecutor, Future}

trait IngestionFlow extends PhantomConnector with KafkaSettings with SupervisionDecider with Logging {

  implicit val alertIngestionServiceActorSystem = ActorSystem("Alert-Ingestion-Service-system")
  implicit val ec: ExecutionContextExecutor     = alertIngestionServiceActorSystem.dispatcher
  implicit val mat = ActorMaterializer(
    ActorMaterializerSettings(alertIngestionServiceActorSystem)
      .withInputBuffer(INITIAL_BUFFER_SIZE, MAX_BUFFER_SIZE)
      .withSupervisionStrategy(systemDecider)
  )

  private[this] val kafkaSinkTimer: Timer      = metrics.timer("kafkaSink-timer")
  private[this] val unPackingFlowTimer: Timer  = metrics.timer("msgUnpacking-timer")
  private[this] val msgPackToRestPackFlowTimer = metrics.timer("MsgPackToRestPack-timer")

  lazy val videoNodeKafkaSource = kafkaRestartableConsumerSource(Set(alarmTopic))
  lazy val coreNodeKafkaSource  = kafkaRestartableConsumerSource(Set(coreAlarmTopic))

  val messagePackTransformFlow: Flow[ConsumerRecord[Array[Byte], Array[Byte]], AlarmEvent, NotUsed] =
    Flow[ConsumerRecord[Array[Byte], Array[Byte]]]
      .mapAsync(PARALLELISM) { event =>
        msgPackToRestPackFlowTimer.time {
          fromJsonAsync[CoreAlarmEvent](event.value())
            .recover {
              case ex: JsonParseException =>
                throw new JsonParsingException("Unable to parse the core alarm element: "
                                               + event.value + " with error " + ex.getMessage,
                                               ex.getCause)
            }
        }
      }
      .mapAsync(PARALLELISM) { alarm =>
        Future.successful(AlarmEvent(alarm))
      }
      .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  val restPackTransformFlow: Flow[ConsumerRecord[Array[Byte], Array[Byte]], AlarmEvent, NotUsed] =
    Flow[ConsumerRecord[Array[Byte], Array[Byte]]]
      .mapAsync(PARALLELISM) { event =>
        unPackingFlowTimer.time {
          fromJsonAsync[AlarmEvent](event.value()).recover {
            case ex: JsonParseException =>
              throw new JsonParsingException("Unable to parse the alarm element: "
                                             + event.value + " with error " + ex.getMessage,
                                             ex.getCause)
          }
        }
      }
      .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  lazy val validateAlarmFlow: Flow[AlarmEvent, AlarmEvent, NotUsed] = Flow[AlarmEvent]
    .filter(AlarmValidator.validationPredicate)
    .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  lazy val checkExistingAlertInDbFlow: Flow[Alert, Alert, NotUsed] =
    Flow[Alert]
      .mapAsync(PARALLELISM) { alert =>
        checkExistingAlert(alert.nodeId, alert.`type`, alert.orgId, alert.siteId).map {
          case Some(mapping) =>
            if (alert.severity.getOrElse("").equalsIgnoreCase(AlarmSeverity.Clear.toString))
              alert.copy(alertId = mapping.alertId, created = mapping.created, active = false)
            else
              alert.copy(alertId = mapping.alertId, created = mapping.created)
          case None => alert
        }
      }
      .withAttributes(ActorAttributes.supervisionStrategy(dbDecider))

  lazy val lookupOrgFlowForNewAlert: Flow[AlarmEvent, Alert, NotUsed] =
    Flow[AlarmEvent]
      .mapAsync(PARALLELISM) {
        alarmEventToAlert(_).recover {
          case ex: Exception => throw ex
        }
      }
      .withAttributes(ActorAttributes.supervisionStrategy(dbDecider))

  private def alarmEventToAlert(alarmEvent: AlarmEvent): Future[Alert] =
    getOrgHierarchy(alarmEvent.sid)
      .map {
        case Some(org) =>
          val checkOrgPredicate = org.orgId.contains("Unknown") || org.siteId.contains("Unknown") ||
          org.orgId.contains("") || org.siteId.contains("") || org.orgId.isEmpty || org.siteId.isEmpty ||
          org.orgId == null || org.siteId == null
          if (checkOrgPredicate) {
            throw new OrgDataNotFoundException("Org Id and SiteId are Invalid for node: " + alarmEvent.sid)
          }
          Alert(alarmEvent, org)
        case None =>
          throw new OrgDataNotFoundException(
            "No org Data found in orghierarchy_by_nodeid table for Node: " + alarmEvent.sid
          )
      }

  private def getUFName(alert: Alert): Future[AlertResponse] = {
    val existingModels = Set("unode", "falcon-q", "merlin", "vdkmaster", "cnext")
    val nodeHw = alert.nodeHw match {
      case Some(model) =>
        if (model.contains("unode"))
          Set("unode")
        else Set(model)
      case None => Set.empty[String]
    }

    nodeHw.foreach { model =>
      if (!existingModels.contains(model))
        log.error("Invalid Node Model: " + nodeHw)
    }

    val defaultUfAlarm = UfAlarm(
      UUID.randomUUID.toString,
      alert.`type`,
      nodeHw,
      Some(alert.`type`),
      alert.msg,
      displayToCustomer = false,
      displayToPartner = false,
      Some(Instant.now.getEpochSecond),
      Some(Instant.now.getEpochSecond)
    )
    getOrElseCreateUfAlarm(defaultUfAlarm).map(AlertResponse(alert, _))
  }

  lazy val filterClearAlerts: Alert => Boolean = { alert =>
    alert.severity.getOrElse("").equalsIgnoreCase(AlarmSeverity.Clear.toString) && alert.active
  }

  lazy val persistFlow: Sink[Alert, NotUsed] = Flow[Alert]
    .filterNot(filterClearAlerts)
    .mapAsync(PARALLELISM) { alert =>
      persistAlert(alert)
    }
    .to(Sink.ignore)
    .withAttributes(ActorAttributes.supervisionStrategy(dbDecider))

  lazy val UfAlarmFlow: Flow[Alert, AlertResponse, NotUsed] = Flow[Alert]
    .mapAsync(PARALLELISM) { alert =>
      getUFName(alert).recover {
        case ex: Exception => throw ex
      }
    }
    .withAttributes(ActorAttributes.supervisionStrategy(dbDecider))

  lazy val kafkaFlow = Flow[AlertResponse]
    .mapAsync(PARALLELISM) { alertResponse =>
      log.debug("Sending enriched alert to kafka alert topic: " + alertResponse.toJSON)
      kafkaSinkTimer.time {
        Future(new ProducerRecord[Array[Byte], Array[Byte]](alertTopic, alertResponse.toJsonArray))
      }
    }
    .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  lazy val persistToAppendOnlyTableSink: Sink[AlarmEvent, NotUsed] = Flow[AlarmEvent]
    .mapAsync(PARALLELISM) { alarm =>
      persistAlarm(ApAlarm(alarm))
    }
    .to(Sink.ignore)
    .withAttributes(ActorAttributes.supervisionStrategy(dbDecider))

  lazy val kafkaSink = configKafkaProducer(_producerSettings = setupProducerSettings)

}
