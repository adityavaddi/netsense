package com.verizon.netsense.services.alert.services.alertsimulator

import java.util.{Calendar, UUID}

import akka.Done
import akka.actor.ActorSystem
import akka.kafka.ProducerSettings
import akka.kafka.scaladsl.Producer
import akka.stream.scaladsl.{Sink, Source}
import akka.stream.{ActorMaterializer, ThrottleMode}
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.services.alert.customexceptions.CassandraDBException
import com.verizon.netsense.services.alert.model.{AlarmEvent, CoreAlarmEvent, DeviceAlarmPayload, OrgHierarchy}
import com.verizon.netsense.services.alert.services.database.CassandraDBTest
import com.verizon.netsense.utils.{ConfigLoader, Logging}
import nl.grons.metrics.scala.Timer
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.StringSerializer

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object Simulator extends App with Logging with Instrumented {

  lazy val throttleLevel = 10000 //Events per second produced to kafka
  val eventCount         = 100000 //Alarm generation count
  val nodePrefix         = "falcon-q_"
  val coreNodePrefix     = "unode-v4_"
  val severity           = 3
  val category           = 1
  val msg                = "Disk Volume Fail"
  val alarmType          = "SW/storage/volmissing/data_"
  val sensorType         = "SW/storage/volmissing/data"
  val solicitationType   = "UNSOL"
  val path               = "v1/falcon-q_1/out/UNSOL/alarm/{alarmid}"

  implicit val system = ActorSystem("Alert-Simulator-Actor-System")
  implicit val mat    = ActorMaterializer()
  implicit val ec     = system.dispatcher

  private[this] val producerTimer: Timer = metrics.timer("kafka-producer-timer")

  lazy val kafkaConfig = ConfigLoader.config.getConfig("kafka")
  val requestTopic     = kafkaConfig.getString("alarm-request-topic")
  val coreRequestTopic = kafkaConfig.getString("core-alarm-request-topic")

  val producerSettings = ProducerSettings(system, new StringSerializer, new StringSerializer)
    .withBootstrapServers(kafkaConfig.getString("host") + ":" + kafkaConfig.getInt("port"))

  def sink1: Sink[ProducerRecord[String, String], Future[Done]] =
    Producer.plainSink(producerSettings)

  def sink2: Sink[ProducerRecord[String, String], Future[Done]] =
    Producer.plainSink(producerSettings)

  lazy val loadOrgHieracrchy: Unit = {
    Await.ready(CassandraDBTest.OrgHierarchyByNode.truncateTable, 5.seconds)
    Await.ready(CassandraDBTest.DeviceAlerts.truncateTable, 5.seconds)
    Await.ready(CassandraDBTest.DeviceAlarms.truncateTable, 5.seconds)
    for (i <- 1 to eventCount) {
      Await
        .ready(
          CassandraDBTest.OrgHierarchyByNode.store(
            OrgHierarchy(nodePrefix + i.toString,
                         None,
                         Some(UUID.randomUUID().toString),
                         None,
                         Some(UUID.randomUUID().toString),
                         None)
          ),
          2.seconds
        )
        .recover {
          case ex: Exception =>
            log.error("Error loading org hierarchy table: " + ex.getMessage)
            throw new CassandraDBException(ex.getMessage)
        }
      Await
        .ready(
          CassandraDBTest.OrgHierarchyByNode.store(
            OrgHierarchy(coreNodePrefix + i.toString,
                         None,
                         Some(UUID.randomUUID().toString),
                         None,
                         Some(UUID.randomUUID().toString),
                         None)
          ),
          2.seconds
        )
        .recover {
          case ex: Exception =>
            log.error("Error loading org hierarchy table: " + ex.getMessage)
            throw new CassandraDBException(ex.getMessage)
        }
      log.debug("Loading " + i + "th record to Org Hierarchy table")
    }
  }

  def getAlarm(i: Long): AlarmEvent = {
    val alarmPayload = DeviceAlarmPayload(alarmType + i.toString, severity, msg, category)
    val alarmEvent = AlarmEvent(UUID.randomUUID().toString,
                                solicitationType,
                                sensorType,
                                path,
                                alarmPayload,
                                nodePrefix + i.toString,
                                Calendar.getInstance().toInstant.toString)
    log.debug("Alarm " + i + " sent with payload: " + alarmEvent.toJSON)
    alarmEvent
  }

  def getCoreAlarm(i: Long): CoreAlarmEvent = {
    val coreAlarm = CoreAlarmEvent("DeviceAlarm", coreNodePrefix + i.toString, "Comm_Fail" + i.toString, "Minor", msg)
    log.debug("Alarm " + i + " sent with payload: " + coreAlarm.toJSON)
    coreAlarm
  }

  lazy val eventSimulator = Source(1 to eventCount)
  //.fromIterator(() => Iterator from 1)
    .map(x => getAlarm(x))
    .map { e =>
      producerTimer.time(e)
      new ProducerRecord[String, String](requestTopic, UUID.randomUUID().toString, e.toJSON)
    }
    .throttle(throttleLevel, 1.second, throttleLevel, ThrottleMode.Shaping)
    .runWith(sink1)

  lazy val coreEventSimulator = Source(1 to eventCount)
  //.fromIterator(() => Iterator from 1)
    .map(x => getCoreAlarm(x))
    .map { e =>
      producerTimer.time(e)
      new ProducerRecord[String, String](coreRequestTopic, UUID.randomUUID().toString, e.toJSON)
    }
    .throttle(throttleLevel, 1.second, throttleLevel, ThrottleMode.Shaping)
    .runWith(sink2)

  log.info("Running Alert-Simulator...\nLoading org hierarchy data into Org Hierarchy table...")
  loadOrgHieracrchy
  log.info(
    eventCount + " records loaded into Org Hierarchy table.\n" +
    "Pumping " + eventCount + " Video node alarms to Kafka topic: " + requestTopic
  )
  Thread.sleep(2000)
  eventSimulator
  log.info(
    eventCount + " video node alarms triggered.\n" +
    "Pumping " + eventCount + " Core node alarms to Kafka topic: " + coreRequestTopic
  )
  Thread.sleep(2000)
  coreEventSimulator
  log.info(eventCount + " Core node alarms triggered")
}
