package com.verizon.netsense.sse.service

/**
 * Created by sundach on 5/23/17.
 */
import java.net.InetAddress
import java.util.UUID

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.alpakka.mqtt.scaladsl.MqttSink
import akka.stream.alpakka.mqtt.{MqttMessage, MqttQoS}
import akka.stream.scaladsl.{Flow, RestartSink, RestartSource}
import akka.stream.{ActorAttributes, ActorMaterializer, Supervision}
import akka.util.ByteString
import com.datastax.driver.core.Session
import com.fasterxml.jackson.core.JsonParseException
import com.verizon.netsense.connector.{ConnectionRetry, KafkaSettings, MqttConnector}
import com.verizon.netsense.helper.SensorSampleMsgPackConverter
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.model._
import com.verizon.netsense.sse.config.SSEConfigLoader._
import com.verizon.netsense.sse.db.CassandraQuery
import com.verizon.netsense.sse.exceptions.CustomExceptions.{CassandraDBException, EventTypeNotInSSEScopeException, OrgDataNotFoundException}
import com.verizon.netsense.sse.model._
import com.verizon.netsense.sse.util.{EventWrapper, ObjectMapperUtil}
import com.verizon.netsense.utils._
import nl.grons.metrics.scala.Timer
import org.apache.kafka.clients.consumer.ConsumerRecord

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

/**
 * Created by sundach on 5/23/17.
 */
trait BaseSSEService extends Instrumented with Logging with CassandraQuery with KafkaSettings {

  private[this] val loginReqMetrics: Timer = metrics.timer("sse-kafka-loginReq-event")

  private[this] val sensorMetrics: Timer = metrics.timer("sse-kafka-sensorsample-event")

  private[this] val alertMetrics: Timer = metrics.timer("sse-kafka-devicealarm-event")

  private[this] val bussAlertMetrics: Timer = metrics.timer("sse-kafka-businessalert-event")

  private[this] val conStatusMetrics: Timer = metrics.timer("sse-kafka-connectionstatus-event")

  private[this] val gpsMetrics: Timer = metrics.timer("sse-kafka-gps-event")

  private[this] val enrichMetrics: Timer = metrics.timer("sse-enrich-event-process")

  private[this] val filterMetrics: Timer = metrics.timer("sse-filter-event-process")

  private[this] val mqttMessageMetrics: Timer = metrics.timer("sse-mqtt-message-process")

  val kafkaSourceTimer: Timer = metrics.timer("sse-kafka-source")

  val mqttSinkTimer: Timer = metrics.timer("sse-mqtt-sink")

  implicit val ec = scala.concurrent.ExecutionContext.Implicits.global

  object EventTypeEnum extends Enumeration {

    val LOGINEVENT            = EventType(s"login$envSuffix")
    val SENSOREVENT           = EventType(s"sensor$envSuffix")
    val CORENODESENSOREVENT   = EventType(s"corenode.sensor$envSuffix")
    val ALERTEVENT            = EventType(s"alert$envSuffix")
    val CONNECTIONSTATUSEVENT = EventType(s"connectionstatus$envSuffix")
    val BUSINESSALERTEVENT    = EventType(s"businessalert$envSuffix")
    val GPSEVENT              = EventType(s"gps$envSuffix")

    def EventType(name: String): Value with Matching = new Val(nextId, name) with Matching

    def unapply(s: String): Option[Value] = values.find(s == _.toString)

    trait Matching {
      def unapply(s: String): Boolean = s == toString
    }

  }

  /*
   * Flow Decider
   */

  lazy val flowDecider: Supervision.Decider = {
    case ex: EventTypeNotInSSEScopeException => log.error(ex.getMessage); Supervision.Resume
    case ex: JsonParseException =>
      log.error(ex.getMessage)
      ex.printStackTrace()
      Supervision.Resume
    case ex: CassandraDBException =>
      log.error(ex.getMessage)
      ex.printStackTrace()
      Supervision.Resume
    case ex: OrgDataNotFoundException => log.warn(ex.getMessage); Supervision.Resume
    case ex: NullPointerException     => log.warn("Null element in stream " + ex.getMessage); Supervision.Resume
    case ex: Exception =>
      log.warn(ex.getMessage)
      ex.printStackTrace()
      Supervision.Resume
  }

  def mergeSources(s: Set[String])(implicit system: ActorSystem) = {
    log.debug(s"Consume from TOPIC: $s")
    consumeTopics(s).map(kafkaSourceTimer.time(_))
  }

  /*
   * Restartable Kafka for FlowService
   */
  var kafkaEnrichSourceRetryCount: Long  = 1
  def incKafkaSourceRetryCounter(): Unit = kafkaEnrichSourceRetryCount = kafkaEnrichSourceRetryCount + 1

  def restartableEnrichSource(topicNames: Set[String])(implicit system: ActorSystem) =
    RestartSource
      .withBackoff(
        minBackoff = 1.seconds,
        maxBackoff = 10.seconds,
        randomFactor = 0.2
      ) { () =>
        if (kafkaEnrichSourceRetryCount == 1) {
          incKafkaSourceRetryCounter()
          log.info("Starting Consumer for SSEFlowService for " + topicNames.seq)
        } else {
          incKafkaSourceRetryCounter()
          log.error("Restarting SSEFlowService KafkaConsumer on failure " + kafkaEnrichSourceRetryCount)
        }
        mergeSources(topicNames)
      }
      .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  /*
   * Restartable Kafka for Subscription
   */
  var kafkaSubscriptionSourceRetryCount: Long  = 1
  def incKafkaSubscriptionSourceRetryCounter(): Unit = kafkaSubscriptionSourceRetryCount = kafkaSubscriptionSourceRetryCount + 1

  def restartableSubscriptionSource(topicName: String)(implicit system: ActorSystem) =
    RestartSource
      .withBackoff(
        minBackoff = 1.seconds,
        maxBackoff = 10.seconds,
        randomFactor = 0.2
      ) { () =>
        if (kafkaSubscriptionSourceRetryCount == 1) {
          incKafkaSubscriptionSourceRetryCounter()
          log.info("Starting Consumer for SSESubscribeService for " + topicName)
        } else {
          incKafkaSubscriptionSourceRetryCounter()
          log.error("Restarting SSESubscriptionService KafkaConsumer on failure " + kafkaSubscriptionSourceRetryCount)
        }
        consumeTopic1(topicName)
      }
      .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  /*
   * Restartable Mqtt
   */

  val hostName                        = InetAddress.getLocalHost
  val mqttClientId: String            = ""
  private val uniqueMqttClientId      = mqttClientId + UUID.randomUUID().toString + "-" + hostName
  var mqttSinkRetryCount: Int        = 0
  def incMqttSinkRetryCounter(): Unit = mqttSinkRetryCount = mqttSinkRetryCount + 1

  def mqttSink(implicit system: ActorSystem) =
    RestartSink.withBackoff(
      minBackoff = 1.seconds,
      maxBackoff = 5.seconds,
      randomFactor = 0.2
    ) { () =>
      val mqttSinkSettings = MqttConnector.mqttSettings(useInternalMqtt = true)
      if (mqttSinkRetryCount == 1) {
        incMqttSinkRetryCounter()
        log.info("Starting sink for SSE " + mqttSinkSettings.broker + " clientid: " + mqttSinkSettings.clientId)
      } else {
        log.error(
          "Reconnecting MQTT publisher on failure count: " + mqttSinkRetryCount + " " + mqttSinkSettings.broker + " clientid: "
            + mqttSinkSettings.clientId
        )
        incMqttSinkRetryCounter()
        ConnectionRetry.mqttMaxRetryCheck(mqttSinkRetryCount)
      }
      mqttSinkTimer.time {
        MqttSink(mqttSinkSettings
          .withClientId(uniqueMqttClientId + "-sink"),
          MqttQoS.AtLeastOnce)
      }
    }

  /*
   * Merge all the kafka topics
   */
  def kafkaToEvent()(implicit parallel: Int,
                     ec: ExecutionContext,
                     mat: ActorMaterializer): Flow[ConsumerRecord[Array[Byte], String], SSEEvent, NotUsed] =
    Flow[ConsumerRecord[Array[Byte], String]]
      .mapAsync(parallel)(msg => {
        log.debug("Raw event : " + msg.value())
        log.debug("MsgTopic : " + msg.topic())
        Future {
          msg.topic() match {
            case EventTypeEnum.LOGINEVENT() =>
              loginReqMetrics.time {
                loginReq(msg)
              }
            case EventTypeEnum.SENSOREVENT() =>
              sensorMetrics.time {
                sensorSampleEvent(msg)
              }
            case EventTypeEnum.CORENODESENSOREVENT() =>
              sensorMetrics.time {
                coreNodesensorSampleEvent(msg)
              }
            case EventTypeEnum.ALERTEVENT() =>
              alertMetrics.time {
                alertEvent(msg)
              }
            case EventTypeEnum.CONNECTIONSTATUSEVENT() =>
              conStatusMetrics.time {
                conStatus(msg)
              }
            case EventTypeEnum.BUSINESSALERTEVENT() =>
              bussAlertMetrics.time {
                bussAlertEvent(msg)
              }
            case EventTypeEnum.GPSEVENT() =>
              gpsMetrics.time {
                gpsEvent(msg)
              }
            case _ => throw new EventTypeNotInSSEScopeException(msg.value() + " " + msg.topic())

          }
        }
      })
      .mapConcat(identity)
      .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  def loginReq(msg: ConsumerRecord[Array[Byte], String]) = {
    val loginReqEvent = ObjectMapperUtil.fromJson[SSELoginEvent](msg.value)
    List(EventWrapper.buildLoginResponse(loginReqEvent))
  }

  def sensorSampleEvent(msg: ConsumerRecord[Array[Byte], String]) = {
    val sensorEvent = ObjectMapperUtil.fromJson[SensorSampleEvent](msg.value)
    SensorValueConverter
      .convertSensorValues(sensorEvent)
      .map(s => SSESensorSampleEvent(s))
      .map(e => EventWrapper.buildSensorResponse(e))
  }

  def coreNodesensorSampleEvent(msg: ConsumerRecord[Array[Byte], String]) = {
    val corenodesensorEvent = ObjectMapperUtil.fromJson[CoreNodeRawSensorSample](msg.value)
    SensorValueConverter
      .convertSensorValues(SensorSampleMsgPackConverter.msgPackToRestPackConverter(corenodesensorEvent))
      .map(SSESensorSampleEvent.apply)
      .map(e => EventWrapper.buildSensorResponse(e))
  }

  def alertEvent(msg: ConsumerRecord[Array[Byte], String]) = {
    val alertEvent = ObjectMapperUtil.fromJson[SSEAlarmEvent](msg.value)
    List(EventWrapper.buildAlertResponse(alertEvent))
  }

  def bussAlertEvent(msg: ConsumerRecord[Array[Byte], String]) = {
    val bussAlertEvent = ObjectMapperUtil.fromJson[SSEBusinessAlertEvent](msg.value)
    List(EventWrapper.buildBusinessAlertResponse(bussAlertEvent))
  }

  def conStatus(msg: ConsumerRecord[Array[Byte], String]) = {
    val connectionStatusEvent = ObjectMapperUtil.fromJson[SSEConnectionEvent](msg.value)
    List(EventWrapper.buildConnectionResponse(connectionStatusEvent))
  }

  def gpsEvent(msg: ConsumerRecord[Array[Byte], String]) = {
    val gpsEvent = ObjectMapperUtil.fromJson[SSEGpsEvent](msg.value)
    List(EventWrapper.buildGpsResponse(gpsEvent))
  }

  /*
   * Enrich the message with orgid and siteid based on nodeid
   */
  def enrich()(implicit parallel: Int,
               session: Session,
               ec: ExecutionContext,
               mat: ActorMaterializer): Flow[SSEEvent, SSEEvent, NotUsed] =
    Flow[SSEEvent]
      .mapAsync(parallel)(
        s =>
          enrichMetrics.time {
            log.debug("Data to be Enriched: " + s)
            s.asInstanceOf[DeviceEvent].name match {
              case "DeviceAlarm" | "GpsSample" => Future { s }
              case _                           => enrichEvent(s)
            }
        }
      )
      .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  /*
   * Filter the message based on wildcard(+) to nodeid and eventType
   */
  def filter()(implicit parallel: Int,
               session: Session,
               ec: ExecutionContext,
               mat: ActorMaterializer): Flow[SSEEvent, List[Result], NotUsed] =
    Flow[SSEEvent]
      .mapAsync(parallel)(
        x =>
          filterMetrics.time {
            x.asInstanceOf[DeviceEvent].name match {
              case "DeviceAlarm" | "GpsSample" => Future(buildAlertTopic(x))
              case _                           => filterEvent(x).map(_.flatten)
            }
        }
      )
      .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  def buildAlertTopic(event: SSEEvent)(implicit ec: ExecutionContext): List[Result] =
    List(Result(kafkaStreamingTopicPrefix + event.asInstanceOf[DeviceEvent].topicName, event))

  def businessAlertfilter()(
      implicit parallel: Int,
      session: Session,
      ec: ExecutionContext,
      mat: ActorMaterializer
  ): Flow[SSEEvent, List[Result], NotUsed] =
    Flow[SSEEvent]
      .mapAsync(parallel)(
        x =>
          filterMetrics.time {
            Future(buildBusinessAlertTopic(x))
        }
      )
      .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

  def buildBusinessAlertTopic(event: SSEEvent)(implicit ec: ExecutionContext): List[Result] =
    List(Result(kafkaStreamingTopicPrefix + event.asInstanceOf[BusinessAlertEvent].businessAlerttopicName, event))

  /*
   * Publish the message to mqtt on wildcard(+) to nodeid and eventType
   */
  def prepareMqttMessage()(implicit parallel: Int,
                           session: Session,
                           ec: ExecutionContext,
                           mat: ActorMaterializer): Flow[List[Result], MqttMessage, NotUsed] =
    Flow[List[Result]]
      .mapConcat(identity)
      .mapAsync(parallel)(
        x =>
          Future {
            mqttMessageMetrics.time {
              log.debug("Message published to Mqtt: " + x)
              MqttMessage(x.topicName.replaceAll("\\+", "ALL"), ByteString(ObjectMapperUtil.toJson(x.sseEvent)))
            }
        }
      )
      .withAttributes(ActorAttributes.supervisionStrategy(flowDecider))

}
