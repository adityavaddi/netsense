package com.verizon.netsense.sse.specs.service

import java.util.concurrent.TimeUnit

import akka.stream.alpakka.mqtt.MqttMessage
import akka.stream.scaladsl.Keep
import akka.stream.testkit.TestSubscriber
import akka.stream.testkit.scaladsl.TestSink
import akka.stream.{ActorMaterializer, ActorMaterializerSettings, Supervision}
import akka.util.ByteString
import com.outworkers.phantom.connectors.CassandraConnection
import com.outworkers.phantom.dsl._
import com.verizon.netsense.sse.service.BaseSSEService
import com.verizon.netsense.sse.specs.data.{SSEGenData, TestData}
import com.verizon.netsense.sse.specs.db.CassandraSpec
import com.verizon.netsense.sse.specs.phantom.PhantomServiceSpec
import com.verizon.netsense.test.kafka.BaseKafkaTest
import com.verizon.netsense.test.kafka.Utils._
import com.verizon.netsense.utils.Logging
import org.apache.kafka.clients.producer.ProducerRecord
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.control.NonFatal

class SSEFlowServiceSpec
    extends BaseKafkaTest
    with Inspectors
    with ScalaFutures
    with OptionValues
    with PhantomServiceSpec
    with BeforeAndAfterAll
    with Logging
    with SSEGenData
    with TestData
    with CassandraSpec {

  implicit val stageStoppingTimeout = StageStoppingTimeout(15.seconds)

  val decider: Supervision.Decider = {
    case NonFatal(ex) =>
      log.error("Got non fatal exception in SSEActor flow", ex)
      Supervision.Resume
    case ex =>
      log.error("Got fatal exception in SSEActor flow, stream will be stopped", ex)
      Supervision.Stop
  }

  implicit val conn: CassandraConnection = startDB()
  implicit val ec                        = system.dispatcher
  implicit val session: Session          = startDB().session

  override def beforeAll() = {
    createSchemaAndTable(session)
    insertIntoDevices(session)
    insertIntoFilter(session)
    super.beforeAll()
  }

  override def afterAll() = {
    stopDB()
    super.afterAll()
  }

  implicit val mate: ActorMaterializer = ActorMaterializer(
    ActorMaterializerSettings(system).withSupervisionStrategy(decider)
  )

  trait ServiceBase extends BaseSSEService {
    def graphiteHost: String = "test"
    def graphitePort: Int    = 1
    implicit val parallel    = 2
    val i                    = 1

    override def kafkaBootstrapServers: String = ???
    override def kafkaClientId: String         = ???
    override def kafkaGroupId: String          = ???
    override def kafkaOffsetReset: String      = ???
  }

  def givenInitializedTopic(topic: String, event: Array[Byte]): Unit = {
    val producer = producerSettings.createKafkaProducer()
    producer.send(new ProducerRecord(topic, partition0, null: Array[Byte], event))
    producer.close(60, TimeUnit.SECONDS)
  }

  "Reactive kafka streams" must {
    "produce to plainSink and consume from plainSource" in new ServiceBase {
      assertAllStagesStopped {
        val topic1 = createTopic2
        val group1 = createGroup

        Await.result(produce(topic1, 1 to 1, loginReqBytes), remainingOrDefault)
        loginEventObj.orgid = null
        loginEventObj.siteid = null

        val source = createConsumer(group1, topic1)
        val probe = source
          .via(kafkaToEvent)
          .toMat(TestSink.probe)(Keep.right)
          .run()
        val response = probe
        probe
          .request(1)
          .requestNext(100.second)

        probe.cancel()
      }
    }
  }

  "Consume from one kafka topic with all SSE flows" must {
    "should return topicname and LoginReq json" in new ServiceBase {
      assertAllStagesStopped {
        val sourceTopic = createTopic1
        val group1      = createGroup
        givenInitializedTopic(sourceTopic, loginReqBytes)

        Await.result(produce(sourceTopic, 1 to 2, loginReqBytes), remainingOrDefault)

        val source = createConsumer(group1, sourceTopic)

        val probe1 = source
          .via(kafkaToEvent)
          .via(enrich)
          .via(filter)
          .via(prepareMqttMessage)
          .toMat(TestSink.probe)(Keep.right)
          .run()

        probe1.request(1)
        val consumedMsg = probe1.requestNext(30.second)
        val topic       = consumedMsg.topic
        val payload     = consumedMsg.payload
        topic should be(s"/streamv1/$orgId/$siteId/$nodeId/LoginReq")
        payload.decodeString("US-ASCII") should be(s"$loginReqStr")
        probe1.cancel()
      }
    }
  }

  def checkAll(payload1: ByteString, loginReqStr: String, sensorSampleStr: String, corenodeSensorRawStr: String) =
    assert(
      payload1.decodeString("US-ASCII") === s"$loginReqStr" || payload1
        .decodeString("US-ASCII") === s"$sensorSampleStr" || payload1
        .decodeString("US-ASCII") === s"$corenodeSensorRawStr"
    )

  def checkAllEvent(payload1: ByteString, GpsSampleStr: String, devicealarmStr: String, connectionStatusStr: String) =
    assert(
      payload1.decodeString("US-ASCII") === s"$GpsSampleStr" || payload1
        .decodeString("US-ASCII") === s"$devicealarmStr" || payload1
        .decodeString("US-ASCII") === s"$connectionStatusStr"
    )

  def assertSSEResult(probe1: TestSubscriber.Probe[MqttMessage],
                      loginReqStr: String,
                      sensorSampleStr: String,
                      corenodeSensorRawStr: String) = {
    val consumedMsg1 = probe1.requestNext(20.second)
    val topic1       = consumedMsg1.topic
    val payload1     = consumedMsg1.payload
    println(s"topic1:$topic1")
    println(s"payload1:${payload1.decodeString("US-ASCII")}")
    topic1 match {
      case s: String if s === s"/streamv1/$orgId/$siteId/+/LoginReq" =>
        assert(payload1.decodeString("US-ASCII") === s"$loginReqStr")
      case s: String
          if s === s"/streamv1/$orgId/$siteId/$nodeId/+" || s === s"/streamv1/$orgId/$siteId/+/SensorSample" || s === s"/streamv1/$orgId/$siteId/+/+" =>
        checkAll(payload1, loginReqStr, sensorSampleStr, corenodeSensorRawStr)
      case s: String if s === s"/streamv1/$orgId/$siteId/$nodeId/LoginReq" =>
        assert(payload1.decodeString("US-ASCII") === s"$loginReqStr")
      case s: String if s === s"$orgId/$siteId/$nodeId/SensorSample" =>
        assert(payload1.decodeString("US-ASCII") === s"$sensorSampleStr")
      case _ => assert(true)
    }
  }

  def assertSSEEventResult(probe1: TestSubscriber.Probe[MqttMessage],
                           gpsSampleStr: String,
                           devicealarmStr: String,
                           connectionStatusStr: String) = {
    val consumedMsg1 = probe1.requestNext(10.second)
    val topic1       = consumedMsg1.topic
    val payload1     = consumedMsg1.payload
    println(s"topic1:$topic1")
    println(s"payload1:${payload1.decodeString("US-ASCII")}")
    topic1 match {
      case s: String if s === s"/streamv1/$orgId/$siteId/+/GpsSample" =>
        assert(payload1.decodeString("US-ASCII") === s"$gpsSampleStr")
      case s: String if s === s"/streamv1/$orgId/$siteId/+/DeviceAlarm" =>
        assert(payload1.decodeString("US-ASCII") === s"$devicealarmStr")
      case s: String if s === s"/streamv1/$orgId/$siteId/+/ConnectionStatus" =>
        assert(payload1.decodeString("US-ASCII") === s"$connectionStatusStr")
      case s: String if s === s"/streamv1/$orgId/$siteId/$nodeId/+" || s === s"/streamv1/$orgId/$siteId/+/+" =>
        checkAllEvent(payload1, gpsSampleStr, devicealarmStr, connectionStatusStr)
      case s: String if s === s"/streamv1/$orgId/$siteId/$nodeId/DeviceAlarm" =>
        assert(payload1.decodeString("US-ASCII") === s"$devicealarmStr")
      case s: String if s === s"/streamv1/$orgId/$siteId/$nodeId/GpsSample" =>
        assert(payload1.decodeString("US-ASCII") === s"$gpsSampleStr")
      case s: String if s === s"/streamv1/$orgId/$siteId/$nodeId/ConnectionStatus" =>
        assert(payload1.decodeString("US-ASCII") === s"$connectionStatusStr")
      case _ => assert(true)
    }
  }
  "Consume from multiple kafka topics with all flows" must {
    "should return topicname and LoginReq json" in new ServiceBase {
      assertAllStagesStopped {
        val t1     = createTopic1
        val t2     = createTopic2
        val t4     = createTopic4
        val group1 = createGroup
        givenInitializedTopic(t1, loginReqBytes)
        givenInitializedTopic(t2, sensorSampleBytes)
        givenInitializedTopic(t4, corenodeSensorRawBytes)

        Await.result(produce(t1, 1 to 1, loginReqBytes), remainingOrDefault)
        Await.result(produce(t2, 1 to 1, sensorSampleBytes), remainingOrDefault)
        Await.result(produce(t4, 1 to 1, corenodeSensorRawBytes), remainingOrDefault)

        val source1 = createConsumer(group1, t1)
        val source2 = createConsumer(group1, t2)
        val source4 = createConsumer(group1, t4)
        val probe1 = source1
          .merge(source2)
          .merge(source4)
          .via(kafkaToEvent)
          .via(enrich)
          .via(filter)
          .via(prepareMqttMessage)
          .toMat(TestSink.probe)(Keep.right)
          .run()

        probe1.request(7)
        (1 to 7).foreach(x => assertSSEResult(probe1, loginReqStr, sensorSampleStr, corenodeSensorRawStr))
        probe1.cancel()
      }
    }
  }

  "Consume from devicealarm and GpsSample kafka topics with all flows" must {
    "should return topicname and json data" in new ServiceBase {
      assertAllStagesStopped {
        val t3     = createTopic3
        val t5     = createTopic5
        val t7     = createTopic7
        val group1 = createGroup
        givenInitializedTopic(t3, devicealarmBytes)
        givenInitializedTopic(t5, gpsSampleBytes)
        givenInitializedTopic(t7, connectionStatusBytes)

        Await.result(produce(t3, 1 to 1, devicealarmBytes), remainingOrDefault)
        Await.result(produce(t5, 1 to 1, gpsSampleBytes), remainingOrDefault)
        Await.result(produce(t7, 1 to 1, connectionStatusBytes), remainingOrDefault)

        val source3 = createConsumer(group1, t3)
        val source5 = createConsumer(group1, t5)
        val source7 = createConsumer(group1, t7)
        val probe1 = source3
          .merge(source5)
          .merge(source7)
          .via(kafkaToEvent)
          .via(enrich)
          .via(filter)
          .via(prepareMqttMessage)
          .toMat(TestSink.probe)(Keep.right)
          .run()

        probe1.request(6)
        (1 to 6).foreach(x => assertSSEEventResult(probe1, gpsSampleStr, devicealarmStr, connectionStatusStr))
        probe1.cancel
      }
    }
  }
}
