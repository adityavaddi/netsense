package com.verizon.netsense.sse.specs.service

import java.util.concurrent.TimeUnit

import akka.stream.scaladsl.Keep
import akka.stream.testkit.scaladsl.TestSink
import akka.stream.{ActorMaterializer, ActorMaterializerSettings, Supervision}
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

class SSEBusinessServiceSpec
    extends BaseKafkaTest
    with CassandraSpec
    with Inspectors
    with ScalaFutures
    with OptionValues
    with PhantomServiceSpec
    with BeforeAndAfterAll
    with Logging
    with SSEGenData
    with TestData {
  implicit val stageStoppingTimeout = StageStoppingTimeout(15.seconds)

  val decider: Supervision.Decider = {
    case NonFatal(ex) =>
      log.error("Got non fatal exception in SSEActor flow", ex)
      Supervision.Resume
    case ex =>
      log.error("Got fatal exception in SSEActor flow, stream will be stopped", ex)
      Supervision.Stop
  }

  implicit val mate: ActorMaterializer = ActorMaterializer(
    ActorMaterializerSettings(system).withSupervisionStrategy(decider)
  )

  implicit val conn: CassandraConnection = startDB()
  implicit val ec                        = system.dispatcher
  implicit val session: Session          = startDB().session

  override def beforeAll() = {
    createSchemaAndTable(session)
    insertIntoBusinessAlertFilter(session)
    super.beforeAll()
  }

  override def afterAll() = {
    deleteFilter(session)
    stopDB()
    super.afterAll()
    system.terminate()
  }

  trait ServiceBase extends BaseSSEService {
    def graphiteHost: String = "test"
    def graphitePort: Int    = 1
    implicit val parallel    = 2
    val i                    = 1

    override def kafkaBootstrapServers: String = ???

    override def kafkaClientId: String = ???

    override def kafkaGroupId: String = ???

    override def kafkaOffsetReset: String = ???

  }

  def givenInitializedTopic(topic: String, event: Array[Byte]): Unit = {
    val producer = producerSettings.createKafkaProducer()
    producer.send(new ProducerRecord(topic, partition0, null: Array[Byte], event))
    producer.close(60, TimeUnit.SECONDS)
  }

  "BusinessAlert Data" must {
    "getting Historical data for BusinessAlert" in {
      val future = databaseTest.sseBusinessAlertFilterEventtest.insertBussinessAlertSSEFilter(
        generateValidBusinessAlertSubscribeRequest(stype),
        businessTopicname,
        sseStreamTTL,
        count
      )
      whenReady(future) { result =>
        result isExhausted () shouldBe true
        result wasApplied () shouldBe true
        result one ()
      }
    }
  }

  "Reactive kafka streams" must {
    "produce to plainSink and consume from plainSource" in new ServiceBase {
      assertAllStagesStopped {
        val topic = createTopic6
        val group = createGroup

        Await.result(produce(topic, 1 to 1, businessAlertBytes), remainingOrDefault)

        businessAlertObj.orgid = null
        businessAlertObj.siteid = null

        val source = createConsumer(group, topic)
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

  "Consume from one kafka topic with businessAlert SSE flow" must {
    "should return topicname and BusinessAlert json" in new ServiceBase {
      assertAllStagesStopped {
        val sourceTopic = createTopic6
        val group1      = createGroup
        givenInitializedTopic(sourceTopic, businessAlertBytes)

        Await.result(produce(sourceTopic, 1 to 2, businessAlertBytes), remainingOrDefault)
        Await.ready(
          databaseTest.sseBusinessAlertFilterEventtest.insertBussinessAlertSSEFilter(
            generateValidBusinessAlertSubscribeRequest(stype),
            businessTopicname,
            sseStreamTTL,
            count
          ),
          remainingOrDefault
        )

        val source = createConsumer(group1, sourceTopic)

        val probe1 = source
          .via(kafkaToEvent)
          .via(businessAlertfilter)
          .via(prepareMqttMessage)
          .toMat(TestSink.probe)(Keep.right)
          .run()

        probe1.request(1)
        val consumedMsg = probe1.requestNext(30.second)
        val topic       = consumedMsg.topic
        val payload     = consumedMsg.payload
        println(s"BusinessAlert Mqtt topic:$topic")
        println(s"BusinessAlert payload:${payload.decodeString("US-ASCII")}")
        topic should be(s"/streamv1/$orgId/$siteId/businessalert/parking/$triggerId")
        payload.decodeString("US-ASCII") should be(s"$businessAlertStr")
        probe1.cancel()
      }
    }
  }
}
