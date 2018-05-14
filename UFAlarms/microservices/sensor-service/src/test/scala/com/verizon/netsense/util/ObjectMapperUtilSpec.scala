package com.verizon.netsense.util

import java.util.UUID

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Keep, Sink, Source}
import akka.stream.testkit.scaladsl.TestSink
import akka.testkit.TestKit
import com.fasterxml.jackson.annotation.JsonProperty
import com.verizon.netsense.config.TestSuiteConfig
import com.verizon.netsense.data.TestData
import com.verizon.netsense.helper.CommonKafkaConsumer
import com.verizon.netsense.model._
import com.verizon.netsense.service.{SensorHistoryQueryService, SensorSampleIngestService}
import com.verizon.netsense.utils.ObjectMapperUtil
import org.apache.kafka.clients.consumer.ConsumerRecord
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, FlatSpecLike, MustMatchers}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext}

/**
 * Created by nalamte on 5/31/17.
 */
class ObjectMapperUtilSpec extends TestKit(ActorSystem("QueryServiceIntegrationSpec-System"))
with FlatSpecLike
with MustMatchers
with BeforeAndAfterAll
with BeforeAndAfterEach
with TestSuiteConfig with TestData {

  implicit val ec = ExecutionContext.Implicits.global
  implicit val mat                  = ActorMaterializer()(system)
  /**
   * Test the Object mapper Util
   */
  "ObjectMapper" should "serde on the SensorQueryEnvelope" in {

    case class SensorQueryEnvelope1(messageid: String, @JsonProperty request: IsQueryEnvelope)

    val nodeProps = NodeProps(nodeId)
    val siteProps = SiteProps(siteId)
    val orgProps  = OrgProps(orgId)

    case class ExtProps1(sensorid: String, date1: String, date2: String, limit: Int, period: String)
    val extProps = ExtProps1(sensorId, timestamp, timestamp, limit = limit, aggregationtype)

    case class IsQueryEnvelope(requestid: String,
                               `type`: String,
                               model: String,
                               action: String,
                               instanceid: String,
                               timestamp: String,
                               user: String,
                               @JsonProperty nodeprops: NodeProps,
                               @JsonProperty siteprops: SiteProps,
                               @JsonProperty orgprops: OrgProps,
                               @JsonProperty extprops: ExtProps1)

    val isQueryEnvelope = IsQueryEnvelope(requestId,
                                          instanceId,
                                          timestamp,
                                          `type`,
                                          model,
                                          action,
                                          userId,
                                          nodeProps,
                                          siteProps,
                                          orgProps,
                                          extprops = extProps)

    val expectedQueryPayload = SensorQueryEnvelope1(messageid = messageId, isQueryEnvelope)
    val objToJson            = ObjectMapperUtil.toJson(expectedQueryPayload)
    val strToObj             = ObjectMapperUtil.fromJsonByteArrayAsync[SensorQueryEnvelope](objToJson.getBytes())
    val result               = Await.result(strToObj, 5.second)

    result.get.request.extprops.date2.isDefined mustBe true
    result.get.request.extprops.date2.get mustBe timestamp

  }
  it should "fail in serde on the SensorQueryEnvelope for bad date2" in {

    case class SensorQueryEnvelope1(messageid: String, @JsonProperty request: IsQueryEnvelope)

    val nodeProps = NodeProps(nodeId)
    val siteProps = SiteProps(siteId)
    val orgProps  = OrgProps(orgId)

    case class ExtProps1(sensorid: String, date1: String, date2: String, limit: Int, period: String)
    val extProps = ExtProps1(sensorId, timestamp, null, limit = limit, aggregationtype)

    case class IsQueryEnvelope(requestid: String,
                               `type`: String,
                               model: String,
                               action: String,
                               instanceid: String,
                               timestamp: String,
                               user: String,
                               @JsonProperty nodeprops: NodeProps,
                               @JsonProperty siteprops: SiteProps,
                               @JsonProperty orgprops: OrgProps,
                               @JsonProperty extprops: ExtProps1)

    val isQueryEnvelope = IsQueryEnvelope(requestId,
                                          instanceId,
                                          timestamp,
                                          `type`,
                                          model,
                                          action,
                                          userId,
                                          nodeProps,
                                          siteProps,
                                          orgProps,
                                          extprops = extProps)

    val expectedQueryPayload = SensorQueryEnvelope1(messageid = messageId, isQueryEnvelope)
    val objToJson            = ObjectMapperUtil.toJson(expectedQueryPayload)
    val strToObj             = ObjectMapperUtil.fromJsonByteArrayAsync[SensorQueryEnvelope](objToJson.getBytes())
    val result               = Await.result(strToObj, 5.second)

    result.get.request.extprops.date2.isDefined mustBe false
    result.get.request.extprops.date2 mustBe None

  }

  it should "handle exceptions" in {

    val flowUnderTest = SensorHistoryQueryService.msgUnmarshallingFlow

    val mockConsumerRecord = new ConsumerRecord[String, String]("requesttopic", 0, 0, "key", "dldhdlf")
    val graph = Source.single(mockConsumerRecord)
      .via(flowUnderTest).runWith(Sink.headOption)

    val result = Await.result(graph, remainingOrDefault)
    result mustBe None
  }

  "Unmarshaller flow from Ingest" should "handle Json parsing exception by logging event" in {
    lazy val mockconsumerrecord =
      new ConsumerRecord[Array[Byte], Array[Byte]](responsetopicInRequest, 1,1,"mockKey".getBytes(), "mockValue".getBytes())
    val probe = Source.single(mockconsumerrecord)
      .via(CommonKafkaConsumer.restPackUnmarshallingFlow)
      .toMat(TestSink.probe)(Keep.right)
      .run()
    probe.request(1)
  }

  it should "Check nodeId in the video node sensor payload" in {


    val flowUnderTest = CommonKafkaConsumer.restPackUnmarshallingFlow
    lazy val sensorSamplePayload: SensorPayload = SensorPayload(n = null, u = sensorUnits, s = sensorId, v = sensorValue, t = starttimeInMicros)

    lazy val sensorSampleEvent: SensorSampleEvent = SensorSampleEvent(UUID.randomUUID().toString, nodeId, unsolEventType, sensorId, eventPath, sensorSamplePayload)

    val mockConsumerRecord = new ConsumerRecord[Array[Byte], Array[Byte]]("RequestTopic",
      0,
      1521669398L,
      sensorSampleEvent.uuid.getBytes,
      sensorSampleEvent.toJsonArray)
    val graph = Source.single(mockConsumerRecord)
      .via(flowUnderTest).runWith(Sink.headOption)

    val result = Await.result(graph, remainingOrDefault)
    result mustBe defined
    result.get.l.n mustBe nodeId
  }

  it should "Check nodeId in the core node sensor payload" in {


    val flowUnderTest = CommonKafkaConsumer.msgPackUnmarshallingFlow
    val unpackedFlowUnderTest = CommonKafkaConsumer.msgPackUnpackingFlow
    lazy val sensorSamplePayload: SensorPayload = SensorPayload(n = null, u = sensorUnits, s = sensorId, v = sensorValue, t = starttimeInMicros)

    lazy val sensorSampleEvent = generateCoreNodeSensorSample(nodeId = coreNodeId, value = 100)

    val mockConsumerRecord = new ConsumerRecord[Array[Byte], Array[Byte]]("RequestTopic",
      0,
      1521669398L,
      null,
      sensorSampleEvent.toJsonArray)
    val graph = Source.single(mockConsumerRecord)
      .via(flowUnderTest).via(unpackedFlowUnderTest).runWith(Sink.headOption)

    val result = Await.result(graph, remainingOrDefault)
    result mustBe defined
    result.get.l.n mustBe coreNodeId
  }

}
