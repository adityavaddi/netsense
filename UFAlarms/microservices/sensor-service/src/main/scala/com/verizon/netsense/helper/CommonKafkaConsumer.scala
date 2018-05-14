package com.verizon.netsense.helper

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream._
import akka.stream.scaladsl.{Flow, GraphDSL, Merge, RestartSource, Source}
import com.fasterxml.jackson.core.JsonParseException
import com.verizon.netsense.config.KafkaConfig._
import com.verizon.netsense.model.{CoreNodeRawSensorSample, SensorSampleEvent}
import com.verizon.netsense.utils.ObjectMapperUtil
import org.apache.kafka.clients.consumer.ConsumerRecord

/**
  * Created by maidapr on 1/4/18.
  */
object CommonKafkaConsumer extends Common {

  implicit val ec = scala.concurrent.ExecutionContext.Implicits.global

  lazy val msgPackUnmarshallingFlow = Flow[ConsumerRecord[Array[Byte], Array[Byte]]]
    .mapAsync(parallelism) { x =>
        ObjectMapperUtil.fromJsonAsync[CoreNodeRawSensorSample](x.value()).recover {
          case ex @ (_ : JsonParseException) =>
            log.error("Unable to parse the element: " + new String(x.value(), "UTF-8") + " " + ex); throw ex
        }
      }
    .withAttributes(ActorAttributes.supervisionStrategy(flowDeciderForQuery))

  lazy val msgPackUnpackingFlow = Flow[CoreNodeRawSensorSample]
    .filter(IngestServiceValidator.validateCoreNodeSensorSample)
    .map(SensorSampleMsgPackConverter.msgPackToRestPackConverter)
    .withAttributes(ActorAttributes.supervisionStrategy(flowDeciderForQuery))

  lazy val restPackUnmarshallingFlow = Flow[ConsumerRecord[Array[Byte], Array[Byte]]]
    .mapAsync(parallelism) { x =>
      ObjectMapperUtil.fromJsonAsync[SensorSampleEvent](x.value()).recover {
          case ex @ (_ : JsonParseException) =>
            log.error("Unable to parse the element: " + new String(x.value(), "UTF-8") + " " + ex); throw ex
        }
    }
    .map(x => x.copy(l = x.l.copy(n = x.sid)))
    .withAttributes(ActorAttributes.supervisionStrategy(flowDeciderForQuery))

  lazy val messageIngestionValidationflow = Flow[SensorSampleEvent]
    .filter(IngestServiceValidator.validationIngestionPredicate)
    .withAttributes(ActorAttributes.supervisionStrategy(flowDeciderForQuery))

  import scala.concurrent.duration._

  def kafkaRestartableConsumerSource(topicNames: Set[String], groupId: String)(implicit system: ActorSystem):
  Source[ConsumerRecord[Array[Byte], Array[Byte]], NotUsed] =
    RestartSource.withBackoff(
      minBackoff = 1.seconds,
      maxBackoff = 5.seconds,
      randomFactor = 0.2
    ) { () =>
      log.info("Starting Kafka Consumer with broker(s): " + bootStrapServers + " topic(s): " + topicNames
        + " groupId: " + groupId)
      configureKafkaConsumers(topicNames, groupId)
    }

  def SensorSampleSourceSubgraph()(implicit system: ActorSystem) =  Source.fromGraph(GraphDSL.create() {
    implicit b =>

      import GraphDSL.Implicits._

      val VideoNodeSource = b.add(kafkaRestartableConsumerSource(Set(videoNodeSensorSampleTopic), groupId))

      val CoreNodeSource = b.add(kafkaRestartableConsumerSource(Set(coreNodeSensorSampleTopic), groupId))

      val MsgPackConverter = b.add(msgPackUnpackingFlow)

      val RestPackUnMarshallFlow = b.add(restPackUnmarshallingFlow)

      val MsgPackUnMarshallFlow = b.add(msgPackUnmarshallingFlow)

      val merge = b.add(Merge[SensorSampleEvent](2))

      val MessageValidationFlow = b.add(messageIngestionValidationflow)

      VideoNodeSource ~> RestPackUnMarshallFlow ~> MessageValidationFlow  ~> merge
      CoreNodeSource  ~> MsgPackUnMarshallFlow  ~> MsgPackConverter       ~> merge

      SourceShape(merge.out)
  })

}
