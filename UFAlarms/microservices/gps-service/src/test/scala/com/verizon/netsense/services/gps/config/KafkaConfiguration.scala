package com.verizon.netsense.services.gps.config

import java.util.UUID

import akka.actor.ActorSystem
import akka.kafka.ProducerMessage.Message
import akka.kafka.Subscriptions
import akka.kafka.scaladsl.{Consumer, Producer}
import akka.stream.scaladsl.{Keep, Sink, Source}
import akka.stream.testkit.scaladsl.TestSink
import akka.stream.{ActorMaterializer, ActorMaterializerSettings, Materializer}
import akka.testkit.TestKit
import akka.{Done, NotUsed}
import com.verizon.netsense.services.gps.db.GpsSinglePointService
import com.verizon.netsense.services.gps.dbLayer.DbLayer
import com.verizon.netsense.services.gps.helper.KafkaConfig.groupId
import com.verizon.netsense.services.gps.helper.{DataDealerHelper, GpsHelper, KafkaConnector, SchGpsEventHelper}
import com.verizon.netsense.services.gps.services.{DataDealerRequestHandlerService, GpsEventIngestionService, ISRequestHandlerservice, SchGpsEventIngestionService}
import com.verizon.netsense.services.gps.util.ObjectMapperUtil
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.{StringDeserializer, StringSerializer}

import scala.concurrent.Future

class KafkaConfiguration extends TestKit(ActorSystem("TestKitActorSystem")) with TestSuiteConfig {

  val testActorSystem = system
  implicit val ec = system.dispatcher
  implicit val mat = ActorMaterializer(ActorMaterializerSettings(system))

  val dbLayer = new DbLayer(new GpsSinglePointService {})
  val gpsHelper = new GpsHelper(dbLayer)
  val ddHelper = new DataDealerHelper(dbLayer)
  val schGpsEventHelper = new SchGpsEventHelper(dbLayer)
  val gpsEventIngestionService = new GpsEventIngestionService(gpsHelper)
  val dataDealerRequestHandlerService = new DataDealerRequestHandlerService(ddHelper)
  val iSRequestHandlerservice = new ISRequestHandlerservice(gpsHelper)
  val schGpsEventIngestionService = new SchGpsEventIngestionService(schGpsEventHelper)

  val embeddedBootstrapServers = s"localhost:${configLoader.getString("embedded-kafka.kafka-port")}"

  val consumerSettings = KafkaConnector.configKafkaConsumerSettings(embeddedBootstrapServers,groupId,new StringDeserializer,new StringDeserializer)

  val producerSettings = KafkaConnector.configKafkaProducerSettings(embeddedBootstrapServers,new StringSerializer,new StringSerializer)

  def produceEvent[T](topic: String, event: T): Future[Done] = {
    val source = Source
      .single(event)
      .map(e => ObjectMapperUtil.toJson(e))
      .map(m => {
        Message(new ProducerRecord(topic, UUID.randomUUID().toString, m), NotUsed)
      })
      .viaMat(Producer.flow(producerSettings))(Keep.right)
    source.runWith(Sink.ignore)
  }

  def createEventProbe(topic: String)(implicit mat: Materializer): Unit = {
    Consumer.plainSource(consumerSettings,Subscriptions.topics(topic))
      .runWith(TestSink.probe)
  }

}
