package com.verizon.netsense.services.services

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Keep, Sink, Source}
import akka.stream.testkit.scaladsl.TestSink
import akka.testkit.TestKit
import com.fasterxml.jackson.annotation.JsonProperty
import com.verizon.netsense.model._
import com.verizon.netsense.services.DeviceLoginKafkaService
import com.verizon.netsense.services.data.TestData
import org.apache.kafka.clients.consumer.ConsumerRecord
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, FlatSpecLike, MustMatchers}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext}

class DeviceLoginKafkaServiceSpec extends TestKit(ActorSystem("QueryServiceIntegrationSpec-System"))
with FlatSpecLike
with MustMatchers
with BeforeAndAfterAll
with BeforeAndAfterEach
with TestData {

  implicit val ec = ExecutionContext.Implicits.global
  implicit val mat                  = ActorMaterializer()(system)

  //val deviceLoginginService = new DeviceLoginKafkaService
}
