package com.verizon.netsense.services.handler

import akka.actor.{Actor, ActorSystem, Props}
import akka.testkit.{TestKit, TestProbe}
import com.verizon.netsense.actors.ISConsumerKafkaHandler
import com.verizon.netsense.entity.{ISApplyConfigToDeviceResponse, ISApplySingleParamToDeviceResponse, ISConfigResponse, ISResponseEvent}
import com.verizon.netsense.services.data.TestData
import com.verizon.netsense.services.mock.CassandraDatabaseMock
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, FlatSpecLike, MustMatchers}

import scala.concurrent.duration._

class ISConsumerKafkaHandlerSpec extends TestKit(ActorSystem("ISConsumerKafkaHandlerSpec-System"))
  with FlatSpecLike
  with MustMatchers
  with BeforeAndAfterAll
  with BeforeAndAfterEach
  with TestData {

  import ConfigServiceIntegrationSpec._

  val cassandraMock = new CassandraDatabaseMock
  val responseReceiverActorRef = system.actorOf(Props(new ResponseReceiverActor))



  val handler = new ISConsumerKafkaHandler(cassandraMock)

  it should "receive some message" in {

    val message1 = generateIsRequestWithHeaders(nodeId, "getDefaultConfigs", "falcon-q")
    val message2 = generateIsRequestWithHeaders(nodeId, "getDefaultConfigs", "unode-v6")

    handler.messageReceive(message1.toJSON)
    handler.messageReceive(message2.toJSON)

  }

    it should "create config" in {
      val message = generateIsConfigRequestWithHeaders(nodeId, "createConfig", "falcon-q")

      handler.messageReceive(message.toJSON)
    }

  it should "get default configs for site" in {
    val message1 = generateIsRequestWithHeaders(nodeId, "getDefaultConfigsForSite", "falcon-q")
    val message2 = generateIsRequestWithHeaders(nodeId, "getDefaultConfigsForSite", "unode-v6")

    handler.messageReceive(message1.toJSON)
    handler.messageReceive(message2.toJSON)
  }

  it should "send unsupported format" in {
    val message1 = generateIsRequestWithHeaders(nodeId, "unsupported", "falcon-q")

    handler.messageReceive(message1.toJSON)
  }

  it should "update config" in {
    val message = generateIsRequestWithHeaders(nodeId, "updateConfig", "falcon-q")
  }

  it should "apply config to nedes" in {
    val message1 = generateIsRequestWithHeaders(nodeId, "unsupported", "falcon-q")

    handler.messageReceive(message1.toJSON)
  }

  //  it should "receive confirm" in {
//      val finishWait = TestProbe()
//      system.eventStream.subscribe(finishWait.ref, classOf[FinishStatusApplyConfigToDevice])
//
//      finishWait.expectMsg(2.minutes, FinishStatusApplyConfigToDevice(true))
//  }
}

object ConfigServiceIntegrationSpec {

  case class FinishStatusApplySingleToDevice(val finished: Boolean)

  case class FinishStatusApplyConfigToDevice(val finished: Boolean)

  class ResponseReceiverActor(implicit val system: ActorSystem) extends Actor {

    override def receive: Receive = {
      case ISApplyConfigToDeviceResponse(messageId, uuid, replyTo, model, s, t) =>
        system.eventStream.publish(FinishStatusApplyConfigToDevice(true))
      case ISApplySingleParamToDeviceResponse(messageId, uuid, replyTo, model, s, t) =>
        println(s"Apply Single Received message: ${uuid}, ${s}, ${t}")
        system.eventStream.publish(FinishStatusApplySingleToDevice(true))
      case ISConfigResponse(messageId, requestId, replyTo, model, body, nodes) =>
        println(s"ISConfigResponse: ${requestId}, ${model}, ${body}")
        system.eventStream.publish(FinishStatusApplyConfigToDevice(true))

    }

    override def preStart(): Unit = {
      println(s"Subscribe")
      system.eventStream.subscribe(this.self, classOf[ISResponseEvent])
    }
  }

}
