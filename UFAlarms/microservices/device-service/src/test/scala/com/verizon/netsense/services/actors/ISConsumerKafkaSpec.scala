package com.verizon.netsense.services.actors

import akka.actor.{Actor, ActorSystem, Props}
import akka.testkit.{TestKit, TestProbe}
import com.verizon.netsense.actors.ISConsumerHandler
import com.verizon.netsense.services.data.TestData
import com.verizon.netsense.services.mock.CassandraDatabaseMock
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, FlatSpecLike, MustMatchers}

class ISConsumerKafkaSpec extends TestKit(ActorSystem("ISConsumerActorSpec-System"))
  with FlatSpecLike
  with MustMatchers
  with BeforeAndAfterAll
  with BeforeAndAfterEach
  with TestData {




  val cassandraMock = new CassandraDatabaseMock
  val handler = new ISConsumerHandler(cassandraMock)

  "ISConsumerHandler" should "receive IS message" in {
    val message = generateIsRequestWithHeaders(nodeId, "getNodeStatus", "falcon-q")

    handler.messageReceive(message.toJSON)
  }

}
