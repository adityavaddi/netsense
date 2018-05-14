package com.verizon.netsense.actors

import akka.actor.{ActorSystem, Props, Scheduler, Scope}
import scala.concurrent.duration._
import org.joda.time.{DateTime, DateTimeZone}
import java.util.UUID
import org.apache.cassandra.utils.UUIDGen
import org.cassandraunit.utils.EmbeddedCassandraServerHelper
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}
import akka.testkit.{ImplicitSender, TestKit}
import com.verizon.netsense.actors.SicStatusManager._
import com.verizon.netsense.database._
import com.verizon.netsense.connector.CassandraConnector.testConnector
import com.verizon.netsense.entity.SicStatus

class SicStatusManagerSpec  extends TestKit(ActorSystem("SicStatusManagerTest-System"))
  with ScalaFutures
  with ImplicitSender
  with WordSpecLike
  with BeforeAndAfterAll
  with BeforeAndAfterEach
  with Matchers {

  implicit val ec = ExecutionContext.Implicits.global

  override def beforeAll(): Unit = {
    EmbeddedCassandraServerHelper.startEmbeddedCassandra()
    SicStatusDB(testConnector).create(120.seconds)(ec.asInstanceOf[ExecutionContextExecutor])
  }

  /*
  trait TestScope extends  Scope  {
    val mockScheduler = mock[Scheduler]
    val actor = TestActorRef(new SicStatusManager(CassandraConnector.connector){
      override def scheduler = mockScheduler })
  }
  */

  val sicActorRef = system.actorOf(Props(new SicStatusManager(testConnector)), "sicTestActor")


  "SICStatusManager" should {

    "should receive an IS REQ message and store in DB" in  {
      val msgId = UUID.randomUUID().toString

      sicActorRef ! ISReqReceived(Map("msgId" -> msgId, "topic" -> "ms.request.media"))
      expectNoMsg
      ignoreNoMsg

      val getById = for {
        get <- SicStatusDB(testConnector).model.getByType(msgId, "IS", MsgType.REQ.toString)
      } yield get

      whenReady(getById) { result =>
        result shouldBe a[Some[_]]
        result should not be empty
        result.get.state shouldBe(MsgState.RCVD.toString)

      }
    }

    "should receive an IS RESP SENT message and store in DB" in  {
      val msgId = UUID.randomUUID().toString

      sicActorRef ! ISRespSent(Map("msgId" -> msgId, "topic" -> "ms.api.reply"))
      expectNoMsg
      ignoreNoMsg

      val getById = for {
        get <- SicStatusDB(testConnector).model.getByType(msgId, "IS", MsgType.RESP.toString)
      } yield get

      whenReady(getById) { result =>
        result shouldBe a[Some[_]]
        result should not be empty
        result.get.state shouldBe(MsgState.SENT.toString)

      }

    }


    "should receive an SIC REQ SENT message" in  {
      val msgId = UUID.randomUUID().toString

      sicActorRef ! SicReqSent(Map("msgId" -> msgId, "topic" -> "node.command"))
      expectNoMsg
      ignoreNoMsg

      val getById = for {
        get <- SicStatusDB(testConnector).model.getByType(msgId, "SIC_KAFKA", MsgType.REQ.toString)
      } yield get

      whenReady(getById) { result =>
        result shouldBe a[Some[_]]
        result should not be empty
        result.get.state shouldBe(MsgState.SENT.toString)
      }
    }

    "should receive an SIC ACK REC message" in  {
      val msgId = UUID.randomUUID().toString

      sicActorRef ! SicAckReceived(Map("msgId" -> msgId, "topic" -> "media"))
      expectNoMsg
      ignoreNoMsg

      val getById = for {
        get <- SicStatusDB(testConnector).model.getByType(msgId, "SIC_KAFKA", MsgType.ACK.toString)
      } yield get

      whenReady(getById) { result =>
        result shouldBe a[Some[_]]
        result should not be empty
        result.get.state shouldBe(MsgState.RCVD.toString)
      }
    }

    "should receive an SIC RESP REC message" in  {
      val msgId = UUID.randomUUID().toString

      sicActorRef ! SicRespReceived(Map("msgId" -> msgId, "topic" -> "media"))
      expectNoMsg
      ignoreNoMsg

      val getById = for {
        get <- SicStatusDB(testConnector).model.getByType(msgId, "SIC_KAFKA", MsgType.RESP.toString)
      } yield get

      whenReady(getById) { result =>
        result shouldBe a[Some[_]]
        result should not be empty
        result.get.state shouldBe(MsgState.RCVD.toString)
      }
    }


    "should receive and reply-to status check request" in {
      val id = UUIDGen.getTimeUUID(System.currentTimeMillis())
      val msgId = UUID.randomUUID().toString
      val msgSrc = "IS"
      val topic = "ms.api.request"
      val mtype = "REQ"
      val state = "REC"
      val timestamp: DateTime = new DateTime(id.timestamp(),DateTimeZone.UTC)
      val count = 0
      val reason = "test"

      val newState = SicStatus( id, msgId, msgSrc,
         mtype,state, timestamp,  reason)

      implicit val ec = scala.concurrent.ExecutionContext.global

      sicActorRef ! ISReqReceived(Map("msgId" -> msgId, "topic" -> "ms.api.request"))
      expectNoMsg
      sicActorRef ! CheckStatus(msgId, "IS", MsgType.REQ)
      expectNoMsg

    }

    /* Todo - Mix in Specs2 ?
    "schedule a callback to Check Status" in new TestScope{
      val id = UUIDs.timeBased()
      val ts = new DateTime(id.timestamp(),DateTimeZone.UTC)
      actor ! ISReqReceived(Map("msgId" -> "msg1", "topic" -> "media"))
      there was one(mockScheduler.scheduleOnce(60 seconds, self, UpdateStatus("msg1", "IS",
        msgType.REQ, msgState.RCVD, ts)))
    }
    */
  }

  override def afterAll {
    SicStatusDB(testConnector).truncate(120.seconds)(ec.asInstanceOf[ExecutionContextExecutor])

    TestKit.shutdownActorSystem(system)
  }

}
