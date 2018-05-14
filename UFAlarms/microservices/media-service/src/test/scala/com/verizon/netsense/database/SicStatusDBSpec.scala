package com.verizon.netsense.database

import java.util.UUID

import akka.actor.ActorSystem
import akka.testkit.TestKit
import com.verizon.netsense.connector.CassandraConnector.testConnector
import com.verizon.netsense.entity.SicStatus
import org.apache.cassandra.utils.UUIDGen
import org.cassandraunit.utils.EmbeddedCassandraServerHelper
import org.joda.time.{DateTime, DateTimeZone}
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

class SicStatusDBSpec
  extends TestKit(ActorSystem("SicStatusDBSpec-System"))
    with FunSuiteLike
    with MustMatchers
    with ScalaFutures
    with BeforeAndAfterAll
    with BeforeAndAfterEach {

  implicit val ec = ExecutionContext.Implicits.global

  override def beforeAll(): Unit = {
    EmbeddedCassandraServerHelper.startEmbeddedCassandra()

    SicStatusDB(testConnector).create(120.seconds)(ec.asInstanceOf[ExecutionContextExecutor])
  }

  override def afterAll(): Unit = {
    SicStatusDB(testConnector).truncate(120.seconds)(ec.asInstanceOf[ExecutionContextExecutor])
  }

  val id = UUIDGen.getTimeUUID(System.currentTimeMillis())
  val msgId = UUID.randomUUID().toString
  val msgSrc = "IS"
  val topic = "ms.api.request"
  val mtype = "REQ"
  val state = "REC"
  val timestamp: DateTime = new DateTime(id.timestamp(),DateTimeZone.UTC)
  val count = 0
  val reason = "test"

  test("Store a Status record in DB") {

    val newState = SicStatus( id, msgId, msgSrc,
      mtype,state, timestamp, reason)

    implicit val ec = scala.concurrent.ExecutionContext.global

    val getById = for {
      store <- SicStatusDB(testConnector).model.store(newState)
      get <- SicStatusDB(testConnector).model.getByType(msgId, msgSrc, mtype)
    } yield get

    whenReady(getById) { result =>
      result mustBe Some(
        SicStatus( id, msgId, msgSrc,
           mtype,state, timestamp, reason)
      )
    }

  }

  test("Retrieve Status Record by Type") {
    val id = UUIDGen.getTimeUUID(System.currentTimeMillis())
    val msgId = UUID.randomUUID().toString
    val newState = SicStatus( id, msgId, msgSrc,
       mtype,state, timestamp, reason)
    val getByType = for {
      store <- SicStatusDB(testConnector).model.store(newState)
      get <- SicStatusDB(testConnector).model.getByType(msgId, msgSrc, mtype)
    } yield get

    whenReady(getByType) { result =>
      result mustBe Some(
        SicStatus( id, msgId, msgSrc,
           mtype,state, timestamp, reason)
      )
    }

  }


}