package com.verizon.netsense.database

import java.util.UUID

import akka.actor.ActorSystem
import akka.testkit.TestKit
import com.datastax.driver.core.utils.UUIDs
import com.verizon.netsense.database.CassandraConnector.testConnector
import com.verizon.netsense.entity.OTANodeStatus
import org.cassandraunit.utils.EmbeddedCassandraServerHelper
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

class OTANodeStatusSpec
  extends TestKit(ActorSystem("OTANodeStatusSpec-System"))
    with FunSuiteLike
    with MustMatchers
    with ScalaFutures
    with BeforeAndAfterAll
    with BeforeAndAfterEach {

  implicit val ec = ExecutionContext.Implicits.global

  override def beforeAll(): Unit = {
    EmbeddedCassandraServerHelper.startEmbeddedCassandra(20000)

    OTANodeStatusDB(testConnector).create(10.seconds)(ec.asInstanceOf[ExecutionContextExecutor])
  }

  override def afterAll(): Unit = {
    OTANodeStatusDB(testConnector).truncate(10.seconds)(ec.asInstanceOf[ExecutionContextExecutor])
  }


  test("store a Node Status record in DB") {
    val jobid = UUID.randomUUID().toString
    val nodeid = UUID.randomUUID().toString
    val status = "started"
    val progress = 937209
    val when = UUIDs.timeBased()

    val nodeStatus = OTANodeStatus(jobid, nodeid, status, progress, when)

    implicit val ec = scala.concurrent.ExecutionContext.global

    val chain = for {
      store <- OTANodeStatusDB(testConnector).model.store(nodeStatus)
      get <- OTANodeStatusDB(testConnector).model.get(jobid)
    } yield get

    whenReady(chain) { result =>
      result mustBe List(
        OTANodeStatus(
          jobid,
          nodeid,
          status,
          progress,
          when
        )
      )
    }
  }


}
