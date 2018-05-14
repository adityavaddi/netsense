package com.verizon.netsense.database

import java.util.UUID

import akka.actor.ActorSystem
import akka.testkit.TestKit
import com.datastax.driver.core.utils.UUIDs
import com.verizon.netsense.database.CassandraConnector.testConnector
import com.verizon.netsense.entity.OTAJobStatus
import org.cassandraunit.utils.EmbeddedCassandraServerHelper
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

class OTAJobStatusSpec
  extends TestKit(ActorSystem("OTAJobStatusSpec-System"))
    with FunSuiteLike
    with MustMatchers
    with ScalaFutures
    with BeforeAndAfterAll
    with BeforeAndAfterEach {

  implicit val ec = ExecutionContext.Implicits.global

  override def beforeAll(): Unit = {
    EmbeddedCassandraServerHelper.startEmbeddedCassandra(20000)

    OTAJobStatusDB(testConnector).create(10.seconds)(ec.asInstanceOf[ExecutionContextExecutor])
  }

  override def afterAll(): Unit = {
    OTAJobStatusDB(testConnector).truncate(10.seconds)(ec.asInstanceOf[ExecutionContextExecutor])
  }


  test("store a Job Status record in DB") {
    val jobid = UUID.randomUUID().toString
    val status = "started"
    val when = UUIDs.timeBased()

    val jobStatus = OTAJobStatus(jobid, status, when)

    implicit val ec = scala.concurrent.ExecutionContext.global

    val chain = for {
      store <- OTAJobStatusDB(testConnector).model.store(jobStatus)
      get <- OTAJobStatusDB(testConnector).model.get(jobid)
    } yield get

    whenReady(chain) { result =>
      result mustBe List(
        OTAJobStatus(
          jobid,
          status,
          when
        )
      )
    }
  }


}
