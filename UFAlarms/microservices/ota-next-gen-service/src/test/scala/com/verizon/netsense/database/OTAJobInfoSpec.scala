package com.verizon.netsense.database

import java.util.UUID

import akka.actor.ActorSystem
import akka.testkit.TestKit
import com.datastax.driver.core.utils.UUIDs
import com.verizon.netsense.database.CassandraConnector.testConnector
import com.verizon.netsense.entity.OTAJobInfo
import org.cassandraunit.utils.EmbeddedCassandraServerHelper
import org.joda.time.{DateTime, DateTimeZone}
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

class OTAJobInfoSpec
  extends TestKit(ActorSystem("OTAJobInfoSpec-System"))
    with FunSuiteLike
    with MustMatchers
    with ScalaFutures
    with BeforeAndAfterAll
    with BeforeAndAfterEach {

  implicit val ec = ExecutionContext.Implicits.global

  override def beforeAll(): Unit = {
    EmbeddedCassandraServerHelper.startEmbeddedCassandra(20000)

    OTAJobInfoDB(testConnector).create(10.seconds)(ec.asInstanceOf[ExecutionContextExecutor])
  }

  override def afterAll(): Unit = {
    OTAJobInfoDB(testConnector).truncate(10.seconds)(ec.asInstanceOf[ExecutionContextExecutor])
  }


  test("store a Job Info record in DB") {
    val jobid = UUID.randomUUID().toString
    val firmwareid = "c12345def"
    val targetid = UUID.randomUUID().toString
    val targettype = "site"
    val count = 10
    val description = "Just a test"
    val when = UUIDs.timeBased()

    val jobInfo = OTAJobInfo(jobid, firmwareid, targetid, targettype, count, description, when)

    implicit val ec = scala.concurrent.ExecutionContext.global

    val chain = for {
      store <- OTAJobInfoDB(testConnector).model.store(jobInfo)
      get <- OTAJobInfoDB(testConnector).model.get(jobid)
    } yield get

    whenReady(chain) { result =>
      result mustBe List(
        OTAJobInfo(
          jobid,
          firmwareid,
          targetid,
          targettype,
          count,
          description,
          when
        )
      )
    }
  }


}
