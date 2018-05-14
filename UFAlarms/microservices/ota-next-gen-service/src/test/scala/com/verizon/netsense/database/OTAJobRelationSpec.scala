package com.verizon.netsense.database

import java.util.UUID

import akka.actor.ActorSystem
import akka.testkit.TestKit
import com.datastax.driver.core.utils.UUIDs
import com.verizon.netsense.database.CassandraConnector.testConnector
import com.verizon.netsense.entity.OTAJobRelation
import org.cassandraunit.utils.EmbeddedCassandraServerHelper
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

class OTAJobRelationSpec
  extends TestKit(ActorSystem("OTAJobRelationSpec-System"))
    with FunSuiteLike
    with MustMatchers
    with ScalaFutures
    with BeforeAndAfterAll
    with BeforeAndAfterEach {

  implicit val ec = ExecutionContext.Implicits.global

  override def beforeAll(): Unit = {
    EmbeddedCassandraServerHelper.startEmbeddedCassandra(20000)

    OTAJobRelationDB(testConnector).create(10.seconds)(ec.asInstanceOf[ExecutionContextExecutor])
  }

  override def afterAll(): Unit = {
    OTAJobRelationDB(testConnector).truncate(10.seconds)(ec.asInstanceOf[ExecutionContextExecutor])
  }


  test("store a Job Relation record in DB") {
    val orgid = UUID.randomUUID().toString
    val siteid = UUID.randomUUID().toString
    val jobid = UUID.randomUUID().toString
    val when = UUIDs.timeBased()

    val jobRelation = OTAJobRelation(orgid, siteid, jobid, when)

    implicit val ec = scala.concurrent.ExecutionContext.global

    val chain = for {
      store <- OTAJobRelationDB(testConnector).model.store(jobRelation)
      get <- OTAJobRelationDB(testConnector).model.get(orgid, siteid)
    } yield get

    whenReady(chain) { result =>
      result mustBe List(
        OTAJobRelation(
          orgid,
          siteid,
          jobid,
          when
        )
      )
    }
  }


}
