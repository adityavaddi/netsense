package com.verizon.netsense.db

import com.verizon.netsense.config.{Neo4jConnection, TestSuiteConfig}
import com.verizon.netsense.data.TestData
import com.verizon.netsense.db.neo4j.{Neo4jHelper, Neo4jService}
import com.verizon.netsense.model.Fixture
import com.verizon.netsense.util.BaseSpec
import com.verizon.netsense.utils.Logging

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}


/**
  * Created by nalamte on 3/25/18.
  */

class DbLayerSpec extends BaseSpec
                          with TestSuiteConfig
                          with TestData
                          with Logging{

  private implicit val ec = scala.concurrent.ExecutionContext.Implicits.global

  val siteid = siteId
  val nodeid2 = nodeId

  override def beforeAll(): Unit = {
    super.beforeAll()
    println("initializing neo4j")
    neo4jInit
  }

  override def afterAll(): Unit = {
    super.afterAll()
    println("Cleaning up neo4j")
    neo4jCleanup()
  }

  def neo4jCleanup(): Unit = {
    lazy val neo4JSession = Neo4jConnection.neo4jDriver.session()

    val deleteNode = s"MATCH (a:Node {nodeid: '$nodeid2'})" +
      s" DETACH DELETE a RETURN" +
      s" { success: true }"
    try {
      neo4JSession.run(deleteNode)
    } catch {
      case ex: Exception => log.error("Unable to cleanup the graph db " + ex)
    } finally {
      neo4JSession.close()
    }
  }

  def neo4jInit = {
    lazy val neo4JSession = Neo4jConnection.neo4jDriver.session()
    try {
      neo4JSession.run(createFixture)
    } catch {
      case ex: Exception => log.error("***** Failed in Initializing Neo4j DB " + ex)
    }
    finally {
      neo4JSession.close()
    }
  }

  val dbTest = new DbLayer(new PhantomService with ProductionDatabase{},
    new Neo4jService(new Neo4jHelper) {})

  "DbLayer" should "Convert get fixture from neo4j" in {

    val Result =  dbTest.getFixtureForNode(fixturenodeid)

    val ActualResult = Await.result(Result, 20.seconds)

    val eResult = Future(Fixture(fixturenodeid, Some("unode-v7"), Some("10"), Some("30"), Some("50"), Some("70"), Some("120"), Some("140"), Some("160"), Some("180")))

    val ExprResult = Await.result(eResult, 20.seconds)
    ActualResult mustBe ExprResult
  }
}