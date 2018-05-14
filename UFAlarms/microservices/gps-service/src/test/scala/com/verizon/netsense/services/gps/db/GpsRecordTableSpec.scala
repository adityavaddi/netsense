package com.verizon.netsense.services.gps.db

import com.outworkers.phantom.connectors.KeySpace
import com.verizon.netsense.connector.CassandraConnector
import com.verizon.netsense.services.gps.config.TestSuiteConfig
import com.verizon.netsense.services.gps.constants.TestData._
import com.verizon.netsense.services.gps.db.CassandraDBTest._

import scala.concurrent.ExecutionContext

class GpsRecordTableSpec extends DatabaseSpec with EmbeddedDatabase with TestSuiteConfig with CassandraConnector.testConnector.Connector {

  implicit val ec = ExecutionContext.global
  implicit val connection = Defaults.connection
  implicit override val space: KeySpace = KeySpace(Defaults.cassandraKeyspace)

  def init() = {
    GpsRecords.create.ifNotExists().future()
  }

  def clenup() = {
    GpsRecords.truncate()
  }


  override def beforeAll(): Unit = {
    init()
    GpsEmbeddedDatabase.session.init()
  }

  override def afterAll(): Unit = {
    clenup()
  }

  "persist Gps" should "store Gps in Cassandra" in  {

    val future = database.GpsRecord.storeGps(gps)

    whenReady(future) { res =>
      res wasApplied()
      res isExhausted()
      res one()

    }

  }



  "delete Gps" should "remove GPs from database" in {
    val result = {
      database.GpsRecord.deleteGps(orgId,siteId,nodeId)
    }
    whenReady(result) {res =>
      res.wasApplied()
      res.isExhausted()
      res one()
    }
  }

  "get GPS By Node Id" should "return Gps data for node Id" in {
    val result = for {
      save <- database.GpsRecord.storeGps(gps)
      get <- database.GpsRecord.getGps(nodeId, orgId, siteId)
    } yield get

        whenReady(result) { res =>
        res.size mustBe 1
      }
    }


  "get All Gps By Org and Site Ids" should "return the all the Gps in the Org and Site" in {

    val result = for {
      save <- database.GpsRecord.storeGps(gps)
      update <- database.GpsRecord.storeGps(gps2)
      get <- database.GpsRecord.getGpsByOrgIdAndSiteId(orgId,siteId)
    } yield get

    whenReady(result) { res =>
      res.size mustBe 2
    }

  }

}
