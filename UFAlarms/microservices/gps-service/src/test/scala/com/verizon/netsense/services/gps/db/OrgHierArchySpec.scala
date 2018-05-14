package com.verizon.netsense.services.gps.db

import com.datastax.driver.core.Session
import com.outworkers.phantom.connectors.KeySpace
import com.verizon.netsense.services.gps.config.TestSuiteConfig
import com.verizon.netsense.services.gps.constants.TestData._

import scala.concurrent.ExecutionContext


class OrgHierArchySpec extends DatabaseSpec with EmbeddedDatabase with TestSuiteConfig{

  implicit val ec = ExecutionContext.global
  implicit val connection = Defaults.connection
  implicit  val keySpace: KeySpace = KeySpace(Defaults.cassandraKeyspace)
  implicit val session: Session = GpsEmbeddedDatabase.session.init()

  def init() = {
    CassandraDBTest.OrgHierarchy.create.ifNotExists().future()
  }

  def clenup() = {
    CassandraDBTest.OrgHierarchy.truncate()
  }


  override def beforeAll(): Unit = {
    init()
    GpsEmbeddedDatabase.session.init()
  }

  override def afterAll(): Unit = {
    clenup()
  }

  "persist orgHierArchy" should "store orgHierarchy" in {
    val result = database.OrgHierarchy.storeHierArchy(orgHierarchy)

    whenReady(result) { res =>
      res.wasApplied()
      res.isExhausted
      res.one()
    }
  }

  "get OrgHierArchy By NodeId" should "return orgHierarchy for node" in {
    val result = database.OrgHierarchy.getById(nodeId)

    whenReady(result) { res =>
      res.size mustBe 1
    }
  }

  ignore  should "return first entry of an org" in {
    val result = {
      database.OrgHierarchy.storeHierArchy(orgHierarchy)
      database.OrgHierarchy.lookupOrgByOrgAndSite(orgId,siteId)
    }
    whenReady(result) { res =>
      res.size mustBe 1
      //res.asInstanceOf[OrgHierarchy].orgId eq orgId
      //res.asInstanceOf[OrgHierarchy].siteId eq siteId

    }
  }

}
