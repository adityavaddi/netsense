package com.verizon.netsense.services.gps.db

import com.outworkers.phantom.dsl.ResultSet
import com.verizon.netsense.services.gps.constants.TestData._
import com.verizon.netsense.services.gps.dbLayer.DbLayer
import org.specs2.mock.mockito.MockitoStubs

import scala.concurrent.{ExecutionContext, Future}

class DbLayerSpec extends DatabaseSpec {

  val mockGpsDbService = mock[GpsSinglePointService]
  val spyDbLayer = MockitoStubs.spy(new DbLayer(mockGpsDbService))
  val resultset = mock[ResultSet]
  implicit val ec = ExecutionContext.Implicits.global


  "store Gps" should "return success status" in {
    doReturn(Future(resultset)).when(mockGpsDbService).storeGps(gps)
    doReturn(true).when(resultset).wasApplied()
    val result = spyDbLayer.storeGps(gps)

    whenReady(result) {res =>
      res.success mustBe true
    }

  }

  "store Gps" should "return success Failure status" in {
    doReturn(Future(resultset)).when(mockGpsDbService).storeGps(gps)
    doReturn(false).when(resultset).wasApplied()
    val result = spyDbLayer.storeGps(gps)

    whenReady(result) {res =>
      res.success mustBe false
    }

  }


  "delete Gps" should " remove gps data successfully" in {
    doReturn(Future(resultset)).when(mockGpsDbService).deleteGps(orgId,siteId,nodeId)
    doReturn(true).when(resultset).wasApplied()


    val result = spyDbLayer.deleteGps(orgId,siteId,nodeId)

    whenReady(result) { res =>
      res.success mustBe true
    }


  }

  "delete Gps" should " fail to remove gps data " in {
    doReturn(Future(resultset)).when(mockGpsDbService).deleteGps(orgId,siteId,nodeId)
    doReturn(false).when(resultset).wasApplied()

    val result = spyDbLayer.deleteGps(orgId,siteId,nodeId)

    whenReady(result) { res =>
      res.success mustBe false
    }


  }


  "get Gps by Node Id" should "return GPS data" in {
    doReturn(Future(Some(gps))).when(mockGpsDbService).getGps(nodeId,orgId,siteId)

    val result = spyDbLayer.getGpsByNodeId(nodeId,orgId,siteId)

    whenReady(result) { res =>
      res.get.nodeid eq nodeId
    }
  }

  "get Gps by OrgId and SiteId" should "return list of Gps" in {
    doReturn(Future(List(gps,gps2))).when(mockGpsDbService).getGpsForOrgAndSite(orgId,siteId)

    val result = spyDbLayer.getGpsByOrgIdAndSiteId(orgId,siteId)

    whenReady(result) { res =>
      res.size mustBe 2
    }

  }


  "get Org HierArchy By Node Id" should "return org HierArchy" in {
    doReturn(Future(Some(orgHierarchy))).when(mockGpsDbService).getOrghierarchyByNodeId(nodeId)
    val result = spyDbLayer.getOrghierarchyByNodeId(nodeId)

    whenReady(result) { res =>
      res.get.nodeId eq nodeId
    }
  }

  "get OrgHierArchy by Org and Site" should "return OrgHierArchy" in {
    doReturn(Future(Some(orgHierarchy))).when(mockGpsDbService).getOrgHierarchyByOrgIdAndSiteId(orgId,siteId)

    val result = spyDbLayer.getOrgHierarchyByOrgIdAndSiteId(orgId,siteId)

    whenReady(result) { res =>
      res.get.nodeId eq nodeId
    }
  }

}
