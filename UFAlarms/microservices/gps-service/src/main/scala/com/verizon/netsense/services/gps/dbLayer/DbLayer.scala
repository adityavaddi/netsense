package com.verizon.netsense.services.gps.dbLayer

import com.verizon.netsense.services.gps.db.GpsSinglePointService
import com.verizon.netsense.services.gps.model.{Gps, OrgHierarchy, Status}

import scala.concurrent.{ExecutionContext, Future}

class DbLayer(gpsSinglePointService: GpsSinglePointService) {


  def storeGps(gps: Gps)(implicit ec: ExecutionContext): Future[Status] = {
    gpsSinglePointService.storeGps(gps)
      .map(x => x.wasApplied() match {
        case true => Status(true)
        case false => Status(false)
      })
  }


  def deleteGps(orgId: String,siteId: String,nodeId: String)(implicit ec: ExecutionContext): Future[Status] = {
    gpsSinglePointService.deleteGps(orgId,siteId,nodeId)
      .map(x => x.wasApplied() match {
        case true => Status(true)
        case false => Status(false)
      })
  }


  def getGpsByNodeId(nodeId: String,orgId: String,siteId: String)(implicit ec: ExecutionContext): Future[Option[Gps]] = {
    gpsSinglePointService.getGps(nodeId,orgId,siteId)
  }

  def getGpsByOrgIdAndSiteId(orgId: String,siteId: String)(implicit ec: ExecutionContext): Future[List[Gps]] = {
    gpsSinglePointService.getGpsForOrgAndSite(orgId,siteId)
      .map(x => x.size > 0 match {
        case true => x
        case false => Nil
      })
  }

  def getOrghierarchyByNodeId(nodeId: String)(implicit ec: ExecutionContext): Future[Option[OrgHierarchy]] = {
    gpsSinglePointService.getOrghierarchyByNodeId(nodeId)
  }

  def getOrgHierarchyByOrgIdAndSiteId(orgId: String,siteId: String)(implicit ec:ExecutionContext): Future[Option[OrgHierarchy]] = {
    gpsSinglePointService.getOrgHierarchyByOrgIdAndSiteId(orgId,siteId)
  }


}

object DbLayer {
  def apply(gpsSinglePointService: GpsSinglePointService): DbLayer = new DbLayer(new GpsSinglePointService{})
}