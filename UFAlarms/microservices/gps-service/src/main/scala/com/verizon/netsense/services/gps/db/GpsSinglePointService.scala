package com.verizon.netsense.services.gps.db

import com.outworkers.phantom.dsl.ResultSet
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.services.gps.model.{Gps, OrgHierarchy}
import nl.grons.metrics.scala.Timer

import scala.concurrent.Future

trait GpsSinglePointService extends ProductionDataBase with Instrumented{

    private implicit val connector = GpsDatabase.connector
    private val cassandraTimer: Timer = metrics.timer("cassandra-processing-timer")

    def storeGps(gps: Gps): Future[ResultSet] = cassandraTimer.time {
      database.GpsRecord.storeGps(gps)
    }


    def deleteGps(orgId: String,siteId: String,nodeId: String): Future[ResultSet] = cassandraTimer.time {
      database.GpsRecord.deleteGps(orgId,siteId,nodeId)
    }

    def getGps(nodeId: String,orgId: String,siteId: String): Future[Option[Gps]] = cassandraTimer.time {
      database.GpsRecord.getGps(nodeId,orgId,siteId)
    }

    def getGpsForOrgAndSite(orgId: String,siteId: String): Future[List[Gps]] = cassandraTimer.time {
      database.GpsRecord.getGpsByOrgIdAndSiteId(orgId,siteId)
    }

    def getOrghierarchyByNodeId(nodeId: String): Future[Option[OrgHierarchy]] = cassandraTimer.time {
      database.OrgHierarchy.getById(nodeId)
    }

    def getOrgHierarchyByOrgIdAndSiteId(orgId: String,siteId: String): Future[Option[OrgHierarchy]] = cassandraTimer.time {
      database.OrgHierarchy.lookupOrgByOrgAndSite(orgId,siteId)
    }

}
