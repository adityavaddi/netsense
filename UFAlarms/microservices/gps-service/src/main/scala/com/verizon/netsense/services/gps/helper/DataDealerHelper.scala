package com.verizon.netsense.services.gps.helper

import java.util.Calendar

import com.verizon.netsense.services.gps.dbLayer.DbLayer
import com.verizon.netsense.services.gps.exceptions._
import com.verizon.netsense.services.gps.model._
import com.verizon.netsense.services.gps.util.StringConstants.GPS_SAMPLE
import com.verizon.netsense.utils.Logging
import org.apache.cassandra.thrift.InvalidRequestException

import scala.concurrent.{ExecutionContext, Future}

class DataDealerHelper(dbLayer: DbLayer) extends Logging{

  def collectGpsPropsAndOrgHierArchy(apprequest: Option[AppRequest])(implicit ec: ExecutionContext): Future[(GpsProps, Option[OrgHierarchy])] = {
    apprequest match {
      case None => throw new AppRequestException()
      case Some(appReq) => {
        (appReq.request.orgprops, appReq.request.siteprops, appReq.request.gpsprops) match {
          case (Some(orgprops), Some(siteProps), Some(gpsProps)) => {
            dbLayer.getOrghierarchyByNodeId(gpsProps.nodeid).map(org => Tuple2(gpsProps, org))
          }
          case (None, None, None) => throw new InvalidRequestException()
          case (None, None, Some(gpsProps)) => throw new InvalidRequestException()
          case (None, Some(siteProps), Some(gpsProps)) => throw new OrgPropsEmptyException()
          case (Some(orgProps), None, Some(gpsProps)) => throw new SitePropsEmptyException()
          case (Some(orgProbs), Some(siteProps), None) => throw new GpsPropsEmptyException()

        }
      }
    }

  }

  def populateGps(tuple: (GpsProps, Option[OrgHierarchy]))(implicit ec: ExecutionContext): Future[Gps] = {
    val gpsProps = tuple._1
    val orgHierarchy = tuple._2
    orgHierarchy match {
      case None => throw new OrgDataNotFoundException()
      case Some(orgHierarchy) => {
        populateAndEnrichGps(gpsProps,orgHierarchy)

      }
    }


  }


  def populateAndEnrichGps(gpsProps: GpsProps, orgHierarchy: OrgHierarchy)(implicit ec: ExecutionContext): Future[Gps] = {

    val gps = Gps(gpsProps.nodeid, Some(gpsProps.name.getOrElse(GPS_SAMPLE)), orgHierarchy.orgId, orgHierarchy.siteId
      , None, None, gpsProps.latitude, gpsProps.longitude, Some(Calendar.getInstance().getTimeInMillis), Some(0))

    val gpsStatus = dbLayer.getGpsByNodeId(gps.nodeid, gps.orgid, gps.siteid).map {
      case None => {
        gps
      }
      case Some(gpsdb) => {
        val updatedGps = gps.copy(latitude = gpsdb.latitude, longitude = gpsdb.longitude, created = gpsdb.created,updated = Some(Calendar.getInstance().getTimeInMillis))
        updatedGps

      }
    }
    gpsStatus
  }


  def persistGps(gps: Gps)(implicit ec: ExecutionContext) : Future[Status] = {
    dbLayer.storeGps(gps).map(status => status.success match {
      case true => {
        log.debug("persisted Gps for Node: " + gps.nodeid)
        status
      }
      case false => {
        log.debug(" Failed to persist Gps " + gps.nodeid)
        status
      }
    })
  }



}




object DataDealerHelper {
  def apply(dbLayer: DbLayer): DataDealerHelper = new DataDealerHelper(dbLayer)
}
