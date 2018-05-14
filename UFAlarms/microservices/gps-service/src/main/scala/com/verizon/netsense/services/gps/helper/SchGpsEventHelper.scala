package com.verizon.netsense.services.gps.helper

import java.util.Calendar

import com.fasterxml.jackson.core.JsonParseException
import com.verizon.netsense.services.gps.dbLayer.DbLayer
import com.verizon.netsense.services.gps.exceptions.OrgDataNotFoundException
import com.verizon.netsense.services.gps.model._
import com.verizon.netsense.services.gps.util.ObjectMapperUtil
import com.verizon.netsense.services.gps.util.StringConstants.GPS_SAMPLE
import com.verizon.netsense.utils.Logging
import org.apache.kafka.clients.consumer.ConsumerRecord

import scala.concurrent.{ExecutionContext, Future}

class SchGpsEventHelper(dbLayer: DbLayer) extends Logging{

  val filter: (Option[SchGpsEvent]) => Boolean = {
    case None => false
    case Some(schGpsEvent) => {
      if(schGpsEvent.sid!= null && schGpsEvent.l.lat!= null
        && schGpsEvent.l.lon != null && schGpsEvent.l.lat != null
        && schGpsEvent.l.t != null) {
        true
      } else {
        false
      }

    }
  }


  def parseGpsEvent(record: ConsumerRecord[String,String])(implicit ec: ExecutionContext,m: Manifest[SchGpsEvent]): Future[Option[SchGpsEvent]] = {
    ObjectMapperUtil.fromJsonAsync[SchGpsEvent](record.value()).recover{
      case ex: JsonParseException =>
        log.error("Unable to parse the element: " + record.value + " " + ex.getMessage); throw ex
    }
  }


  def populateGpsTuple2(gpsEvent: Option[SchGpsEvent])(implicit ec: ExecutionContext): Future[(SchGpsEvent, OrgHierarchy)] = {
    gpsEvent match {
      case None => throw new RuntimeException()
      case Some(gps) => {
        dbLayer.getOrghierarchyByNodeId(gps.sid)}.map {
        case None => throw new OrgDataNotFoundException()
        case Some(e) => Tuple2(gps, e)
      }
    }
  }

  def populateGpsModel(tuple: (SchGpsEvent, OrgHierarchy))(implicit ec: ExecutionContext): Future[Gps] = {
    val schGpsEvent = tuple._1
    val orgHierarchy = tuple._2
    log.debug("OrgHierarchy loaded from db " + orgHierarchy.toJSON)
    populateAndEnrichGps(schGpsEvent,orgHierarchy)
  }


  def populateAndEnrichGps(schEvent: SchGpsEvent, orgHierarchy: OrgHierarchy)(implicit ec: ExecutionContext): Future[Gps] = {

    val gps = Gps(schEvent.sid, Some(GPS_SAMPLE), orgHierarchy.orgId, orgHierarchy.siteId
      , Some(schEvent.l.lat), Some(schEvent.l.lon), None, None, Some(Calendar.getInstance().getTimeInMillis), Some(0))

    val gpsStatus = dbLayer.getGpsByNodeId(gps.nodeid, gps.orgid, gps.siteid).map {
      case None => {
        val updatedGps = gps.copy(latuseradded = gps.latitude,lonuseradded = gps.longitude)
        updatedGps
      }
      case Some(gpsdb) => {
        if (gpsdb.latuseradded == None && gpsdb.lonuseradded == None) {
          val updatedGps = gps.copy(latuseradded = gps.latitude, lonuseradded = gps.longitude, created = gpsdb.created,updated = Some(Calendar.getInstance().getTimeInMillis))
          updatedGps
        } else {
          val updatedGps = gps.copy(latuseradded = gpsdb.latuseradded, lonuseradded = gpsdb.lonuseradded, created = gpsdb.created,updated = Some(Calendar.getInstance().getTimeInMillis))
          updatedGps
        }

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

object SchGpsEventHelper {
  def apply(dbLayer: DbLayer): SchGpsEventHelper = new SchGpsEventHelper(dbLayer)
}
