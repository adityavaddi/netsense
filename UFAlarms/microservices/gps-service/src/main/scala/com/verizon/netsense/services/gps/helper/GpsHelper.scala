package com.verizon.netsense.services.gps.helper

import java.time.Instant
import java.util.Calendar

import com.fasterxml.jackson.core.JsonParseException
import com.verizon.netsense.services.gps.dbLayer.DbLayer
import com.verizon.netsense.services.gps.exceptions._
import com.verizon.netsense.services.gps.model.CaselType._
import com.verizon.netsense.services.gps.model.StatusCodes._
import com.verizon.netsense.services.gps.model._
import com.verizon.netsense.services.gps.util.StringConstants._
import com.verizon.netsense.services.gps.util.{GpsModelUtil, ObjectMapperUtil}
import com.verizon.netsense.utils.Logging
import org.apache.kafka.clients.consumer.ConsumerRecord

import scala.concurrent.{ExecutionContext, Future}


class GpsHelper(dbLayer: DbLayer) extends Logging {


  val validate: (Option[GpsEvent]) => Boolean = {
    case None => false
    case Some(gpsEvent) => {
      if (gpsEvent.lat != null && gpsEvent.lon != null
        && gpsEvent.nodeid != null && gpsEvent.name != null
        && gpsEvent.epochsecs != null) {
        true
      } else {
        false
      }
    }
  }

  def parseGpsEvent(record: ConsumerRecord[String, String])(implicit ec: ExecutionContext, m: Manifest[GpsEvent]): Future[Option[GpsEvent]] = {
    ObjectMapperUtil.fromJsonAsync[GpsEvent](record.value()).recover {
      case ex: JsonParseException =>
        log.error("Unable to parse the element: " + record.value + " " + ex.getMessage); throw ex
    }
  }

  def populateGpsTuple2(gpsEvent: Option[GpsEvent])(implicit ec: ExecutionContext): Future[(GpsEvent, OrgHierarchy)] = {
    gpsEvent match {
      case None => throw new RuntimeException()
      case Some(gps) => dbLayer.getOrghierarchyByNodeId(gps.nodeid).map {
        case None => throw new OrgDataNotFoundException()
        case Some(e) => Tuple2(gps, e)
      }
    }
  }

  def populateAndEnrichGps(gpsEvent: GpsEvent,orgHierarchy: OrgHierarchy)(implicit ec: ExecutionContext): Future[Gps] = {

    val gps = Gps(gpsEvent.nodeid, Some(gpsEvent.name.getOrElse(GPS_SAMPLE)), orgHierarchy.orgId, orgHierarchy.siteId
      , Some(gpsEvent.lat), Some(gpsEvent.lon), None, None, Some(Calendar.getInstance().getTimeInMillis), Some(0))

    val enrichedGps = dbLayer.getGpsByNodeId(gps.nodeid, gps.orgid, gps.siteid).map {
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
    enrichedGps
  }

  def populateGpsModel(tuple: (GpsEvent, OrgHierarchy))(implicit ec: ExecutionContext): Future[Gps] = {
    val gpsEvent = tuple._1
    val orgHierarchy = tuple._2
    log.debug("Gps event received" + gpsEvent.toJSON)
    log.debug("OrgHierarchy loaded from db " + orgHierarchy.toJSON)
    populateAndEnrichGps(gpsEvent,orgHierarchy)
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

  def processGetAppRequest(apprequest: Option[AppRequest])(implicit ec: ExecutionContext): Future[AppResponse] = {

    apprequest match {
      case None => throw new AppRequestException()
      case Some(appReq) => {
        appReq.request.`type` match {
          case GET_GPS_BY_NODE_ID.value => {
            (appReq.request.orgprops,appReq.request.siteprops,appReq.request.nodeprops) match {
              case (Some(orgprops),Some(siteProps),Some(nodeProps)) => {
                dbLayer.getGpsByNodeId(nodeProps.nodeId,orgprops.orgId,siteProps.siteId).map(res =>
                  AppSuccessResponse(appReq.messageid,SuccessResponseBody(appReq.request.requestid,Instant.now().toString,true,GpsModelUtil.toGpsIsModel(res))))
              }
              case (None, None, None) => Future(AppFailureResponse(appReq.messageid, FailureResponseBody(
                appReq.request.requestid, Instant.now().toString, false, INVALID_REQUEST_WITH_MISSING_FIELDS, BADREQUEST.id)))
              case (None, None, Some(gpsProps)) => Future(AppFailureResponse(appReq.messageid, FailureResponseBody(
                appReq.request.requestid, Instant.now().toString, false, INVALID_REQUEST_WITH_MISSING_FIELDS, BADREQUEST.id
              )))
              case (None, Some(siteProps), Some(gpsProps)) => Future(AppFailureResponse(appReq.messageid, FailureResponseBody(
                appReq.request.requestid, Instant.now().toString, false, INVALID_REQUEST_WITH_MISSING_ORG_PROPS, BADREQUEST.id
              )))
              case (Some(orgProps), None, Some(gpsProps)) => Future(AppFailureResponse(appReq.messageid, FailureResponseBody(
                appReq.request.requestid, Instant.now().toString, false, INVALID_REQUEST_WITH_MISSING_SITE_PROPS, BADREQUEST.id
              )))
              case (Some(orgProbs), Some(siteProps), None) => Future(AppFailureResponse(appReq.messageid, FailureResponseBody(
                appReq.request.requestid, Instant.now().toString, false, INVALID_REQUEST_WITH_MISSING_NODE_PROPS, BADREQUEST.id
              )))

            }

          }
          case GET_ALL_GPS_FOR_ORG_ID_AND_SITE_ID.value => {
            (appReq.request.orgprops, appReq.request.siteprops) match {
              case (Some(orgProps), Some(siteProps)) => {
                dbLayer.getGpsByOrgIdAndSiteId(orgProps.orgId, siteProps.siteId).map(res =>
                  AppSuccessResponse(appReq.messageid, SuccessResponseBody(appReq.request.requestid, Instant.now().toString, true, GpsModelUtil.toGpsISModelList(res))))
              }
              case (None, Some(siteProps)) => Future(AppFailureResponse(appReq.messageid, FailureResponseBody(
                appReq.request.requestid, Instant.now().toString, false, INVALID_REQUEST_WITH_MISSING_ORG_PROPS, BADREQUEST.id
              )))
              case (Some(orgProps), None) => Future(AppFailureResponse(appReq.messageid, FailureResponseBody(
                appReq.request.requestid, Instant.now().toString, false, INVALID_REQUEST_WITH_MISSING_SITE_PROPS, BADREQUEST.id
              )))
              case (None, None) => Future(AppFailureResponse(appReq.messageid, FailureResponseBody(
                appReq.request.requestid, Instant.now().toString, false, INVALID_REQUEST_WITH_MISSING_FIELDS, BADREQUEST.id
              )))
            }

          }
          case _ => Future(AppFailureResponse(appReq.messageid, FailureResponseBody(
            appReq.request.requestid, Instant.now().toString, false, NOT_MATCHING_CASEL_TYPE, BADREQUEST.id
          )))
        }
      }
    }
  }


}

object GpsHelper {
  def apply(dbLayer: DbLayer): GpsHelper = new GpsHelper(dbLayer)
}
