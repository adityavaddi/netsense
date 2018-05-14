package com.verizon.netsense.businessalertservice.service

import java.util.Calendar

import com.verizon.netsense.businessalertservice.db.BusinessAlertsDbLayer
import com.verizon.netsense.businessalertservice.model.BusinessAlertConstants._
import com.verizon.netsense.businessalertservice.model.ResponseMessage._
import com.verizon.netsense.businessalertservice.model.StatusCodes._
import com.verizon.netsense.businessalertservice.model._
import com.verizon.netsense.businessalertservice.util.BARequestResponseGenerator
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.utils.Logging

import scala.concurrent.{ExecutionContext, Future}

/**
 * Business Alert Apis
 * Created by Rajitha on 15/02/18.
 */
class BusinessAlertService(dbLayer: BusinessAlertsDbLayer, reqResGenerator: BARequestResponseGenerator)
    extends Logging
    with Instrumented {

  implicit val ec = ExecutionContext.Implicits.global

  /**
   * Get Business Alert by businessAlertId
   * @param orgid
   * @param siteid
   * @param alertGetByIdRequest
   * @return
   */
  def getBusinessAlertByIdProcess(orgid: String,
                                  siteid: String,
                                  alertGetByIdRequest: AppRequest): Future[AppResponse] = {
    log.debug(
      s"Entered the getBusinessAlertByIdProcess to get the businessAlert with orgid: $orgid and siteid: $siteid"
    )
    val businessAlertId = alertGetByIdRequest.request.businessalertprops.get.businessAlertId.get
    log.debug(s"businessAlertId: $businessAlertId")

    dbLayer
      .getBusinessAlertById(orgid, siteid, businessAlertId)
      .map(_ match {
        case Some(businessAlert) => {
          log.debug("Successfully got the Business alert by businessAlertId " + businessAlert)
          reqResGenerator.successResponseWithoutFuture(
            alertGetByIdRequest,
            reqResGenerator.generateBusinessAlertResponse(businessAlert)
          )
        }
        case None =>
          reqResGenerator.failureCaseResponseWithoutFuture(
            alertGetByIdRequest,
            NOTFOUND.id,
            NotFound_BusinessAlertId_In_db.value + businessAlertId + Does_Not_Exist_OrgSite.value + orgid + Siteid.value + siteid
          )
      })

  }

  def getBusinessAlertSys(orgid: String, siteid: String, alertGetByIdRequest: AppRequest): Future[AppResponse] = {
    log.debug(
      s"Entered the getBusinessAlertSys to get the businessAlert with orgid: $orgid and siteid: $siteid"
    )
    val businessAlertId = alertGetByIdRequest.request.businessalertprops.get.businessAlertId.get
    log.debug(s"businessAlertId: $businessAlertId")

    dbLayer
      .getBusinessAlertByIdSys(orgid, siteid, businessAlertId)
      .map {
        case Some(businessAlert) =>
          if (businessAlert.active.getOrElse(false) && !businessAlert.severity
                .getOrElse("")
                .equalsIgnoreCase(ClearKey.value)) {
            log.debug("Successfully got the Business alert by businessAlertId " + businessAlert)
            reqResGenerator.successResponseWithoutFuture(
              alertGetByIdRequest,
              reqResGenerator.generateBusinessAlertSysResponse(businessAlert)
            )
          } else {
            log.debug(s"${Active_Or_Acknowledged_Alert.value} " + businessAlert)
            reqResGenerator.failureCaseResponseWithoutFuture(
              alertGetByIdRequest,
              NOTFOUND.id,
              Active_Or_Acknowledged_Alert.value
            )
          }

        case None =>
          reqResGenerator.failureCaseResponseWithoutFuture(
            alertGetByIdRequest,
            NOTFOUND.id,
            NotFound_BusinessAlertId_In_db.value + businessAlertId + Does_Not_Exist_OrgSite.value + orgid + Siteid.value + siteid
          )
      }
  }

  /**
   * Get all business alerts with in org and site
   * @param orgid
   * @param siteid
   * @param getAllBusinessAlertsRequest
   * @return
   */
  def getAllBusinessAlertsProcess(
      orgid: String,
      siteid: String,
      getAllBusinessAlertsRequest: AppRequest
  ): Future[AppSuccessResponse[List[BusinessAlertResponse]]] = {
    log.debug(s"Entered  getBusinessAlertsProcess with orgid: $orgid and siteid: $siteid")
    dbLayer
      .getAllBusinessAlerts(orgid, siteid)
      .map(listOfBusinessAlerts => {
        log.debug("Successfully got the BusinessAlerts for siteid and orgid")
        reqResGenerator.successResponseWithoutFuture(
          getAllBusinessAlertsRequest,
          listOfBusinessAlerts.map(businessAlert => reqResGenerator.generateBusinessAlertResponse(businessAlert))
        )
      })
  }

  /**
   * Dismiss business alert
   * @param orgid
   * @param siteid
   * @param businessAlertDismissByIdRequest
   * @return
   */
  def dismissBusinessAlertProcess(orgid: String,
                                  siteid: String,
                                  businessAlertDismissByIdRequest: AppRequest): Future[AppResponse] = {
    log.debug(s"Entered  dismissBusinessAlertProcess with orgid: $orgid and siteid: $siteid")

    val businessAlertId = businessAlertDismissByIdRequest.request.businessalertprops.get.businessAlertId.get
    log.debug(s"businessAlertId: $businessAlertId")
    dbLayer
      .getBusinessAlertById(orgid, siteid, businessAlertId)
      .flatMap(_ match {
        case None =>
          reqResGenerator.failureCaseResponse(
            businessAlertDismissByIdRequest,
            NOTFOUND.id,
            NotFound_BusinessAlertId_In_db.value + businessAlertId + Does_Not_Exist_OrgSite.value + orgid + Siteid.value + siteid
          )
        case Some(businessAlertResult) =>
          if (businessAlertResult.severity.getOrElse("").equalsIgnoreCase(ClearKey.value)) {
            reqResGenerator.successResponse(
              businessAlertDismissByIdRequest,
              SuccessWithMessage(false, s"Alert with businessAlertId $businessAlertId was dismissed earlier")
            )
          } else
            dismissBusinessAlertById(orgid,
                                     siteid,
                                     businessAlertId,
                                     businessAlertResult,
                                     businessAlertDismissByIdRequest)
      })

  }

  /**
   * Internal method to dismiss alert
   * @param orgid
   * @param siteid
   * @param businessAlertId
   * @param businessAlertResult
   * @param dismissAlertByIdRequest
   * @return
   */
  def dismissBusinessAlertById(orgid: String,
                               siteid: String,
                               businessAlertId: String,
                               businessAlertResult: BusinessAlert,
                               dismissAlertByIdRequest: AppRequest): Future[AppResponse] = {
    val userId        = dismissAlertByIdRequest.request.user.getOrElse("")
    val lastClearedAt = Calendar.getInstance().getTimeInMillis
    val businessAlertHistory = reqResGenerator.generateBusinessAlertHistory(
      businessAlertResult,
      ClearKey.value,
      userId,
      lastClearedAt
    )
    dbLayer
      .storeBusinessAlertHistory(businessAlertHistory)
      .flatMap(
        result =>
          result match {
            case true => {
              dbLayer
                .dismissByBusinesssAlertId(orgid, siteid, businessAlertId, ClearKey.value, userId, lastClearedAt)
                .map(
                  a =>
                    a match {
                      case true =>
                        log.debug(s"Successfully dismissed the Business alert by businessalertid : $businessAlertId")
                        reqResGenerator.successResponseWithoutFuture(dismissAlertByIdRequest, SuccessMessage(a))
                      case false =>
                        reqResGenerator.failureCaseResponseWithoutFuture(dismissAlertByIdRequest,
                                                                         INTERNALSERVERERROR.id,
                                                                         InternalError_DismissAlert.value)
                  }
                )
            }
            case false =>
              reqResGenerator
                .failureCaseResponse(dismissAlertByIdRequest, INTERNALSERVERERROR.id, InternalError_DismissAlert.value)
        }
      )
  }

  /**
   * Search Business alert by search criteria
   * @param orgid
   * @param siteid
   * @param alertGetByIdRequest
   * @return
   */
  def searchBusinessAlert(orgid: String, siteid: String, alertGetByIdRequest: AppRequest): Future[AppResponse] = {
    log.debug(s"Entered the searchBusinessAlert to get the business Alert with orgid: $orgid and siteid: $siteid")
    val searchCondition = alertGetByIdRequest.request.businessalertprops.get.search.get
    log.debug(s"searchcondition: ${searchCondition}")
    validateFilterCondition(searchCondition, "eq") match {
      case None =>
        reqResGenerator.failureCaseResponse(alertGetByIdRequest, BADREQUEST.id, condition_correct.value)
      case Some(filterString) => {
        val searchList = filterString.split(" eq ").toList
        if (isValidKey(Some(searchList.head)) == false)
          reqResGenerator.failureCaseResponse(alertGetByIdRequest, BADREQUEST.id, invalid_key.value)
        else if (searchList(0).toLowerCase == activeKey.value && !(Set("true", "false")
                   .contains(searchList(1).toLowerCase.trim))) {
          reqResGenerator.failureCaseResponse(alertGetByIdRequest, BADREQUEST.id, Invalid_Active.value)
        } else {
          dbLayer
            .searchBusinessAlert(orgid, siteid, searchList(0), searchList(1))
            .map(_ match {
              case listOfPC =>
                log.debug("Successfully got the Business alert by businessAlertId " + listOfPC)
                reqResGenerator.successResponseWithoutFuture(
                  alertGetByIdRequest,
                  listOfPC.map(businessAlert => reqResGenerator.generateBusinessAlertResponse(businessAlert))
                )

            })
        }
      }
    }

  }
  lazy val patternPrefix = "(^\\w+)\\s+"
  //lazy val patternSuffix="(\\s+\\w+)+"
  lazy val patternSuffix = "(\\s+.*)+"

  //checking the condition for format key eq value and now operator is just eq but in future it could be anything
  def validateFilterCondition(filterString: String, operatorKey: String): Option[String] = {
    val stringPattern = (patternPrefix + operatorKey + patternSuffix).r
    stringPattern.findFirstIn(filterString)
  }
// checking key is valid one or notstringPattern
  def isValidKey(key: Option[String]): Boolean =
    key match {
      case Some(value) =>
        Set(activeKey.value, severityKey.value, triggerNameKey.value, resourceIdKey.value).contains(value.trim)
      case None => false
    }

}

object BusinessAlertService {
  def apply(dbLayer: BusinessAlertsDbLayer, reqResGenerator: BARequestResponseGenerator): BusinessAlertService =
    new BusinessAlertService(dbLayer, reqResGenerator)
}
