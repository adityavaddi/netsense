package com.vz.nsp.trigger.service

import java.util.UUID

import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.utils.Logging
import com.vz.nsp.trigger.dblayer.DbLayer
import com.vz.nsp.trigger.model.app.{TriggerRequest, TriggerResponse}
import com.vz.nsp.trigger.model.casel.ResponseMessage._
import com.vz.nsp.trigger.model.casel.StatusCodes._
import com.vz.nsp.trigger.model.casel.{AppRequest, AppResponse, AppSuccessResponse}
import com.vz.nsp.trigger.model.db.DBTrigger
import com.vz.nsp.triggerservice.helper.ReqResGenerator
import nl.grons.metrics.scala.Timer
import com.vz.nsp.trigger.helper.CommonHelper.generateUUID
import scala.concurrent.{ExecutionContext, Future}

/**
 * Created by maleva on 2/9/18.
 */
class TriggerProcessor(dbLayer: DbLayer, reqResGenerator: ReqResGenerator) extends Logging with Instrumented {

  private[this] val cassandraReadTriggerTimer: Timer     = metrics.timer("cassandra-read-triggers")
  private[this] val cassandraReadAllTriggersTimer: Timer = metrics.timer("cassandra-read-all-triggers")
  private[this] val cassandraPostTriggerTimer: Timer     = metrics.timer("cassandra-create-trigger")
  private[this] val cassandraDeletetriggerTimer: Timer   = metrics.timer("cassandra-delete-trigger")
  private[this] val cassandraUpdateTriggerTimer: Timer   = metrics.timer("cassandra-update-trigger")

  implicit lazy val ec = ExecutionContext.Implicits.global

  def getTriggerProcess(orgid: String, siteid: String, triggerGetByIdRequest: AppRequest): Future[AppResponse] = {
    log.debug(s"Entered the getTriggerProcess to get the tag with orgid: $orgid and siteid: $siteid")
    triggerGetByIdRequest.request.triggerprops match {
      case Some(triggerProps) =>
        triggerProps.triggerId match {
          case None =>
            log.error("triggerId not found in request")
            reqResGenerator.failurecaseResponse(triggerGetByIdRequest, NOTFOUND.id, Missing_TriggerId.value)
          case Some(triggerid) =>
            log.debug(s"triggerId: $triggerid")
            cassandraReadTriggerTimer
              .time(
                dbLayer
                  .getTriggerByTriggerId(orgid, siteid, triggerid)
              )
              .map {
                case None =>
                  log.info(s"Could not find trigger in DB with id $triggerid")
                  reqResGenerator.failurecaseResponseWithoutFuture(
                    triggerGetByIdRequest,
                    NOTFOUND.id,
                    Missing_DBTriggerId.value + triggerid + Does_Not_Exist_OrgSite.value + orgid + Siteid.value + siteid
                  )
                case Some(dbTrigger) =>
                  log.debug("Successfully got the trigger by triggerId " + dbTrigger)
                  val triggerResponse = TriggerResponse(dbTrigger)
                  reqResGenerator.successResponseWithoutFuture(triggerGetByIdRequest, triggerResponse)
              }
        }
      case None =>
        log.error(s"Triggerprops is empty in the appRequest $triggerGetByIdRequest")
        reqResGenerator.failurecaseResponse(triggerGetByIdRequest, NOTFOUND.id, Missing_Triggerprops.value)
    }
  }

  def getAllTriggersProcess(orgid: String,
                            siteid: String,
                            triggerGetAllRequest: AppRequest): Future[AppSuccessResponse[List[TriggerResponse]]] = {
    log.debug(s"Entered  getAllTriggerProcess with orgid: $orgid and siteid: $siteid")
    cassandraReadAllTriggersTimer
      .time(
        dbLayer
          .getAllTriggers(orgid, siteid)
      )
      .map(x => {
        log.debug("Successfully got the triggers for siteid and orgid")
        val convertedTriggerList: List[TriggerResponse] = x.map(TriggerResponse(_))
        reqResGenerator.successResponseWithoutFuture(triggerGetAllRequest, convertedTriggerList)
      })
  }

  def postTriggerProcess(orgid: String, siteid: String, triggerPostRequest: AppRequest): Future[AppResponse] = {
    log.debug(s"Entered postTriggerProcess with orgid: $orgid and siteid: $siteid ")
    triggerPostRequest.request.triggerprops match {
      case Some(triggerProps) =>
        triggerProps.trigger match {
          case None =>
            log.error("Trigger not found in request")
            reqResGenerator.failurecaseResponse(triggerPostRequest, NOTFOUND.id, Missing_Trigger.value)
          case Some(triggerFromUser) =>
            log.debug(s"triggerFromUser: $triggerFromUser")
            val userid          = triggerPostRequest.request.user.getOrElse("")
            val validationError = TriggerValidator.validateTrigger(triggerFromUser)
            if (validationError.isEmpty) {
              val dbTrigger = DBTrigger(triggerFromUser, orgid, siteid, generateUUID, userid)
              dbLayer
                .triggerExistWithSameName(orgid, siteid, triggerFromUser.triggerName)
                .flatMap(
                  exist =>
                    if (exist) {
                      log.debug(s"Trigger already exist with name ${triggerFromUser.triggerName}")
                      reqResGenerator.failurecaseResponse(triggerPostRequest,
                                                          BADREQUEST.id,
                                                          Trigger_Name_Exist.value + triggerFromUser.triggerName)
                    } else {
                      cassandraPostTriggerTimer
                        .time(
                          dbLayer
                            .storeTrigger(dbTrigger)
                        )
                        .map(
                          a =>
                            if (a.success) {
                              log.debug(s"Successfully persisted trigger in cassandra $dbTrigger")
                              val triggerResponse = TriggerResponse(dbTrigger)
                              reqResGenerator.successResponseWithoutFuture(triggerPostRequest, triggerResponse)
                            } else {
                              log.error(s"Error persisting trigger in cassandra $dbTrigger")
                              reqResGenerator.failurecaseResponseWithoutFuture(triggerPostRequest,
                                                                               NOTFOUND.id,
                                                                               Missing_TriggerId.value)
                          }
                        )
                  }
                )
            } else {
              log.error(s"$validationError for trigger $triggerFromUser")
              reqResGenerator.failurecaseResponse(triggerPostRequest, BADREQUEST.id, validationError)
            }
        }
      case None =>
        log.error(s"Triggerprops is empty in the appRequest $triggerPostRequest")
        reqResGenerator.failurecaseResponse(triggerPostRequest, NOTFOUND.id, Missing_Triggerprops.value)
    }

  }

  def deleteTriggerProcess(orgid: String, siteid: String, triggerDeleteRequest: AppRequest): Future[AppResponse] = {
    log.debug(s"Entered  deleteTriggerProcess with orgid: $orgid and siteid: $siteid")
    triggerDeleteRequest.request.triggerprops match {
      case Some(triggerProps) =>
        triggerProps.triggerId match {
          case None =>
            log.error("triggerId not found in request")
            reqResGenerator.failurecaseResponse(triggerDeleteRequest, NOTFOUND.id, Missing_TriggerId.value)
          case Some(triggerId) =>
            log.debug(s"triggerId: $triggerId")
            cassandraReadTriggerTimer
              .time(
                dbLayer
                  .getTriggerByTriggerId(orgid, siteid, triggerId)
              )
              .flatMap {
                case None =>
                  log.info(s"Could not find trigger in DB with id $triggerId")
                  reqResGenerator.failurecaseResponse(
                    triggerDeleteRequest,
                    NOTFOUND.id,
                    Missing_DBTriggerId.value + triggerId + Does_Not_Exist_OrgSite.value + orgid + Siteid.value + siteid
                  )
                case _ =>
                  cassandraDeletetriggerTimer
                    .time(
                      dbLayer
                        .deleteByTriggerId(triggerId, siteid, orgid)
                    )
                    .map(
                      a =>
                        if (a.success) {
                          log.debug(s"Successfully deleted the trigger by triggerId $triggerId")
                          reqResGenerator.successResponseWithoutFuture(triggerDeleteRequest, a)
                        } else {
                          log.error(s"Error deleting the trigger in cassandra with id $triggerId")
                          reqResGenerator.failurecaseResponseWithoutFuture(
                            triggerDeleteRequest,
                            INTERNALSERVERERROR.id,
                            s"Internal server error while deleting the trigger with id $triggerId"
                          )
                      }
                    )
              }
        }
      case None =>
        log.error(s"Triggerprops is empty in the appRequest $triggerDeleteRequest")
        reqResGenerator.failurecaseResponse(triggerDeleteRequest, NOTFOUND.id, Missing_Triggerprops.value)
    }
  }

  def updateTriggerProcess(orgid: String, siteid: String, triggerUpdateRequest: AppRequest): Future[AppResponse] = {
    log.debug(s"Entered updateTriggerProcess with orgid: $orgid and siteid: $siteid ")
    triggerUpdateRequest.request.triggerprops match {
      case Some(triggerProps) =>
        (triggerProps.triggerId, triggerProps.trigger) match {
          case (None, _) =>
            log.error(s"TriggerId not found in request $triggerUpdateRequest")
            reqResGenerator.failurecaseResponse(triggerUpdateRequest, NOTFOUND.id, Missing_TriggerId.value)
          case (_, None) =>
            log.error(s"Trigger payload not found in request $triggerUpdateRequest")
            reqResGenerator.failurecaseResponse(triggerUpdateRequest, NOTFOUND.id, Missing_Trigger.value)
          case (Some(triggerid), Some(trigger)) =>
            log.debug(s"TriggerId: $triggerid Trigger: $trigger")
            cassandraReadTriggerTimer
              .time(
                dbLayer
                  .getTriggerByTriggerId(orgid, siteid, triggerid)
              )
              .flatMap {
                case None =>
                  log.info(s"Could not find trigger in DB with id $triggerid")
                  reqResGenerator.failurecaseResponse(
                    triggerUpdateRequest,
                    NOTFOUND.id,
                    Missing_DBTriggerId.value + triggerid + Does_Not_Exist_OrgSite.value + orgid + Siteid.value + siteid
                  )
                case Some(dbTrigger) => {
                  updateDBAndGenerateResponse(triggerUpdateRequest, dbTrigger, trigger, triggerid, orgid, siteid)
                }
              }
        }
      case None =>
        log.error(s"Triggerprops is empty in the appRequest $triggerUpdateRequest")
        reqResGenerator.failurecaseResponse(triggerUpdateRequest, NOTFOUND.id, Missing_Triggerprops.value)
    }

  }

  def updateDBAndGenerateResponse(triggerUpdateRequest: AppRequest,
                                  triggerFromDB: DBTrigger,
                                  triggerFromUser: TriggerRequest,
                                  triggerId: String,
                                  orgid: String,
                                  siteid: String): Future[AppResponse] = {
    val validationError = TriggerValidator.validateTrigger(triggerFromUser)
    if (validationError.isEmpty) {
      val updatedTrigger = DBTrigger(triggerFromUser,
                                     orgid,
                                     siteid,
                                     triggerFromDB.triggerid,
                                     triggerFromDB.userid,
                                     Some(triggerFromDB.createdon))
      dbLayer
        .triggerExistWithSameName(orgid, siteid, triggerFromUser.triggerName, Some(triggerFromDB.triggername))
        .flatMap(
          exist =>
            if (exist) {
              log.debug(s"Trigger already exist with name ${triggerFromUser.triggerName}")
              reqResGenerator.failurecaseResponse(triggerUpdateRequest,
                                                  BADREQUEST.id,
                                                  Trigger_Name_Exist.value + triggerFromUser.triggerName)
            } else {
              cassandraUpdateTriggerTimer
                .time(
                  dbLayer
                    .updateByTriggerId(triggerId, updatedTrigger.siteid, updatedTrigger.orgid, updatedTrigger)
                )
                .map(
                  s =>
                    if (s.success) {
                      log.debug("Successfully updated the trigger in cassandra " + updatedTrigger)
                      val triggerResponse = TriggerResponse(updatedTrigger)
                      reqResGenerator.successResponseWithoutFuture(triggerUpdateRequest, triggerResponse)
                    } else {
                      log.debug(s"Failed to updated the trigger with triggerId $triggerId")
                      reqResGenerator.failurecaseResponseWithoutFuture(triggerUpdateRequest,
                                                                       INTERNALSERVERERROR.id,
                                                                       InternalError_UpdateTrigger.value)
                  }
                )
          }
        )
    } else {
      log.error(s"$validationError for trigger $triggerFromUser")
      reqResGenerator.failurecaseResponse(triggerUpdateRequest, BADREQUEST.id, validationError)
    }
  }

  /**
   * Search Business alert by search criteria
   *
   * @param orgid
   * @param siteid
   * @param alertGetByIdRequest
   * @return
   */
  def searchTrigger(orgid: String, siteid: String, alertGetByIdRequest: AppRequest): Future[AppResponse] = {
    log.debug(s"Entered the searchBusinessAlert to get the business Alert with orgid: $orgid and siteid: $siteid")
    alertGetByIdRequest.request.triggerprops match {
      case Some(triggerProps) =>
        triggerProps.search match {
          case Some(search) =>
            log.debug(s"searchcondition: $search")
            validateFilterCondition(search, "eq") match {
              case None =>
                log.error(s"Validation failed for search string $search")
                reqResGenerator.failurecaseResponse(alertGetByIdRequest, NOTFOUND.id, condition_correct.value)
              case Some(filterString) => {
                val searchList = filterString.split(" eq ").toList
                if (isValidKey(searchList.headOption))
                  dbLayer
                    .searchTrigger(orgid, siteid, searchList.head, searchList.last)
                    .map(filteredTriggerList => {
                      log.debug("Successfully got the triggers")
                      reqResGenerator.successResponseWithoutFuture(
                        alertGetByIdRequest,
                        filteredTriggerList.map(businessTrigger => TriggerResponse(businessTrigger))
                      )
                    })
                else {
                  reqResGenerator.failurecaseResponse(alertGetByIdRequest, BADREQUEST.id, invalid_key.value)
                }
              }
            }
          case None =>
            log.error(s"Search payload is missing in the appRequest $alertGetByIdRequest")
            reqResGenerator.failurecaseResponse(alertGetByIdRequest, NOTFOUND.id, Missing_Searchstring.value)
        }

      case None =>
        log.error(s"Triggerprops is empty in the appRequest $alertGetByIdRequest")
        reqResGenerator.failurecaseResponse(alertGetByIdRequest, NOTFOUND.id, Missing_Triggerprops.value)
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
        Set("severity", "triggerName", "triggerCategory").contains(value.trim)
      case None => false
    }

}

object TriggerProcessor {
  def apply(dbLayer: DbLayer, reqResGenerator: ReqResGenerator): TriggerProcessor =
    new TriggerProcessor(dbLayer, reqResGenerator)
}
