package com.verizon.netsense.services.alert.services

import java.time.Instant

import com.verizon.netsense.services.alert.customexceptions._
import com.verizon.netsense.services.alert.database.PhantomConnector
import com.verizon.netsense.services.alert.model.CaselType._
import com.verizon.netsense.services.alert.model._
import com.verizon.netsense.utils.Logging

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class UFAlarmRequestHandler(implicit ec: ExecutionContext) extends PhantomConnector with JsonUtils with Logging {

  val ufAlarmsSuccessResponse = successResponse[List[UfAlarm]]
  val ufAlarmSuccessResponse  = successResponse[UfAlarm]
  val statusResponse          = successResponse[Status]
  val ufAlarmBulkResponse     = successResponse[List[BulkSuccess]]

  def validateUfAlarm(ufAlarm: UfAlarm): UfAlarm =
    if (ufAlarm.mappingId == null || ufAlarm.alarmType == null || ufAlarm.ufName == null
        || ufAlarm.alarmType.isEmpty || ufAlarm.ufName.isEmpty)
      throw new UfAlarmPropsMissingException(s"Invalid UF Alarm Payload: $ufAlarm")
    else if (ufAlarm.ufName.getOrElse("").length > 100)
      throw new UfAlarmPropsMissingException(s"UF Name cannot exceed 100 characters: ${ufAlarm.ufName.getOrElse("")}")
    else
      ufAlarm.copy(created = Some(Instant.now.getEpochSecond), updated = Some(Instant.now.getEpochSecond))

  def getAppResponse(appRequest: AppRequest): Future[AppResponse] = {

    import appRequest.request._
    val mappingId: String = ufalarmprops match {
      case Some(x) => x.mappingId.getOrElse("")
      case None    => ""
    }

    `type` match {
      case CREATE_UF_ALARM.value =>
        val ufAlarmPayload = Try {
          ufalarmprops.get.toUfAlarm
        }.toEither match {
          case Left(ex) => throw ex
          case Right(u) => u
        }
        createUfAlarm(ufAlarmPayload).map {
          case Some(ufAlarm) => ufAlarmSuccessResponse(ufAlarm, appRequest)
          case None          => failureResponse(s"Unable to create UF Alarm: $ufalarmprops", appRequest)
        }
      case CREATE_BULK_UF_ALARMS.value =>
        val ufAlarmList: List[UfAlarm] =
          ufalarmprops.get.ufAlarmsList
            .getOrElse(throw new UfAlarmPropsMissingException)
            .toList
            .map(validateUfAlarm)

        createBulkUfAlarms(ufAlarmList).map {
          ufAlarmBulkResponse(_, appRequest)
        }

      case UPDATE_UF_ALARM.value =>
        val ufAlarmPayload = Try {
          ufalarmprops.get.updatedUfAlarm
        }.toEither match {
          case Left(ex) => throw ex
          case Right(u) => u
        }
        updateUfAlarm(ufAlarmPayload).map {
          case Some(ufAlarm) => ufAlarmSuccessResponse(ufAlarm, appRequest)
          case None          => failureResponse(s"Unable to update UF Alarm: $ufAlarmPayload", appRequest)
        }
      case GET_UF_ALARM.value =>
        getUfAlarm(mappingId).map {
          case Some(ufAlarm) => ufAlarmSuccessResponse(ufAlarm, appRequest)
          case None          => failureResponse(s"No UF Alarms found for $mappingId", appRequest)
        }
      case GET_ALL_UF_ALARMS.value =>
        getAllUfAlarms.map {
          ufAlarmsSuccessResponse(_, appRequest)
        }
      case DELETE_UF_ALARM.value =>
        deleteUfAlarm(mappingId).map {
          case true  => statusResponse(Status(true), appRequest)
          case false => failureResponse(s"UF Alarm with $mappingId not found", appRequest)
        }
      case RESET_UF_ALARM.value =>
        resetUfAlarm(mappingId).map {
          case Some(ufAlarm) => ufAlarmSuccessResponse(ufAlarm, appRequest)
          case None          => failureResponse(s"UF Alarm with $mappingId not found", appRequest)
        }
    }
  }

  def createUfAlarm(ufAlarm: UfAlarm): Future[Option[UfAlarm]] = {
    log.debug("Create UF Alarm for " + ufAlarm.toJSON)
    get_UfAlarmByType(ufAlarm.alarmType).flatMap {
      case Some(record) =>
        throw new AlarmTypeExistsException(s"Alarm Type ${ufAlarm.alarmType} already exists")
      case None =>
        checkIfUfNameExists(ufAlarm).recover { case ex: Exception => throw ex }.flatMap { result =>
          if (result)
            create_UfAlarm(ufAlarm)
          else Future(None)
        }
    }
  }

  def createBulkUfAlarms(ufAlarmList: List[UfAlarm]): Future[List[BulkSuccess]] = {
    log.debug("Create Bulk UF Alarms: " + ufAlarmList.toString)

    checkIfUfNameExists(ufAlarmList).recover { case ex: Exception => throw ex }.flatMap { result =>
      if (result) {
        Future.traverse(ufAlarmList) { ufAlarm =>
          create_UfAlarm(ufAlarm).map {
            case Some(alarm) => BulkSuccess(true, alarm)
            case None        => BulkSuccess(false, ufAlarm)
          }
        }
      } else throw new AlarmTypeExistsException()
    }
  }

  def updateUfAlarm(ufAlarm: UfAlarm): Future[Option[UfAlarm]] = {
    log.debug("Update UF Alarm for " + ufAlarm.toJSON)
    getUfAlarm(ufAlarm.mappingId).flatMap {
      case Some(record) =>
        val ufNameCheck = ufAlarm.ufName.getOrElse("").equalsIgnoreCase(record.ufName.getOrElse(""))
        if (ufNameCheck)
          update_UfAlarm(ufAlarm.copy(alarmType = record.alarmType, created = record.created))
        else {
          checkIfUfNameExists(ufAlarm).recover { case ex: Exception => throw ex }.flatMap { result =>
            if (result) {
              update_UfAlarm(ufAlarm.copy(alarmType = record.alarmType, created = record.created))
            } else Future(None)
          }
        }
      case None => Future(None)
    }
  }

  def getUfAlarm(mappingId: String): Future[Option[UfAlarm]] = {
    log.debug("Get UF Alarm " + mappingId)
    get_UfAlarmById(mappingId)
  }

  def getAllUfAlarms: Future[List[UfAlarm]] = {
    log.debug("Get All UF Alarms ")
    get_AllUfAlarms
  }

  def deleteUfAlarm(mappingId: String): Future[Boolean] = {
    log.debug("Deleting UF Alarm " + mappingId)
    delete_UfAlarm(mappingId)
  }

  def resetUfAlarm(mappingId: String): Future[Option[UfAlarm]] = {
    log.debug("Reset UF Alarm " + mappingId)
    getUfAlarm(mappingId).flatMap {
      case Some(ufAlarm) =>
        val updatedUfAlarm = ufAlarm.copy(
          nodeModels = Set(),
          ufName = Some(ufAlarm.alarmType),
          description = None,
          displayToPartner = false,
          displayToCustomer = false,
          updated = Some(Instant.now.getEpochSecond)
        )
        update_UfAlarm(updatedUfAlarm)
      case None => Future(None)
    }
  }

}

object UFAlarmRequestHandler {
  def apply(implicit ec: ExecutionContext): UFAlarmRequestHandler =
    new UFAlarmRequestHandler()(ec)
}
