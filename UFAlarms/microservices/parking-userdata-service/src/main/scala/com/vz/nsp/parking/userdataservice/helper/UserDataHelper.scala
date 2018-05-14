package com.vz.nsp.parking.userdataservice.helper

import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.utils.Logging
import com.vz.nsp.parking.dblayer.DbLayer
import com.vz.nsp.parking.model.ResponseMessage._
import com.vz.nsp.parking.model.StatusCodes._
import com.vz.nsp.parking.model._
import nl.grons.metrics.scala.Timer

import scala.concurrent.{ExecutionContext, Future}

class UserDataHelper(dbLayer: DbLayer, reqResGenerator: ReqResGenerator) extends Logging with Instrumented {

  /**
    * USER DATA SERVICE CASSANDRA METRICS
    */
  private[this] val cassandraPostUserdataTimer: Timer = metrics.timer("cassandra-create-userdata")
  private[this] val cassandraUpdateUserdataTimer: Timer = metrics.timer("cassandra-update-userdata")
  private[this] val cassandraDeleteUserdataTimer: Timer = metrics.timer("cassandra-delete-userdata")
  private[this] val cassandraReadUserdataTimer: Timer = metrics.timer("cassandra-read-userdata")
  private[this] val cassandraReadAllUserdataAllTimer: Timer = metrics.timer("cassandra-read-all-userdata")

  def getUserDataProcess(userDataGetByIdRequest: AppRequest)(implicit ec: ExecutionContext): Future[AppResponse] = {
    log.debug("Entered getUserDataProcess to get the appuserdata by userdataid")
    val dataId = userDataGetByIdRequest.request.appuserdataprops
    dataId match {
      case null => reqResGenerator.failurecaseResponse(userDataGetByIdRequest, BADREQUEST.id, NotFound_appuserdataprops.value)
      case dataId =>
        (userDataGetByIdRequest.request.appuserdataprops.appid,
          userDataGetByIdRequest.request.appuserdataprops.userid,
          userDataGetByIdRequest.request.appuserdataprops.userdataid)
        match {
          case (null, _, _) => reqResGenerator.failurecaseResponse(userDataGetByIdRequest, BADREQUEST.id, NotFound_AppId.value)
          case (_, null, _) => reqResGenerator.failurecaseResponse(userDataGetByIdRequest, BADREQUEST.id, NotFound_UserId.value)
          case (_, _, None) => reqResGenerator.failurecaseResponse(userDataGetByIdRequest, BADREQUEST.id, NotFound_UserDataId.value)
          case (_, _, _) => {
            cassandraReadUserdataTimer.time(dbLayer
              .getUserData(userDataGetByIdRequest.request.appuserdataprops.appid, userDataGetByIdRequest.request.appuserdataprops.userid, userDataGetByIdRequest.request.appuserdataprops.userdataid.getOrElse("")))
              .map(_ match {
                case None =>
                  reqResGenerator.failurecaseResponseWithoutFuture(userDataGetByIdRequest,
                    BADREQUEST.id,
                    NotFound_UserData.value + userDataGetByIdRequest.request.appuserdataprops.userdataid.get + Does_Not_Exist_appid.value + userDataGetByIdRequest.request.appuserdataprops.appid + userid.value + userDataGetByIdRequest.request.appuserdataprops.userid)
                case userData => log.debug("Successfully got the appuserdata by userdataid")
                  reqResGenerator.successResponseWithoutFuture(userDataGetByIdRequest, userData.get)
              })
          }
          case default => reqResGenerator.failurecaseResponse(userDataGetByIdRequest, BADREQUEST.id, NotFound_Default.value)
        }
    }
  }

  def updateUserDataProcess(userDataGetByIdRequest: AppRequest)(implicit ec: ExecutionContext): Future[AppResponse] = {
    log.debug("Entered updateUserDataProcess to update the appuserdata by userdataid")
    userDataGetByIdRequest.request.appuserdataprops match {
      case null => reqResGenerator.failurecaseResponse(userDataGetByIdRequest, BADREQUEST.id, NotFound_appuserdataprops.value)
      case userDataRequest =>
        (userDataGetByIdRequest.request.appuserdataprops.appid,
          userDataGetByIdRequest.request.appuserdataprops.userid,
          userDataGetByIdRequest.request.appuserdataprops.datavalue,
          userDataGetByIdRequest.request.appuserdataprops.userdataid) match {

          case (null, _, _, _) => reqResGenerator.failurecaseResponse(userDataGetByIdRequest, BADREQUEST.id, NotFound_AppId.value)
          case (_, null, _, _) => reqResGenerator.failurecaseResponse(userDataGetByIdRequest, BADREQUEST.id, NotFound_UserId.value)
          case (_, _, None, _) => reqResGenerator.failurecaseResponse(userDataGetByIdRequest, BADREQUEST.id, NotFound_DataValue_Template.value)
          case (_, _, _, None) => reqResGenerator.failurecaseResponse(userDataGetByIdRequest, BADREQUEST.id, NotFound_UserDataId.value)
          case (_, _, _, _) => {
            cassandraReadUserdataTimer.time(dbLayer
              .getUserData(userDataGetByIdRequest.request.appuserdataprops.appid, userDataGetByIdRequest.request.appuserdataprops.userid, userDataGetByIdRequest.request.appuserdataprops.userdataid.getOrElse("")))
              .flatMap { data =>
                data match {
                  case None =>
                    Future {
                      reqResGenerator.failurecaseResponseWithoutFuture(userDataGetByIdRequest,
                        BADREQUEST.id,
                        NotFound_UserData.value + userDataGetByIdRequest.request.appuserdataprops.userdataid.get + Does_Not_Exist_appid.value + userDataGetByIdRequest.request.appuserdataprops.appid
                          + userid.value + userDataGetByIdRequest.request.appuserdataprops.userid)
                    }

                  case Some(userData) =>
                    val updatedUserData = reqResGenerator.updateUserDataObject(userData, userDataRequest)
                    cassandraUpdateUserdataTimer.time(dbLayer
                      .updateByUserDataId(userDataGetByIdRequest.request.appuserdataprops.appid, userDataGetByIdRequest.request.appuserdataprops.userid, userDataGetByIdRequest.request.appuserdataprops.userdataid.getOrElse(""), updatedUserData))
                      .map(
                        a =>
                          a.success match {
                            case true => log.debug("Successfully updated appuserdata by userdataid ")
                              reqResGenerator.successResponseWithoutFuture(userDataGetByIdRequest, updatedUserData)
                            case false =>
                              reqResGenerator.failurecaseResponseWithoutFuture(userDataGetByIdRequest, BADREQUEST.id, NotFound_UserId.value)
                          }
                      )
                }
              }
          }
          case default => reqResGenerator.failurecaseResponse(userDataGetByIdRequest, BADREQUEST.id, NotFound_Default.value)
        }
    }
  }


  def getAllUserDataProcess(userDataGetByIdRequest: AppRequest)(implicit ec: ExecutionContext): Future[AppResponse] = {
    log.debug("Entered getAllUserDataProcess to appuserdata by appid and userid")
    userDataGetByIdRequest.request.appuserdataprops match {
      case null => reqResGenerator.failurecaseResponse(userDataGetByIdRequest, BADREQUEST.id, NotFound_appuserdataprops.value)
      case request =>
        (userDataGetByIdRequest.request.appuserdataprops.appid,
          userDataGetByIdRequest.request.appuserdataprops.userid) match {

          case (null, _) => reqResGenerator.failurecaseResponse(userDataGetByIdRequest, BADREQUEST.id, NotFound_AppId.value)
          case (_, null) => reqResGenerator.failurecaseResponse(userDataGetByIdRequest, BADREQUEST.id, NotFound_UserId.value)
          case (_, _) => {
            cassandraReadAllUserdataAllTimer.time(dbLayer.getAllUserData(userDataGetByIdRequest.request.appuserdataprops.appid, userDataGetByIdRequest.request.appuserdataprops.userid))
              .map(list => list.size > 0 match {
                case true => reqResGenerator.successResponseWithoutFuture(userDataGetByIdRequest, list)
                case false => log.debug("Successfully got the appuserdata by orgid and siteid")
                  reqResGenerator.successResponseWithoutFuture(userDataGetByIdRequest, list)
              }
              )
          }
          case default => reqResGenerator.failurecaseResponse(userDataGetByIdRequest, BADREQUEST.id, NotFound_Default.value)
        }
    }
  }

  def deleteUserDataProcess(userDataGetByIdRequest: AppRequest)(implicit ec: ExecutionContext): Future[AppResponse] = {
    log.debug("Entered deleteUserDataProcess to delete appuserdata by userdataid")
    userDataGetByIdRequest.request.appuserdataprops match {
      case null => reqResGenerator.failurecaseResponse(userDataGetByIdRequest, BADREQUEST.id, NotFound_appuserdataprops.value)
      case request =>
        userDataGetByIdRequest.request.appuserdataprops.userdataid match {
          case None => log.error("userdataid missing in the request")
            reqResGenerator.failurecaseResponse(userDataGetByIdRequest, BADREQUEST.id, NotFound_UserDataId.value)
          case Some(userDataId) =>
            cassandraReadUserdataTimer.time(dbLayer
              .getUserData(userDataGetByIdRequest.request.appuserdataprops.appid, userDataGetByIdRequest.request.appuserdataprops.userid, userDataId))
              .flatMap(_ match {
                case None =>
                  reqResGenerator.failurecaseResponse(
                    userDataGetByIdRequest,
                    BADREQUEST.id,
                    NotFound_UserData.value + userDataGetByIdRequest.request.appuserdataprops.userdataid.get + Does_Not_Exist_appid.value + userDataGetByIdRequest.request.appuserdataprops.appid
                      + userid.value + userDataGetByIdRequest.request.appuserdataprops.userid
                  )
                case _ =>
                  cassandraDeleteUserdataTimer.time(dbLayer
                    .deleteByUserDataId(userDataGetByIdRequest.request.appuserdataprops.appid, userDataGetByIdRequest.request.appuserdataprops.userid, userDataId))
                    .map(
                      a =>
                        a.success match {
                          case true => log.debug(s"Successfully deleted the appuserdata by userdataid: $userDataId")
                            reqResGenerator.successResponseWithoutFuture(userDataGetByIdRequest, a)
                          case false =>
                            reqResGenerator.failurecaseResponseWithoutFuture(userDataGetByIdRequest,
                              INTERNALSERVERERROR.id,
                              InternalErrorSeleteSpot.value)
                        }
                    )
              })
        }
    }
  }

  def postUserDataProcess(userDataGetByIdRequest: AppRequest)(implicit ec: ExecutionContext): Future[AppResponse] = {
    log.debug("Entered postUserDataProcess to create appuserdata")
    userDataGetByIdRequest.request.appuserdataprops match {
      case null => reqResGenerator.failurecaseResponse(userDataGetByIdRequest, BADREQUEST.id, NotFound_appuserdataprops.value)
      case userDataRequest =>
        (userDataGetByIdRequest.request.appuserdataprops.appid,
          userDataGetByIdRequest.request.appuserdataprops.userid,
          userDataGetByIdRequest.request.appuserdataprops.datavalue) match {

          case (null, _, _) => reqResGenerator.failurecaseResponse(userDataGetByIdRequest, BADREQUEST.id, NotFound_AppId.value)
          case (_, null, _) => reqResGenerator.failurecaseResponse(userDataGetByIdRequest, BADREQUEST.id, NotFound_UserId.value)
          case (_, _, None) => reqResGenerator.failurecaseResponse(userDataGetByIdRequest, BADREQUEST.id, NotFound_DataValue_Template.value)
          case (_, _, _) => {

            val userData = reqResGenerator.generateUserDataObject(userDataRequest)
            cassandraPostUserdataTimer.time(dbLayer.storeUserData(userData))
              .map(
                a =>
                  a.success match {
                    case true => log.debug("Persisted successfully userData " + userData)
                      reqResGenerator.successResponseWithoutFuture(userDataGetByIdRequest, userData)
                    case false =>
                      reqResGenerator.failurecaseResponseWithoutFuture(userDataGetByIdRequest, BADREQUEST.id, NotFound_UserId.value)
                  }
              )
          }
          case default => reqResGenerator.failurecaseResponse(userDataGetByIdRequest, BADREQUEST.id, NotFound_Default.value)

        }
    }
  }
}

object UserDataHelper {
  def apply(dbLayer: DbLayer, reqResGenerator: ReqResGenerator): UserDataHelper = new UserDataHelper(dbLayer, reqResGenerator)
}
