package com.vz.nsp.parking.userdataservice.helper

import java.time.Instant
import java.util.{Calendar, UUID}

import com.vz.nsp.parking.model._

import scala.concurrent.{ExecutionContext, Future}

class ReqResGenerator {

  def failurecaseResponse(userDataGetByIdRequest: AppRequest, status: Int, error: String)(
    implicit ec: ExecutionContext
  ): Future[AppFailureResponse] =
    Future.successful(generateAppFailureResponse(userDataGetByIdRequest, status, error))

  def failurecaseResponseForIrrelevantRequestType(status: Int, error: String)(
    implicit ec: ExecutionContext
  ): Future[AppFailureResponse] =
    Future.successful(AppFailureResponse("", FailureResponseBody("", Instant.now.toString, false, error, status)))

  def successResponse[T](headers: AppRequest,
                         resultBody: T)(implicit ec: ExecutionContext): Future[AppSuccessResponse[T]] =
    Future.successful(
      AppSuccessResponse[T](headers.messageid,
        ResponseBody(headers.request.requestid, Instant.now.toString, true, resultBody))
    )

  def failurecaseResponseWithoutFuture(userDataGetByIdRequest: AppRequest, status: Int, error: String)(
    implicit ec: ExecutionContext
  ): AppFailureResponse =
    generateAppFailureResponse(userDataGetByIdRequest, status, error)

  def generateAppFailureResponse(userDataGetByIdRequest: AppRequest, status: Int, error: String)(
    implicit ec: ExecutionContext
  ): AppFailureResponse =
    AppFailureResponse(
      userDataGetByIdRequest.messageid,
      FailureResponseBody(userDataGetByIdRequest.request.requestid, Instant.now.toString, false, error, status)
    )

  def successResponseWithoutFuture[T](headers: AppRequest,
                                      resultBody: T)(implicit ec: ExecutionContext): AppSuccessResponse[T] =
    AppSuccessResponse[T](headers.messageid,
      ResponseBody(headers.request.requestid, Instant.now.toString, true, resultBody))


  def generateUserDataObject(userDataRequest: AppUserDataProps): UserData =
  UserData(
    userdataid = UUID.randomUUID().toString,
    datavalue = userDataRequest.datavalue.getOrElse(""),
    createdon = Calendar.getInstance().getTimeInMillis,
    lastupdated = 0,
    appid = userDataRequest.appid,
    userid = userDataRequest.userid,
    isdeleted = false
  )

  def updateUserDataObject(userDataFromDB: UserData, userDataFromClient: AppUserDataProps): UserData =
    UserData(
      appid = userDataFromDB.appid,
      userid =  userDataFromDB.userid,
      userdataid = userDataFromDB.userdataid,
      datavalue = userDataFromClient.datavalue.getOrElse(""),
      createdon = userDataFromDB.createdon,
      lastupdated = Calendar.getInstance().getTimeInMillis,
      isdeleted = userDataFromDB.isdeleted
    )
}

object ReqResGenerator {
  def apply(): ReqResGenerator = new ReqResGenerator()
}
