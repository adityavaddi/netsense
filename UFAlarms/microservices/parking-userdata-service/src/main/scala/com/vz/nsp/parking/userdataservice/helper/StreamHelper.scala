package com.vz.nsp.parking.userdataservice.helper

import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.utils.Logging
import com.vz.nsp.parking.model.CaselType._
import com.vz.nsp.parking.model.ResponseMessage._
import com.vz.nsp.parking.model.StatusCodes._
import com.vz.nsp.parking.model.{AppRequest, AppResponse}
import nl.grons.metrics.scala.Timer

import scala.concurrent.{ExecutionContext, Future}

class StreamHelper(userDataHelper: UserDataHelper,reqResGenerator: ReqResGenerator) extends Logging with Instrumented {

  private[this] val ApiPostUserDataTimer: Timer = metrics.timer("api-create-userdata")
  private[this] val ApiUpdateUserDataTimer: Timer = metrics.timer("api-update-userdata-by-id")
  private[this] val ApiDeleteUserDataTimer: Timer = metrics.timer("api-delete-userdata-by-id")
  private[this] val ApiGetUserDataTimer: Timer = metrics.timer("api-get-userdata-by-id")
  private[this] val ApiGetAllUserDataTimer: Timer = metrics.timer("api-get-all-userdata")
  private[this] val nullPointerExceptionTimer: Timer = metrics.timer("userdata-nullpointer-exception")
  private[this] val exceptionTimer: Timer = metrics.timer("userdata-exception")
  private[this] val siteOrgMissingTimer: Timer = metrics.timer("userdata-casel-site-org-missing")
  private[this] val invalidJsonTimer: Timer = metrics.timer("userdata-casel-invalid-json")
  private[this] val invalidTypeTimer: Timer = metrics.timer("userdata-invalid-type")
  private[this] val validationFailedTimer: Timer = metrics.timer("userdata-payload-validation-failure")

  def processRequest(appRequest: AppRequest)(implicit ec: ExecutionContext): Future[AppResponse] = {
    log.debug("###### appRequest " + appRequest)
    appRequest match {
      case AppRequest(null, _, _) => {
        log.error("### message id is null")
        invalidJsonTimer.time(reqResGenerator.failurecaseResponseForIrrelevantRequestType(INTERNALSERVERERROR.id, ISR_WrongRequestJson.value))
      }
      case AppRequest(_, null, _) => {
        log.error("### response topic  is null")
        invalidJsonTimer.time(reqResGenerator.failurecaseResponseForIrrelevantRequestType(INTERNALSERVERERROR.id, ISR_WrongRequestJson.value))
      }
      case AppRequest(_, _, null) => {
        log.error("### request object is null")
        invalidJsonTimer.time(reqResGenerator.failurecaseResponseForIrrelevantRequestType(INTERNALSERVERERROR.id, ISR_WrongRequestJson.value))
      }

      case _ => matchCaselTypeAndProcessRequest(appRequest)
    }
  }

  def matchCaselTypeAndProcessRequest(appRequest: AppRequest)(implicit ec: ExecutionContext): Future[AppResponse] =
    try {
      appRequest.request.`type` match {
        case validationfailed.value =>
          reqResGenerator.failurecaseResponse(appRequest, BADREQUEST.id, appRequest.request.configprops.validationError.getOrElse(""))
        case getUserData.value     => ApiGetUserDataTimer.time(userDataHelper.getUserDataProcess(appRequest))
        case getAllUserData.value => ApiGetAllUserDataTimer.time(userDataHelper.getAllUserDataProcess(appRequest))
        case postUserData.value    => ApiPostUserDataTimer.time(userDataHelper.postUserDataProcess(appRequest))
        case deleteUserData.value  => ApiDeleteUserDataTimer.time(userDataHelper.deleteUserDataProcess(appRequest))
        case updateUserData.value  => ApiUpdateUserDataTimer.time(userDataHelper.updateUserDataProcess(appRequest))
        case _                     => invalidTypeTimer.time(reqResGenerator.failurecaseResponse(appRequest, BADREQUEST.id, IrrelevantType.value))
      }
    } catch {
      case np: NullPointerException => {
        np.printStackTrace()
        log.error("NullPointerException found for request: " + appRequest)
        log.error("Exception: " + np.getMessage)
        nullPointerExceptionTimer.time(reqResGenerator.failurecaseResponse(appRequest, INTERNALSERVERERROR.id, InternalServerError.value))
      }
      case e: Exception => {
        log.error("Exception found in the queryFlow: " + e.getMessage)
        exceptionTimer.time(reqResGenerator.failurecaseResponse(appRequest, INTERNALSERVERERROR.id, InternalServerError.value))
      }
    }

}

object StreamHelper {
  def apply(userDataHelper: UserDataHelper, reqResGenerator: ReqResGenerator): StreamHelper = new StreamHelper(userDataHelper, reqResGenerator)
}
