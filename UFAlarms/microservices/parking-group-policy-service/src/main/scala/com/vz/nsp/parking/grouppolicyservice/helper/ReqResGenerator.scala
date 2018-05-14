package com.vz.nsp.parking.grouppolicyservice.helper

import java.time.Instant
import java.util.Calendar

import com.vz.nsp.parking.CommonHelper.convertEpochToDate
import com.vz.nsp.parking.model.{ParkingSpaceResponse, _}

import scala.concurrent.Future

object ReqResGenerator {

  def failureCaseResponse(tagGetByIdRequest: AppRequest, status: Int, error: String): Future[AppFailureResponse] =
    Future.successful(generateAppFailureResponse(tagGetByIdRequest, status, error))

  def failureCaseResponseForIrrelevantRequestType(status: Int, error: String): Future[AppFailureResponse] =
    Future.successful(AppFailureResponse("", FailureResponseBody("", Instant.now.toString, false, error, status)))

  def successResponse[T](headers: AppRequest,
                         resultBody: T): Future[AppSuccessResponse[T]] =
    Future.successful(
      AppSuccessResponse[T](headers.messageid,
                            ResponseBody(headers.request.requestid, Instant.now.toString, true, resultBody))
    )

  def failureCaseResponseWithoutFuture(tagGetByIdRequest: AppRequest, status: Int, error: String): AppFailureResponse =
    generateAppFailureResponse(tagGetByIdRequest, status, error)

  def generateAppFailureResponse(tagGetByIdRequest: AppRequest, status: Int, error: String): AppFailureResponse =
    AppFailureResponse(
      tagGetByIdRequest.messageid,
      FailureResponseBody(tagGetByIdRequest.request.requestid, Instant.now.toString, false, error, status)
    )

  def successResponseWithoutFuture[T](headers: AppRequest,
                                      resultBody: T): AppSuccessResponse[T] =
    AppSuccessResponse[T](headers.messageid,
                          ResponseBody(headers.request.requestid, Instant.now.toString, true, resultBody))

  def generateParkingSpaceObject(parkingSpaceRequest: ParkingSpaceRequest): ParkingSpace =
    ParkingSpace(
      parkingspaceid = parkingSpaceRequest.parkingspaceid,
      name = parkingSpaceRequest.name.getOrElse(""),
      reservation = parkingSpaceRequest.reservation.getOrElse(false),
      handicap = parkingSpaceRequest.handicap.getOrElse(false),
      geoCoordinates = parkingSpaceRequest.geoCoordinates,
      parkingSpaceType = parkingSpaceRequest.parkingSpaceType.getOrElse(""),
      monitoringSensorid = parkingSpaceRequest.monitoringSensorid.getOrElse(""),
      businessUse = parkingSpaceRequest.businessUse.getOrElse(""),
      typeOfVehicle = parkingSpaceRequest.typeOfVehicle.getOrElse(List()),
      howMetered = parkingSpaceRequest.howMetered.getOrElse(""),
      meterid=parkingSpaceRequest.meterid.getOrElse(""),
      active = parkingSpaceRequest.active.getOrElse(false),
      PPV = parkingSpaceRequest.PPV.getOrElse(false),
      areaType = parkingSpaceRequest.areaType.getOrElse(List()),
      paystationid = parkingSpaceRequest.paystationid.getOrElse(""),
      level = parkingSpaceRequest.level.getOrElse(""),
      createdOn = Calendar.getInstance().getTimeInMillis,
      lastUpdated = Calendar.getInstance().getTimeInMillis,
      isDeleted = false
    )

  def spaceAttributesResponse(parkingSpots: List[ParkingSpace]): List[ParkingSpaceResponse] =
    parkingSpots.map(parkingSpot=>
    ParkingSpaceResponse(
      parkingspaceid = parkingSpot.parkingspaceid,
      name = parkingSpot.name,
      reservation = parkingSpot.reservation,
      handicap = parkingSpot.handicap,
      geoCoordinates = parkingSpot.geoCoordinates,
      parkingSpaceType = parkingSpot.parkingSpaceType,
      monitoringSensorid = parkingSpot.monitoringSensorid,
      businessUse = parkingSpot.businessUse,
      typeOfVehicle = parkingSpot.typeOfVehicle,
      howMetered = parkingSpot.howMetered,
      meterid=parkingSpot.meterid,
      active = parkingSpot.active,
      PPV = parkingSpot.PPV,
      areaType = parkingSpot.areaType,
      paystationid = parkingSpot.paystationid,
      level = parkingSpot.level,
      createdOn = convertEpochToDate(parkingSpot.createdOn),
      lastUpdated = convertEpochToDate(parkingSpot.lastUpdated)
    ))
}
