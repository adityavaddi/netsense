package com.vz.nsp.parking.grouppolicyservice.service

import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.utils.Logging
import com.vz.nsp.parking.CommonHelper._
import com.vz.nsp.parking.dblayer.DbLayer
import com.vz.nsp.parking.grouppolicyservice.helper.ReqResGenerator._
import com.vz.nsp.parking.model.ResponseMessage._
import com.vz.nsp.parking.model.StatusCodes._
import com.vz.nsp.parking.model.{SuccessMessageWithIds, _}

import scala.concurrent.{ExecutionContext, Future}

class ParkingSpaceService(dbLayer: DbLayer) extends Logging with Instrumented {

  implicit val ec = ExecutionContext.Implicits.global

  //Get all parkingspace details by parkingspaceid
  def getAllParkingSpacesProcess(
      spaceGetByIdRequest: AppRequest
  ): Future[AppResponse] = {
    log.debug(s"Entered getAllParkingSpacesProcess to get the space attributes for parkingspaceids")
    spaceGetByIdRequest.request.parkingspaceids match {
      case Some(parkingspaceids) => {
        log.debug(s"parkingspaceids $parkingspaceids")
        val spaceids = parkingspaceids.toList.filter(_.trim.nonEmpty)
        dbLayer
          .getAllParkingSpaces(spaceids)
          .map(x => {
            log.debug(s"Successfully got the space attributes for the given parkingspaceids")
            successResponseWithoutFuture(spaceGetByIdRequest, spaceAttributesResponse(x))
          })
      }
      case None =>
        failureCaseResponse(spaceGetByIdRequest, BADREQUEST.id, NotFoundParkingSpace.value)
    }
  }

  def deleteParkingSpaceProcess(
      parkingSpaceDeleteRequest: AppRequest
  ): Future[AppResponse] = {
    log.debug("Entered deleteParkingSpaceProcess to delete the parkingspaces by parkingspaceids")
    parkingSpaceDeleteRequest.request.parkingspaceids match {
      case Some(parkingspaceids) => {
        log.debug(s"parkingspaceids $parkingspaceids")
        val spaceids = parkingspaceids.toList.filter(_.trim.nonEmpty)
        if (spaceids.isEmpty)
          failureCaseResponse(
            parkingSpaceDeleteRequest,
            BADREQUEST.id,
            NotFoundParkingSpace.value
          )
        else {
          dbLayer
            .getExistingParkingSpaceIds(spaceids)
            .flatMap(
              existingSpaceIds =>
                if (existingSpaceIds.isEmpty) {
                  failureCaseResponse(parkingSpaceDeleteRequest,
                                      BADREQUEST.id,
                                      NotFoundSpaceId.value + (parkingspaceids mkString ", "))
                } else {
                  deleteExistingSpaces(parkingSpaceDeleteRequest, existingSpaceIds, spaceids)
              }
            )
        }
      }
      case None =>
        failureCaseResponse(parkingSpaceDeleteRequest, BADREQUEST.id, NotFoundParkingSpace.value)
    }
  }

  //Delete the parkingspaces by parkingspaceids

  def deleteExistingSpaces(parkingSpotDeleteRequest: AppRequest,
                           spaceids: List[String],
                           allParkingSpaceIds: List[String]): Future[AppResponse] =
    dbLayer
      .deleteByParkingSpaceId(spaceids)
      .map(
        x =>
          x match {
            case true => {
              log.debug(s"Successfully deleted the space attributes for parkingspaceids $spaceids")
              successResponseWithoutFuture(
                parkingSpotDeleteRequest,
                SuccessMessageWithIds(true, Some(allParkingSpaceIds.filterNot(spaceids.contains(_))))
              )
            }
            case false => {
              log.error(s"Error while deleting the space attributes for parkingspaceids $spaceids")
              failureCaseResponseWithoutFuture(parkingSpotDeleteRequest,
                                               INTERNALSERVERERROR.id,
                                               InternalErrorSeleteSpot.value)
            }
        }
      )

  def updateParkingSpaceProcess(
      parkingSpotGetByIdRequest: AppRequest
  ): Future[AppResponse] = {
    log.debug(s"Entered updateParkingSpaceProcess to create/update the space attributes for the parkingspaceids")
    (parkingSpotGetByIdRequest.request.spaceattributes) match {

      case (Some(spaceAttributes)) => {
        val filteredSpots = validateParkingSpaceList(spaceAttributes)
        if (filteredSpots._1.size == spaceAttributes.size)
          failureCaseResponse(parkingSpotGetByIdRequest, BADREQUEST.id, InvalidParkingSpace.value)
        else
          updateParkingSpaceGenerateResponse(parkingSpotGetByIdRequest, filteredSpots._2, filteredSpots._1)
      }
      case (None) =>
        failureCaseResponse(parkingSpotGetByIdRequest, BADREQUEST.id, NotFoundParkingSpace.value)

    }
  }

  //validating the input and sending the invalid space payload ids and valid payload ids (invalidspaces,validspaces)
  def validateParkingSpaceList(
      parkingSpaceList: List[ParkingSpaceRequest]
  ): (List[(ParkingSpaceRequest, Int)], List[(ParkingSpaceRequest, Int)]) =
    validateAllParkingSpaces(parkingSpaceList).partition(_._2 > 1)

  //calling all input validation methods to check the inputs
  def validateAllParkingSpaces(parkingSpaces: List[ParkingSpaceRequest]): List[(ParkingSpaceRequest, Int)] =
    parkingSpaces.map(
      parkingSpace =>
        if (parkingSpace.parkingspaceid != null && isValidTypeOfVehicle(parkingSpace.typeOfVehicle) && isValidformFactor(
              parkingSpace.parkingSpaceType
            )
            && isValidBusinessUse(parkingSpace.businessUse) && isValidhowMetered(parkingSpace.howMetered) && isValidAreaType(
              parkingSpace.areaType
            ) &&
            isValidGeoCords(parkingSpace.geoCoordinates)) (parkingSpace, 1)
        else (parkingSpace, 2)
    )

  //in this method we will update the space attributes with the data
  def updateParkingSpaceGenerateResponse(spaceGetByIdRequest: AppRequest,
                                         parkingSpaceListFromUser: List[(ParkingSpaceRequest, Int)],
                                         invalidSpaces: List[(ParkingSpaceRequest, Int)]): Future[AppResponse] = {

    val spaceDetailsByIds = dbLayer.getAllParkingSpaces(parkingSpaceListFromUser.map(e => e._1.parkingspaceid))
    val spaceList: List[ParkingSpace] =
      parkingSpaceListFromUser.foldLeft(List[ParkingSpace]()) { (z, f) =>
        z :+ generateParkingSpaceObject(f._1)
      }

    val resultSeq: Future[Vector[SuccessMessageWithSpaceAttributes]] =
      Future.traverse(spaceList.toVector)(space => dbLayer.updateSpaceAttributes(space, spaceDetailsByIds))
    resultSeq
      .recover {
        case ex: Exception => {
          log.error("Error while saving the spaces attributes " + ex.getMessage)
          throw ex
        }
      }
      .map(e => {
        log.debug("successfully updated the spaces with spaces attributes")
        successUpdate(e, spaceGetByIdRequest, invalidSpaces.map(e => e._1.parkingspaceid))
      })
  }

  //If nothing is inserted to db the we are sending error otherwise success
  def successUpdate(stringList: Vector[SuccessMessageWithSpaceAttributes],
                    appRequest: AppRequest,
                    invalidParkingSpaces: List[String]): AppResponse =
    if (countFilteredRecords(stringList, true).isEmpty)
      failureCaseResponseWithoutFuture(appRequest, BADREQUEST.id, InternalErrorStoreSpace.value)
    else {
      val cassandraFails = countFilteredRecords(stringList, false)
      successResponseWithoutFuture(appRequest,
                                   SuccessMessageWithInvalidSpaces(true, Some(cassandraFails ::: invalidParkingSpaces)))
    }

  //Listing successful/failure records
  def countFilteredRecords(stringList: Vector[SuccessMessageWithSpaceAttributes], filterVal: Boolean): List[String] =
    stringList
      .filter(_.success == filterVal)
      .foldLeft(List[String]()) { (z, f) =>
        {
          z :+ f.spaceAttributes.getOrElse("")
        }
      }

}

object ParkingSpaceService {
  def apply(dbLayer: DbLayer): ParkingSpaceService =
    new ParkingSpaceService(dbLayer)
}
