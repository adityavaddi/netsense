package com.vz.nsp.parking.tagservice.helper

import java.time.Instant
import java.util.{Calendar, UUID}
import com.vz.nsp.parking.model._

import scala.concurrent.{ExecutionContext, Future}

class ReqResGenerator {

  def failurecaseResponse(tagGetByIdRequest: AppRequest, status: Int, error: String)(
      implicit ec: ExecutionContext
  ): Future[AppFailureResponse] =
    Future.successful(generateAppFailureResponse(tagGetByIdRequest, status, error))

  def generateAppFailureResponse(tagGetByIdRequest: AppRequest, status: Int, error: String)(
      implicit ec: ExecutionContext
  ): AppFailureResponse =
    AppFailureResponse(
      tagGetByIdRequest.messageid,
      FailureResponseBody(tagGetByIdRequest.request.requestid, Instant.now.toString, success = false, error, status)
    )

  def failurecaseResponseForIrrelevantRequestType(status: Int, error: String)(
      implicit ec: ExecutionContext
  ): Future[AppFailureResponse] =
    Future.successful(AppFailureResponse("", FailureResponseBody("", Instant.now.toString, success = false, error, status)))

  def successResponse[T](headers: AppRequest,
                         resultBody: T)(implicit ec: ExecutionContext): Future[AppSuccessResponse[T]] =
    Future.successful(
      AppSuccessResponse[T](headers.messageid,
                            ResponseBody(headers.request.requestid, Instant.now.toString, success = true, resultBody))
    )

  def failurecaseResponseWithoutFuture(tagGetByIdRequest: AppRequest, status: Int, error: String)(
      implicit ec: ExecutionContext
  ): AppFailureResponse =
    generateAppFailureResponse(tagGetByIdRequest, status, error)

  def successResponseWithoutFuture[T](headers: AppRequest,
                                      resultBody: T)(implicit ec: ExecutionContext): AppSuccessResponse[T] =
    AppSuccessResponse[T](headers.messageid,
                          ResponseBody(headers.request.requestid, Instant.now.toString, success = true, resultBody))

  def generateTagObject(tagFromUser: TagRequest, orgid: String, siteid: String): Tag =
    Tag(
      uid = UUID.randomUUID().toString,
      name = tagFromUser.name,
      description = tagFromUser.description.getOrElse(""),
      orgid = orgid,
      siteid = siteid,
      createdon = Calendar.getInstance().getTimeInMillis,
      lastupdated = 0,
      isdeleted = false
    )

  def updateTagObject(tagFromUser: TagRequest, tagFromDB: Tag): Tag =
    Tag(
      uid = tagFromDB.uid,
      name = tagFromUser.name,
      description = tagFromUser.description.getOrElse(""),
      orgid = tagFromDB.orgid,
      siteid = tagFromDB.siteid,
      createdon = tagFromDB.createdon,
      lastupdated = Calendar.getInstance().getTimeInMillis,
      isdeleted = false
    )

  def generateTagResponseObject(tag: Tag): TagResponse =
    TagResponse(
      tagId = tag.uid,
      name = tag.name,
      description = tag.description,
      orgId = tag.orgid,
      siteId = tag.siteid,
      createdOn = convertEpochToDate(tag.createdon),
      lastUpdated = convertEpochToDate(tag.lastupdated)
    )

  def getAllTagsResponse(tags: List[Tag]): List[TagResponse] =
    tags.map(tag=>
      TagResponse(
        tagId = tag.uid,
        name = tag.name,
        description = tag.description,
        orgId = tag.orgid,
        siteId = tag.siteid,
        createdOn = convertEpochToDate(tag.createdon),
        lastUpdated = convertEpochToDate(tag.lastupdated)
      ))

  def convertEpochToDate(epoch: Long): String =
    if (epoch == 0) "" else Instant.ofEpochMilli(epoch).toString
}

object ReqResGenerator {
  def apply(): ReqResGenerator = new ReqResGenerator()
}
