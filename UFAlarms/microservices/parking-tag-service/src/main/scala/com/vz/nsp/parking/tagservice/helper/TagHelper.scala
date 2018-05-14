package com.vz.nsp.parking.tagservice.helper

import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.utils.Logging
import com.vz.nsp.parking.dblayer.DbLayer
import com.vz.nsp.parking.model.ResponseMessage._
import com.vz.nsp.parking.model.StatusCodes._
import com.vz.nsp.parking.model._
import nl.grons.metrics.scala.Timer

import scala.concurrent.{ExecutionContext, Future}

class TagHelper(dbLayer: DbLayer, reqResGenerator: ReqResGenerator) extends Logging with Instrumented {

  /**
    * TAG SERVICE CASSANDRA METRICS
    */
  private[this] val cassandraPostTagTimer: Timer = metrics.timer( "cassandra-create-tag" )
  private[this] val cassandraUpdateTagTimer: Timer = metrics.timer( "cassandra-update-tag" )
  private[this] val cassandraDeleteTagTimer: Timer = metrics.timer( "cassandra-delete-tag" )
  private[this] val cassandraReadTagTimer: Timer = metrics.timer( "cassandra-read-tag" )
  private[this] val cassandraReadAllTagsTimer: Timer = metrics.timer( "cassandra-read-all-tags" )
  private[this] val cassandraReadPoliciesOnlyTagidTimer: Timer = metrics.timer( "cassandra-read-policies-onlytagid" )
  implicit val ec: ExecutionContext = ExecutionContext.Implicits.global

  def getParkingTagProcess(orgid: String,
                           siteid: String, requestType: String, tagGetByIdRequest: AppRequest): Future[AppResponse] = {
    log.info( s"Entered the getParkingTagProcess to get the tag with orgid: $orgid and siteid: $siteid and requestType: $requestType" )
    tagGetByIdRequest.request.configprops.tagid match {
      case None =>
        log.error( s"tagid not found in request and messageid: ${tagGetByIdRequest.messageid}" )
        reqResGenerator.failurecaseResponse( tagGetByIdRequest, BADREQUEST.id, NotFound_PolicyCatId.value )
      case Some( tagid ) =>
        log.debug( s"tagid: $tagid" )
        cassandraReadTagTimer.time( dbLayer
          .getTag( orgid, siteid, tagid, requestType = requestType ) )
          .map {
            case Some( tag ) => log.info( "Successfully got the tag by tagid " + tag )
              reqResGenerator.successResponseWithoutFuture( tagGetByIdRequest, reqResGenerator.generateTagResponseObject( tag ) )
            case None =>
              reqResGenerator.failurecaseResponseWithoutFuture( tagGetByIdRequest,
                BADREQUEST.id,
                NotFound_TagId.value + tagid + Does_Not_Exist_OrgSite.value + orgid + Siteid.value + siteid )
          }
    }
  }

  def getAllParkingTagProcess(orgid: String,
                              siteid: String,
                              requestType: String,
                              tagGetByIdRequest: AppRequest
                             ): Future[AppSuccessResponse[List[TagResponse]]] = {
    log.info( s"Entered  getAllParkingTagProcess with orgid: $orgid and siteid: $siteid and requestType: $requestType" )
    cassandraReadAllTagsTimer.time( dbLayer
      .getAllTag( orgid, siteid, requestType ) )
      .map( tags => {
        log.info( "Successfully got the tags for siteid and orgid" )
        reqResGenerator.successResponseWithoutFuture( tagGetByIdRequest, reqResGenerator.getAllTagsResponse( tags ) )
      } )
  }

  def deleteParkingTagProcess(orgid: String,
                              siteid: String, requestType: String, tagGetByIdRequest: AppRequest): Future[AppResponse] = {
    log.info( s"Entered  deleteParkingTagProcess with orgid: $orgid and siteid: $siteid and requestType: $requestType" )
    tagGetByIdRequest.request.configprops.tagid match {
      case None =>
        log.error( s"tagid not found in request and messageid: ${tagGetByIdRequest.messageid}" )
        reqResGenerator.failurecaseResponse( tagGetByIdRequest, BADREQUEST.id, NotFound_PolicyCatId.value )
      case Some( tagId ) =>
        log.debug( s"tagId: $tagId" )
        cassandraReadTagTimer.time( dbLayer
          .getTag( orgid, siteid, tagId, requestType ) )
          .flatMap {
            case Some( _ ) => deleteTagIfItIsNotAssignedToPolicy( tagId, requestType, tagGetByIdRequest, orgid, siteid )
            case None =>
              reqResGenerator.failurecaseResponse( tagGetByIdRequest,
                BADREQUEST.id,
                NotFound_TagId.value + tagId + Does_Not_Exist_OrgSite.value + orgid + Siteid.value + siteid )
          }
    }
  }

  def deleteTagIfItIsNotAssignedToPolicy(tagId: String, requestType: String, tagGetByIdRequest: AppRequest, orgid: String, siteid: String): Future[AppResponse] =
    cassandraReadPoliciesOnlyTagidTimer.time( dbLayer
      .getAllPoliciesByTagid( orgid,siteid,tagId ,requestType) )
      .flatMap( e => if (e.nonEmpty)
        reqResGenerator.failurecaseResponse( tagGetByIdRequest, BADREQUEST.id, Tag_Assigned.value )
      else
        cassandraDeleteTagTimer.time( dbLayer
          .deleteByTagId( tagId, requestType, orgid, siteid ) )
          .map {
            case a@true => log.info( "Successfully deleted the tag by tagid" )
              reqResGenerator.successResponseWithoutFuture( tagGetByIdRequest, SuccessMessage( a ) )
            case false =>
              reqResGenerator.failurecaseResponseWithoutFuture( tagGetByIdRequest,
                INTERNALSERVERERROR.id,
                error = InternalError_DeleteTag.value )
          }
        //}
      )


  def postParkingTagProcess(orgid: String,
                            siteid: String, requestType: String, tagGetByIdRequest: AppRequest): Future[AppResponse] = {
    log.info( s"Entered postParkinTagProcess with orgid: $orgid and siteid: $siteid and requestType: $requestType" )
    tagGetByIdRequest.request.configprops.tag match {
      case None =>
        log.error( s"tag not found in request and messageid: ${tagGetByIdRequest.messageid}" )
        reqResGenerator.failurecaseResponse( tagGetByIdRequest, BADREQUEST.id, NotFound_PolicyCatId.value )
      case Some( policytagFromUser ) =>
        log.debug( s"policytagFromUser: $policytagFromUser" )
        val policyTag = reqResGenerator.generateTagObject( tagFromUser = policytagFromUser,
          orgid = tagGetByIdRequest.request.orgprops.orgid,
          siteid = tagGetByIdRequest.request.siteprops.siteid )
        dbLayer.tagExistWithSameName( orgid, siteid, policyTag.name, requestType ).flatMap( exist => if (exist) {
          log.info( s"Tag already exist with name ${policyTag.name}" )
          reqResGenerator.failurecaseResponse( tagGetByIdRequest, BADREQUEST.id,
            TagAlreadyExists.value + policyTag.name )
        }
        else {
          cassandraPostTagTimer.time( dbLayer
            .storeTag( policyTag, requestType ) )
            .map( if (_) {
              log.info( "Successfully persisted the tag in cassandra " + policyTag )
              reqResGenerator.successResponseWithoutFuture( tagGetByIdRequest, reqResGenerator.generateTagResponseObject( policyTag ) )
            }
            else
              reqResGenerator.failurecaseResponseWithoutFuture( tagGetByIdRequest, BADREQUEST.id, NotFound_PolicyCatId.value )

            )
        } )
    }
  }

  def updateParkingTagProcess(orgid: String,
                              siteid: String, requestType: String, tagGetByIdRequest: AppRequest): Future[AppResponse] = {
    log.info( s"Entered updateParkingTagProcess with orgid: $orgid and siteid: $siteid and requestType: $requestType" )
    (tagGetByIdRequest.request.configprops.tagid, tagGetByIdRequest.request.configprops.tag) match {
      case (None, _) =>
        log.error( s"tagid not found in request and messageid: ${tagGetByIdRequest.messageid}" )
        reqResGenerator.failurecaseResponse( tagGetByIdRequest, BADREQUEST.id, NotFound_PolicyCatId.value )
      case (_, None) =>
        log.error( s"tag not found in request and messageid: ${tagGetByIdRequest.messageid}" )
        reqResGenerator.failurecaseResponse( tagGetByIdRequest, BADREQUEST.id, NotFound_PolicyCat.value )
      case (Some( policycatId ), Some( tag )) =>
        log.debug( s"tagid: $policycatId tag: $tag" )
        cassandraReadTagTimer.time( dbLayer
          .getTag( orgid, siteid, policycatId, requestType ) ).flatMap {
          case Some( responseTag ) => updateDBAndGenerateResponse( tagGetByIdRequest, responseTag, tag, policycatId, requestType )
          case None =>
            reqResGenerator.failurecaseResponse( tagGetByIdRequest,
              BADREQUEST.id,
              NotFound_TagId.value + policycatId + Does_Not_Exist_OrgSite.value + orgid + Siteid.value + siteid )
        }


    }
  }

  def updateDBAndGenerateResponse(tagGetByIdRequest: AppRequest,
                                  tagFromDB: Tag,
                                  tagFromUser: TagRequest,
                                  tagId: String, requestType: String): Future[AppResponse] = {

    val updatedpolicy = reqResGenerator.updateTagObject( tagFromUser, tagFromDB )

    dbLayer.tagExistWithSameName( tagFromDB.orgid, tagFromDB.siteid, tagFromUser.name, requestType, Some( tagFromDB.name ) )
      .flatMap( exist => if (exist) {
        log.info( s"Tag already exist with name ${tagFromDB.name}" )
        reqResGenerator.failurecaseResponse( tagGetByIdRequest, BADREQUEST.id,
          TagAlreadyExists.value + tagFromUser.name )
      }
      else {
        cassandraUpdateTagTimer.time( dbLayer
          .updateByTagId( tagId, updatedpolicy, requestType ) )
          .map( if (_) {
            log.info( "Successfully updated the tag in cassandra " + updatedpolicy )
            reqResGenerator.successResponseWithoutFuture( tagGetByIdRequest, reqResGenerator.generateTagResponseObject( updatedpolicy ) )
          }
          else
            reqResGenerator.failurecaseResponseWithoutFuture( tagGetByIdRequest,
              INTERNALSERVERERROR.id,
              InternalError_UpdateTag.value )
          )

      } )

  }
}

object TagHelper {
  def apply(dbLayer: DbLayer, reqResGenerator: ReqResGenerator): TagHelper = new TagHelper(dbLayer, reqResGenerator)
}
