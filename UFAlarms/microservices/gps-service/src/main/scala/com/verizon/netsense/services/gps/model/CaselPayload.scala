package com.verizon.netsense.services.gps.model

import com.fasterxml.jackson.annotation.JsonProperty
import com.verizon.netsense.entity.Entity

import scala.concurrent.Future


case class NodeProps(@JsonProperty("nodeid") nodeId: String)

case class SiteProps(@JsonProperty("siteid") siteId: String)

case class OrgProps(@JsonProperty("orgid") orgId: String)

case class GpsProps(nodeid: String,
                    name: Option[String],
                    latitude: Option[Double],
                    longitude: Option[Double]
                   ) {
  require(nodeid!= null , "Required Field NodeId is missing")
}

case class AppRequest(messageid: String, responsetopic: String, @JsonProperty request: RequestBody) extends Entity

case class RequestBody(requestid: String,
                       `type`: String,
                       model: String,
                       action: String,
                       @JsonProperty gpsprops: Option[GpsProps] = None,
                       @JsonProperty orgprops: Option[OrgProps] = None,
                       @JsonProperty siteprops: Option[SiteProps] = None,
                       @JsonProperty nodeprops: Option[NodeProps] = None,
                       transactionid: Option[String] = None,
                       instanceid: String,
                       timestamp: String)

sealed trait AppResponse extends Entity {
  def future(): Future[AppResponse] = Future.successful(this)
}

case class AppSuccessResponse[T](messageid: String, @JsonProperty response: SuccessResponseBody[T])
    extends AppResponse {}

trait ResponseBody {
  @JsonProperty("requestid")
  val requestId: String
  @JsonProperty("timestamp")
  val timeStamp: String
  val success: Boolean
}

case class Status(success: Boolean)

case class SuccessResponseBody[T](private val _requestid: String,
                                  private val _timestamp: String,
                                  private val _success: Boolean,
                                  @JsonProperty result: T)
    extends ResponseBody {
  override val requestId: String = _requestid
  override val timeStamp: String = _timestamp
  override val success: Boolean  = _success
}

case class FailureResponseBody(private val _requestid: String,
                               private val _timestamp: String,
                               private val _success: Boolean,
                               error: String,
                               status: Int)
    extends ResponseBody {
  override val requestId: String = _requestid
  override val timeStamp: String = _timestamp
  override val success: Boolean  = _success
}

case class AppFailureResponse(messageid: String, response: FailureResponseBody) extends AppResponse {}

case class MessageId(messageid: String)

sealed case class CaselType(value: String)

object CaselType {

  object GPS extends CaselType("GPS")

  object GET_GPS_BY_NODE_ID extends CaselType("getGpsByNodeId")

  object GET_ALL_GPS_FOR_ORG_ID_AND_SITE_ID extends CaselType("getGpsForOrgIdAndSiteId")

  object DELETE_GPS extends CaselType("deleteGps")


  lazy val allRequestTypes = Set(
    GET_GPS_BY_NODE_ID.value,
    GET_ALL_GPS_FOR_ORG_ID_AND_SITE_ID.value,
    DELETE_GPS.value
  )

}

object StatusCodes extends Enumeration {

  type StatusCodes = Value

  val BADREQUEST          = Value(400)
  val INTERNALSERVERERROR = Value(500)

}
