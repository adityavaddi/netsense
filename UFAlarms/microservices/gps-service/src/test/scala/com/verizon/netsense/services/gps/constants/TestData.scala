package com.verizon.netsense.services.gps.constants

import java.time.Instant
import java.util.{Calendar, UUID}

import com.verizon.netsense.services.gps.model._

object TestData {

  val orgId = UUID.randomUUID().toString
  val siteId = UUID.randomUUID().toString
  val nodeId = "NSN66666"
  val nodeName = "Node1"
  val lat = 234.4567
  val long = 345.4567
  val latitude_gps = 234.4567
  val longitude_gps = 345.4567
  val gps = Gps(nodeId,Some(nodeName),orgId,siteId,Some(lat),Some(long),Some(latitude_gps),Some(longitude_gps),
    Some(Calendar.getInstance().getTimeInMillis),Some(0))

  val lat_updated = 564.4457
  val long_updated = 845.4237
  val latitude_gps_updated = 564.4457
  val longitude_gps_updated = 845.4237
  val updatedGps = Gps(nodeId,Some(nodeName),orgId,siteId,Some(lat_updated),Some(long_updated),Some(latitude_gps_updated),Some(longitude_gps_updated)
    ,Some(Calendar.getInstance().getTimeInMillis),Some(0))


  val nodeId2 = "NSN2595"
  val node2Name = "Node2"
  val lat_nodeId2 = 764.5973
  val long_nodeId2 = 855.4567
  val latitude_gps_nodeId2 = 764.5973
  val longitude_gps_nodeId2 = 855.4567
  val gps2 = Gps(nodeId2,Some(node2Name),orgId,siteId,Some(lat_nodeId2),Some(long_nodeId2),Some(latitude_gps_nodeId2),Some(longitude_gps_nodeId2),
    Some(Calendar.getInstance().getTimeInMillis),Some(0))


  val orgHierarchy = OrgHierarchy(nodeId,Some(nodeName),orgId,None,siteId,None,None,None,None)
  val orgHierarchy2 = OrgHierarchy(nodeId2,Some(node2Name),orgId,None,siteId,None,None,None,None)


  val gpsEvent = GpsEvent(nodeId,Some(nodeName),lat,long,Calendar.getInstance().getTimeInMillis)

  val gpsEventWithNoOrg = GpsEvent("NSN25951112",Some(nodeName),lat,long,Calendar.getInstance().getTimeInMillis)

  val messageId = UUID.randomUUID().toString
  val responseTopic = "api.reply.interface"
  val requestId = UUID.randomUUID().toString
  val caselType = "GPS"
  val model = "GpsModel"
  val action = "CAN_CREATE"
  val action_read = "CAN_READ"
  val gpsProps = GpsProps(nodeId,Some(nodeName),Some(lat),Some(long))
  val gpsProps2 = GpsProps("NSN0987654",Some(nodeName),Some(lat),Some(long))
  val orgProps = OrgProps(orgId)
  val siteProps = SiteProps(siteId)
  val nodeProps = NodeProps(nodeId)
  val instanceId = UUID.randomUUID().toString()
  val timeStamp = Instant.now().toString

  val unknownOrgProps = OrgProps("Unknown")
  val unknownSitePrps = SiteProps("Unknown")
  val nonExistOrgProps = OrgProps(UUID.randomUUID().toString)
  val nonEsitSiteProps = SiteProps(UUID.randomUUID().toString)
  val wrongCaselType = "differentType"
  val getGpsByNodeIdCasel = "getGpsByNodeId"
  val getGpsByOrgAndSiteIdCasel = "getGpsForOrgIdAndSiteId"

  val validAppRequest = AppRequest(messageId,responseTopic,RequestBody(requestId,caselType,model,action,
    Some(gpsProps),Some(orgProps),Some(siteProps),None,None,instanceId,timeStamp))

  val emptyGpsPropsappRequest = AppRequest(messageId,responseTopic,RequestBody(requestId,caselType,model,action,
    None,Some(orgProps),Some(siteProps),None,None,instanceId,timeStamp))

  val unknownValueOrgSiteappRequest = AppRequest(messageId,responseTopic,RequestBody(requestId,caselType,model,action,
    Some(gpsProps),Some(unknownOrgProps),Some(unknownSitePrps),None,None,instanceId,timeStamp))

  val wrongCaselappRequest = AppRequest(messageId,responseTopic,RequestBody(requestId,wrongCaselType,model,action,
    Some(gpsProps),Some(orgProps),Some(siteProps),None,None,instanceId,timeStamp))

  val nonExisteOrgSiteappRequest = AppRequest(messageId,responseTopic,RequestBody(requestId,wrongCaselType,model,action,
    Some(gpsProps),Some(nonExistOrgProps),Some(nonEsitSiteProps),None,None,instanceId,timeStamp))


  val nonExisteOrgSiteWithDifferentNodeIdappRequest = AppRequest(messageId,responseTopic,RequestBody(requestId,wrongCaselType,model,action,
    Some(gpsProps2),Some(nonExistOrgProps),Some(nonEsitSiteProps),None,None,instanceId,timeStamp))

  val getGpsByNodeIdAppRequest = AppRequest(messageId,responseTopic,RequestBody(requestId,getGpsByNodeIdCasel,model,action_read,
    None,Some(orgProps),Some(siteProps),Some(nodeProps),None,instanceId,timeStamp))


  val getGpsByOrgAndSiteIdAppRequest = AppRequest(messageId,responseTopic,RequestBody(requestId,getGpsByOrgAndSiteIdCasel,model,action_read,
    None,Some(orgProps),Some(siteProps),None,None,instanceId,timeStamp))


  /**
    * Invalid request with missing parameters
    */

  val getGpsByNodeIdAppRequestWithMissingOrgProps = AppRequest(messageId,responseTopic,RequestBody(requestId,getGpsByNodeIdCasel,model,action_read,
    None,None,Some(siteProps),Some(nodeProps),None,instanceId,timeStamp))

  val getGpsByNodeIdAppRequestWithMissingSiteProps = AppRequest(messageId,responseTopic,RequestBody(requestId,getGpsByNodeIdCasel,model,action_read,
    None,Some(orgProps),None,Some(nodeProps),None,instanceId,timeStamp))


  val getGpsByNodeIdAppRequestWithMissingNodeProps = AppRequest(messageId,responseTopic,RequestBody(requestId,getGpsByNodeIdCasel,model,action_read,
    None,Some(orgProps),Some(siteProps),None,None,instanceId,timeStamp))

  val getGpsByNodeIdAppRequestWithManyMissingFields = AppRequest(messageId,responseTopic,RequestBody(requestId,getGpsByNodeIdCasel,model,action_read,
    None,None,None,None,None,instanceId,timeStamp))


  val getGpsByOrgAndSiteIdAppRequesttWithMissingOrgProps = AppRequest(messageId,responseTopic,RequestBody(requestId,getGpsByOrgAndSiteIdCasel,model,action_read,
    None,None,Some(siteProps),None,None,instanceId,timeStamp))

  val getGpsByOrgAndSiteIdAppRequestMissingSiteProps = AppRequest(messageId,responseTopic,RequestBody(requestId,getGpsByOrgAndSiteIdCasel,model,action_read,
    None,Some(orgProps),None,None,None,instanceId,timeStamp))

  val getGpsByOrgAndSiteIdAppRequestWithManyMissingFields = AppRequest(messageId,responseTopic,RequestBody(requestId,getGpsByOrgAndSiteIdCasel,model,action_read,
    None,None,None,None,None,instanceId,timeStamp))

  /**
    * sch Event data
    * {"uuid":"391a2e48-bbb8-4544-8147-75e87a0ec5ff","f":"","a":"UNSOL",
    * "l":{"smax":22.0,"t":1520530599,"fix":3,"sav":18.5,
    * "smin":15.0,"hdop":0.95,"pdop":1.24,"alt":-0.4,
    * "lon":-71.324722345,"nsat":11,"lat":42.614722,"vdop":0.8},
    * "p":"global/UNSOL/gps",
    * "sid":"N06232eb5","d":"2018-03-08T17:36:39Z"}
    */

  val deliveryType = "UNSOL"
  val gpsData = SchGpsData(15.0,1520530599,3,18.5,15.0,0.95,1.24,-0.14,-71.324722345,11,42.614722,0.8)
  val uri = "global/UNSOL/gps"
  val sid = "NSN66666"
  val dateString = "2018-03-08T17:36:39Z"
  val schGpsEvent = SchGpsEvent(UUID.randomUUID().toString,deliveryType,gpsData,uri,sid,dateString)

  val sidWithNoOrgHierarchy = "N91234589"
  val schGpsEventWithNoOrgHierarchy = SchGpsEvent(UUID.randomUUID().toString,deliveryType,gpsData,uri,sidWithNoOrgHierarchy,dateString)


}
