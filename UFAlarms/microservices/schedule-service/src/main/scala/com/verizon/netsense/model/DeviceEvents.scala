package com.verizon.netsense.model

import com.verizon.netsense.constants.Constants
import com.verizon.netsense.entity.{Entity, LoginReq}
import com.verizon.netsense.exceptions.CustomExceptions.{NoNodesFoundToSendCommandException, NoNodesFoundToSendScheduleException, UnableToGetDataFromGraph}
import com.verizon.netsense.service.DbLayer

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/**
  * Created by maidapr on 1/24/18.
  */
trait DeviceEvents extends Entity

case class RestPack(a: String, // Action GET, POST, etc
                    p: String, // Topic
                    sid: String, // Nodeid
                    d: String, // Date
                    uuid: String, // UUID
                    f: String,
                    l: Map[String, Object] // LoginReq Payload
                       ) extends DeviceEvents



/**
  * Sample Token Payload
  *{
   "t":"",
   "uuid":"022c60fa-094d-4d97-a590-17ff3c9a78e9",
   "f":"Update",
   "a":"UNSOL",
   "l":{
      "ev":"Update",
      "t":1523554672,
      "rr":"warm",
      "tk":[
         "schedule:3c60ed7d-3b39-4beb-a04a-1b746ec65fda"
      ]
   },
   "p":"global/UNSOL/event/system_info/Update",
   "sid":"N06c020ae",
   "d":"2018-04-12T17:37:52Z"
  }
  *
  * */
case class NodeUpdatePayload(ev: Option[String],
                             t: Long,
                             rr: Option[String],
                             tk: Option[Vector[String]])

case class NodeUpdate(a: String, // Action GET, POST, etc
                      p: String, // Topic
                      sid: Option[String], // Nodeid
                      d: String, // Date
                      uuid: String, // UUID
                      f: String,
                      t: String,
                      l: NodeUpdatePayload) extends Entity


object NodeUpdate {

  def constructLoginRequest(nodeUpdate: NodeUpdate) = {
    RestPack(a = nodeUpdate.a,
      p = nodeUpdate.p,
      sid = nodeUpdate.sid.getOrElse(throw new NoNodesFoundToSendScheduleException(nodeUpdate.toJSON)),
      d = nodeUpdate.d, uuid = nodeUpdate.uuid, f = nodeUpdate.f, l = null)
  }

  def extractTokensFromPayload(nodeUpdate: NodeUpdate): (Option[String], Option[Vector[String]]) =
    (nodeUpdate.sid, nodeUpdate.l.tk)
}

object ResolvedNodeData {

  def resolveNodeFromGraphDb(loginReq: RestPack)(implicit dbLayer: DbLayer, ec: ExecutionContext) = Future {
    dbLayer.getSiteOrgDataForNode(loginReq.sid)
      .headOption.getOrElse(NodeOrgHierarchyData(nodeid = Some(loginReq.sid), scheduleid = Some(Constants.DEFAULT)))
  }

}
