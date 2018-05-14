package com.verizon.netsense.model

import java.util.Calendar

import com.fasterxml.jackson.annotation.JsonIgnoreProperties
import com.verizon.netsense.entity.Entity
import com.verizon.netsense.service.SensorHistoryQueryService.nodeModelToTypeMap
import com.verizon.netsense.utils.{DeviceModel, Logging}
import com.verizon.netsense.utils.DeviceModels.Model

/**
 * Created by maidapr on 6/29/17.
 */
sealed trait SensorRequestToDevice extends Entity

case class SensorSampleCoreRequest(nodeid: Array[String],
                                   name: String = "SensorSampleReq",
                                   sensor: String) extends SensorRequestToDevice with Entity

@JsonIgnoreProperties(ignoreUnknown = true)
case class SensorSampleRequest(uuid: String,
                               sid: String,
                               a: String,
                               f: String,
                               p: String,
                               d: String = Calendar.getInstance().toInstant.toString,
                               l: Array[Byte])
    extends Entity with SensorRequestToDevice


case class SensorSampleProps(s: String,
                             t: Long = System.currentTimeMillis() * 1000,
                             `?u`: Boolean = true,
                             `?d`: Boolean = false)
    extends Entity

case class EmptyRequest(nodeId: String = "") extends Entity with SensorRequestToDevice


object DeviceTypeEnum extends Enumeration {
  type DeviceTypeEnum = Value
  val PUBSUB_NODE = Value("pubsub_node") //video node
  val WEBSOCKET_NODE = Value("websocket_node") //core node
}

case class NodeModelTimeZone(model: Option[String], timeZone: Option[String], nodeType: Option[DeviceTypeEnum.DeviceTypeEnum] = None)

case class QueryRequestBinded(sensorQueryEnvelope: SensorQueryEnvelope, nodeTypeTimeZone: NodeModelTimeZone)

object QueryRequestBinded extends Logging {

  /**
    * function handles the node missing node model from GraphDb
    * returns None when model is out of scope rather Exception.
    *
    * @version 3.0.6
    * */
  private def checkDeviceModel(model: String): Option[Model] = try {
    Some(DeviceModel.apply(model))
  } catch {
    case ex: Exception =>
      log.warn(s"Node model in query not in scope of defined models $model; proceeding further")
      None
  }

  def bindQueryWithGraphResult(sensorQueryEnvelope: SensorQueryEnvelope, nodeTypeTimeZone: NodeModelTimeZone): QueryRequestBinded = {

    val deviceModelFromEnum = nodeTypeTimeZone.model match {
      case Some(nodeModel) => checkDeviceModel(nodeModel)
      case _ => None
    }

    val deviceTypeEnum = deviceModelFromEnum match {
      case Some(enum) => nodeModelToTypeMap.get(enum)
      case _ => None
    }
    QueryRequestBinded(sensorQueryEnvelope, nodeTypeTimeZone.copy(nodeType = deviceTypeEnum))
  }
}



