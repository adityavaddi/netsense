package com.verizon.netsense.util

import com.verizon.netsense.model._

import scala.collection.immutable.HashMap.HashTrieMap
import scala.collection.mutable.ListBuffer

object TrafficUtil extends Logging {

  val trafficEventLookup = Map(("linec" -> "LineCrossingEvent"), ("objent" -> "ObjectEnteringEvent"),
    ("objlev" -> "ObjectLeavingEvent"), ("objdwl" -> "ObjectDwellEvent"))

  def parseKeyForDetails(key: String) : Map[String,String] = {
    val e = key.split("/")
    val nodeid = if (e.length >= 2 ) e(1) else ""
    val eType = if (e.length >= 6 ) e(5) else ""
    val eventType = trafficEventLookup.getOrElse(eType, eType)
    Map(("nodeid" -> nodeid), ("type" -> eventType))
  }

  def updateTrafficMsgKeys(x : Map[Any, Any], parent: String = "") : Map[String,Any] = {
    var res =  Map.empty[String,Any]
    x.asInstanceOf[Map[String, Any]].foreach { k =>
      val lookupKey = if ((k._1 == "c" || k._1 == "uuid" || k._1 == "wp")  ) s"$parent${k._1}" else k._1
      val v = TrafficMetadataMapper.metadataMap.getOrElse(lookupKey,k._1)

      k._2 match {
        case htm: HashTrieMap[_, _] =>
          val r = updateTrafficMsgKeys(htm.asInstanceOf[Map[Any,Any]], s"${k._1}.")
          res = res.+((v,r))
        case m: Map[_,_] =>
          val r = updateTrafficMsgKeys(m.asInstanceOf[Map[Any,Any]], s"${k._1}.")
          res = res.+((v,r))
        case vct: Vector[_]  => {
          var updatedV = new ListBuffer[Any]
          vct.foreach { e =>
            e match {
              case m: Map[_,_] =>
                val r = updateTrafficMsgKeys(m.asInstanceOf[Map[Any,Any]], s"${k._1}.")
                updatedV += r
              case _ =>
                updatedV += e
            }
          }
          res = res.+((v,updatedV))
        }
        case _ => res = res.+((v, k._2))
      }
    }

    // Flatten "h" (header) to top level - Todo: undo once Vision Zero team is ready to deal with "as-is" JSON structure from Device
    val resFlattened = res.flatten {
      case (("h", map : Map[String , Any])) => map
      case ((key, value)) => Map(key -> value)
    }.toMap

    resFlattened
  }

}
