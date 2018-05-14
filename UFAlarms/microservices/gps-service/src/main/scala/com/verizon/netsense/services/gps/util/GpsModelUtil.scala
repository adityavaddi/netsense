package com.verizon.netsense.services.gps.util

import com.verizon.netsense.services.gps.model.{Gps, GpsIsModel}

object GpsModelUtil {

  def populateStringValue(value: Option[Double]): Option[String] = {
    value match {
      case None => None
      case Some(lat) => Some(lat.toString)
    }

  }

  def toGpsIsModel(gps: Option[Gps]) : Option[GpsIsModel] = {
    gps match {
      case None => None
      case Some(gps) => Some(GpsIsModel(gps.nodeid,gps.name,gps.orgid,gps.siteid,populateStringValue(gps.latitude),
      populateStringValue(gps.longitude),populateStringValue(gps.latuseradded)
      ,populateStringValue(gps.lonuseradded),gps.created,gps.updated))
    }
  }

  def toGpsISModelList(gps: List[Gps]) : List[GpsIsModel] = {
    if(gps == Nil) {
      Nil
    } else {
      gps.foldLeft(List[GpsIsModel]())((a,gps) => GpsIsModel(gps.nodeid,gps.name,gps.orgid,gps.siteid,populateStringValue(gps.latitude),
      populateStringValue(gps.longitude),populateStringValue(gps.latuseradded)
      ,populateStringValue(gps.lonuseradded),gps.created,gps.updated)::a)
    }
  }

}
