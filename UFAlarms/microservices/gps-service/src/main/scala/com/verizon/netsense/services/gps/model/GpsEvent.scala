package com.verizon.netsense.services.gps.model

import com.verizon.netsense.entity.Entity

case class GpsEvent(nodeid: String,name: Option[String],lat: Double,lon: Double,epochsecs: Long) extends Entity

case class Gps(nodeid: String, name: Option[String], orgid: String, siteid: String, latitude: Option[Double], longitude: Option[Double],
               latuseradded: Option[Double], lonuseradded: Option[Double], created: Option[Long], updated: Option[Long])

case class SchGpsEvent(uuid: String, a: String, l: SchGpsData, p: String, sid: String, d: String)

case class SchGpsData(smax: Double, t: Long, fix: Int, sav: Double, smin: Double
                      , hdop: Double, pdop: Double, alt: Double, lon: Double, nsat: Double, lat: Double, vdop: Double)

case class GpsIsModel(nodeid: String, name: Option[String], orgid: String, siteid: String, latitude: Option[String], longitude: Option[String],
                      latuseradded: Option[String], lonuseradded: Option[String], created: Option[Long], updated: Option[Long])

/**
  * Below is the full list of variable available in actual GPS Event, our case class took only the needed fields
-- Core Node Gps Event
{ "altAndMisc": 7192641,
  "altitude": 175.60000000000002,
  "ctrlmode": 1,
  "epochsecs": 1517516302,
  "fixtype": 3,
  "gpsver": "undefined",
  "hdop": 3.18,
  "lat": 37.3852233,
  "lon": -121.9323166,
  "name": "GpsSample",
  "nodeid": "N01232eb5",
  "numsatellite": 4,
  "pdop": 3.33,
  "rs": "A",
  "snr": 211715,
  "snrAndMisc": null,
  "snravg": 0,
  "snrmax": 0,
  "snrmin": 0,
  "spare": 0,
  "vdop": 1
 }


-- SCH Node GPS Event
{"uuid":"391a2e48-bbb8-4544-8147-75e87a0ec5ff",
"f":"",
"a":"UNSOL",
"l":{"smax":22.0,
    "t":1520530599,
    "fix":3,
    "sav":18.5,
    "smin":15.0,
    "hdop":0.95,
    "pdop":1.24,
    "alt":-0.4,
    "lon":-121.934006667,
    "nsat":11,
    "lat":37.38453,
    "vdop":0.8
    },
"p":"global/UNSOL/gps",
"sid":"sys_mon_source",
"d":"2018-03-08T17:36:39Z"
}

  */
