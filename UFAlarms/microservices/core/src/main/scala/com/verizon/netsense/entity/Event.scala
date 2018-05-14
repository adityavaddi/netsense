package com.verizon.netsense.entity

/**
 * Created by brefsdal on 4/7/17.
 */
case class Event(name: String,
                 topic: String,
                 event: String,
                 uuid: String,
                 timestamp: Long,
                 rssi: Option[String],
                 ssid: Option[String],
                 imei: Option[String],
                 deviceid: Option[String]) {}
