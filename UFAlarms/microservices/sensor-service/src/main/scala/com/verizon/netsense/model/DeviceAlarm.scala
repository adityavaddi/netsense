package com.verizon.netsense.model

/**
  * Created by nalamte on 3/25/18.
  */
case class DeviceAlarm(name:String="DeviceAlarm",
                       nodeid:String,
                       alarmType:String,
                       alarmSeverity:String,
                       msg:String)
