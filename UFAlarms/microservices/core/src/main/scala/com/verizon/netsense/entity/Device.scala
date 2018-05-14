package com.verizon.netsense.entity

/**
 * Created by brefsdal on 4/4/17.
 */
case class Device(nodeid: String, siteid: String, orgid: String)

case class DeviceStatus(nodeid: String, status: Boolean) extends Entity
