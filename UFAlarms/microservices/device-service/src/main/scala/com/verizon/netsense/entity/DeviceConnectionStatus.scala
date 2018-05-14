package com.verizon.netsense.entity

import com.outworkers.phantom.dsl.UUID

/**
 * Created by davor on 7/6/17.
 */
case class DeviceConnectionStatus(name: String, nodeId: String, isConnected: Boolean, since: UUID) extends Entity
