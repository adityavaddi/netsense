package com.verizon.netsense.entity

/**
 * Created by davor on 7/14/17.
 */
case class NodeStatus(name: String,
                      nodeId: String,
                      orgId: String,
                      siteId: String,
                      netStat: Int,
                      ligStat: String,
                      senStat: String)
    extends Entity
