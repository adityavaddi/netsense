package com.verizon.netsense.services.eventsimulator.model

import java.util.UUID

import com.fasterxml.jackson.annotation.JsonProperty
import com.verizon.netsense.entity.Entity

/**
 * Created by vermri5 on 7/12/17.
 */
case class Alert(@JsonProperty("alertid") alertId: String,
                 name: String,
                 msg: String,
                 @JsonProperty("nodeid") nodeId: String,
                 @JsonProperty("orgid") orgId: String,
                 severity: String,
                 `type`: String,
                 category: String,
                 created: String,
                 updated: String,
                 @JsonProperty("sitename") siteName: Option[String] = None,
                 @JsonProperty("nodename") nodeName: Option[String] = None,
                 @JsonProperty("orgname") orgName: Option[String] = None,
                 @JsonProperty("siteaddress") siteAddress: Option[String] = None,
                 @JsonProperty("siteid") siteId: String,
                 @JsonProperty("bssid") bssId: Option[String] = None,
                 @JsonProperty("nodehw") nodeHw: Option[String] = None,
                 active: Boolean)
    extends Entity

