package com.verizon.netsense.services.alert.model

import com.fasterxml.jackson.annotation.JsonProperty
import com.verizon.netsense.entity.Entity

case class Notification(@JsonProperty("notificationid") notificationId: String,
                        @JsonProperty("orgid") orgId: String,
                        active: Boolean = true,
                        @JsonProperty("siteid") siteId: String,
                        name: Option[String] = None,
                        msg: Option[String] = None,
                        description: Option[String] = None,
                        scope: Option[String] = None,
                        window: Option[String] = None,
                        @JsonProperty("hold_off") holdOff: Option[Int] = None,
                        @JsonProperty("resend_interval") resendInterval: Option[Int] = None,
                        @JsonProperty("notificationtype") notificationType: Set[String] = Set.empty[String],
                        severity: Set[String] = Set.empty[String],
                        emailUsersList: Set[String] = Set.empty[String],
                        additionalEmails: Set[String] = Set.empty[String],
                        smsUsersList: Set[String] = Set.empty[String],
                        created: Option[Long] = None,
                        updated: Option[Long] = None)
    extends Entity
