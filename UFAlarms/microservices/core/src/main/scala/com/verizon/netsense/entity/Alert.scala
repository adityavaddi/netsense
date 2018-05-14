package com.verizon.netsense.entity

case class Alert(alertid: String,
                 name: String,
                 `type`: String,
                 severity: String,
                 nodeid: Option[String],
                 msg: Option[String],
                 sitename: Option[String],
                 nodename: Option[String],
                 orgname: Option[String],
                 siteaddress: Option[String],
                 bssid: Option[String],
                 nodehw: Option[String])
