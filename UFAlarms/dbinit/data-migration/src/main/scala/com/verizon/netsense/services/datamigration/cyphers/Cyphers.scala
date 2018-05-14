package com.verizon.netsense.services.datamigration.cyphers

import java.time.Instant

import com.verizon.netsense.services.datamigration.utils.Configuration.{alerts, gps, keyspace, notifications}
import scala.collection.JavaConverters._
import scala.collection.Map

object Cyphers {

  val allNotificationsCypher = "MATCH " +
    "(o: Org) -[:HAS] -> " +
    "(s:Site)-[:HAS]->" +
    "(n:Notification)" +
    "RETURN n.notificationid as notificationid, " +
    "n.hold_off as hold_off, " +
    "n.window as window, " +
    "n.name as name, " +
    "n.smsUsersList as smsUsersList, " +
    "n.notificationtype as notificationtype, " +
    "n.additionalEmails as additionalEmails, " +
    "n.resend_interval as resend_interval, " +
    "n.description as description, " +
    "n.scope as scope, " +
    "n.updated as updated, " +
    "s.siteid as siteid, " +
    "n.msg as msg, " +
    "n.severity as severity," +
    "n.emailUsersList as emailUsersList, " +
    "n.created as created, " +
    "n.active as active, " +
    "o.orgid as orgid"

  val allAlertsCypher = "MATCH " +
    "(o: Org) -[:HAS] -> " +
    "(s:Site)-[:HAS]->" +
    "(a:Alert)" +
    "RETURN a.alertid as alertid, " +
    "a.name as name, " +
    "a.type as alarmtype, " +
    "a.severity as severity, " +
    "a.nodeid as nodeid, " +
    "a.msg as msg, " +
    "a:Active as active, " +
    "a.created as created, " +
    "a.updated as updated, " +
    "s.siteid as siteid, " +
    "o.orgid as orgid"

  val gpsCypher = "MATCH " +
    "(o: Org) -[:HAS] -> " +
    "(s:Site)-[:HAS]->" +
    "(n:Node)" +
    "RETURN n.nodeid as nodeid, " +
    "n.latitude_gps as latitude, " +
    "n.longitude_gps as longitude, " +
    "n.latitude as latUserAdded, " +
    "n.longitude as lonUserAdded, " +
    "o.orgid as orgid, " +
    "s.siteid as siteid, " +
    "s.created as created, " +
    "s.updated as updated "

  lazy val getCategory: scala.Predef.Map[String, String] =
    scala.Predef
      .Map(
        "Disconnect"             -> "Network",
        "CommFail"               -> "Hardware",
        "SimFail"                -> "Hardware",
        "NotTested"              -> "Software",
        "DownrevSoftware"        -> "Software",
        "BadSensorData"          -> "InternalSensor",
        "ConfigFail"             -> "Software",
        "DegradedNetwork"        -> "Network",
        "SoftwareUpdateFail"     -> "Software",
        "ScheduleFail"           -> "Software",
        "PreRuninFail"           -> "Software",
        "PostRuninFail"          -> "Software",
        "USPFail"                -> "Hardware",
        "PMACFail"               -> "Hardware",
        "DriverFail"             -> "Hardware",
        "FarmUSPFail"            -> "Hardware",
        "SensorFail"             -> "InternalSensor",
        "StrangeReboot"          -> "Software",
        "Assert"                 -> "Software",
        "X509ClientFail"         -> "Software",
        "X509ServerFail"         -> "Software",
        "UnderPower"             -> "Power",
        "OverPower"              -> "Power",
        "HardFault"              -> "Hardware",
        "HWFail_generic"         -> "Hardware",
        "HWFail_HIH6131"         -> "InternalSensor",
        "HWFail_ISL29023"        -> "InternalSensor",
        "HWFail_SE95"            -> "InternalSensor",
        "HWFail_ZMotion"         -> "InternalSensor",
        "HWFail_MMA8451"         -> "InternalSensor",
        "HWFail_TSC3414"         -> "InternalSensor",
        "HWFail_UrbanUSP"        -> "InternalSensor",
        "HWFail_RTC"             -> "InternalSensor",
        "HWFail_EEPROM"          -> "InternalSensor",
        "HWFail_NIGHTHAWK"       -> "InternalSensor",
        "SWUpdateFail_SENSORPOD" -> "InternalSensor",
        "HWFail_STUCK_RELAY"     -> "InternalSensor",
        "HWFail_PCT2075"         -> "InternalSensor",
        "HWFAIL_SIHAWK"          -> "InternalSensor",
        "HWFAIL_GPS"             -> "InternalSensor",
        "HWFail_PodBus"          -> "InternalSensor",
        "Epic_Fail"              -> "Software"
      )
      .withDefaultValue("Lighting")

  def notificationsQuery(row: Map[String, Any]): String = {

    //Column values
    val holdOff          = processInt(row.get("hold_off").getOrElse(0))
    val window           = processString(row.get("window").getOrElse(""))
    val name             = processString(row.get("name").getOrElse("DeviceAlarm"))
    val smsUsersList     = processSet(row.get("smsUsersList").getOrElse(Set.empty))
    val notificationtype = processSet(row.get("notificationtype").getOrElse(Set.empty))
    val additionalEmails = processSet(row.get("additionalEmails").getOrElse(Set.empty))
    val resend_interval  = processInt(row.get("resend_interval").getOrElse(0))
    val description      = processString(row.get("description").getOrElse(""))
    val formattedDesc    = description.replace("'", "").replace("\r\n", "").replace("\n\r", "").replace("\n", "")
    val scope            = processString(row.get("scope").getOrElse(""))
    val updated          = processLong(row.get("updated").getOrElse(Instant.now.getEpochSecond))
    val siteid           = processString(row.get("siteid").getOrElse("Unknown"))
    val msg              = processString(row.get("msg").getOrElse("Got alarm"))
    val formattedMsg     = msg.replace("'", "").replace("\r\n", "").replace("\n\r", "").replace("\n", "")
    val severity         = processSet(row.get("severity").getOrElse(Set.empty))
    val notificationid   = processString(row.get("notificationid").get)
    val emailUsersList   = processSet(row.get("emailUsersList").getOrElse(Set.empty))
    val created          = processLong(row.get("created").getOrElse(Instant.now.getEpochSecond))
    val active           = processBoolean(row.get("active").getOrElse(false))
    val orgId            = processString(row.get("orgid").getOrElse("Unknown"))

    val colNames = "(notificationid, orgid, siteid, hold_off,window, name, smsUsersList, notificationtype, additionalEmails," +
      " resend_interval, description, scope, updated, msg, severity, emailUsersList, created, active)"

    s"""
       | INSERT INTO $keyspace.$notifications ${colNames} VALUES
       | ('${notificationid}','${orgId}','${siteid}', ${holdOff}, '${window}','${name}', ${formatList(smsUsersList)},
       |  ${formatList(notificationtype)}, ${formatList(additionalEmails)}, ${resend_interval},
       | '${formattedDesc}', '${scope}',  ${updated} , '${formattedMsg}', ${formatList(severity)},  ${formatList(emailUsersList)},
       | ${created}, ${active}
       | )
       |
      """.stripMargin
  }

  private def formatList(l: Set[String]): String =
    l.map(x => s"'${x}'").mkString("{", ", ", "}")

  def alertsQuery(row: Map[String, Any]): (String, String) = {
    //Column values
    val alertId      = processString(row.get("alertid").get)
    val orgId        = processString(row.get("orgid").getOrElse("Unknown"))
    val name         = processString(row.get("name").getOrElse("DeviceAlarm"))
    val active       = processBoolean(row.get("active").getOrElse(false))
    val alarmType    = processString(row.get("alarmtype").getOrElse("Unknown"))
    val category     = getCategory(alarmType)
    val msg          = processString(row.get("msg").getOrElse(""))
    val formattedMsg = msg.replace("'", "").replace("\r\n", "").replace("\n\r", "").replace("\n", "")
    val nodeId       = processString(row.get("nodeid").getOrElse("Unknown"))
    val severity     = processString(row.get("severity").getOrElse("Unknown"))
    val siteId       = processString(row.get("siteid").getOrElse("Unknown"))
    val created      = Instant.ofEpochMilli(processLong(row.get("created").getOrElse(Instant.now.getEpochSecond)))
    val updated      = Instant.ofEpochMilli(processLong(row.get("updated").getOrElse(Instant.now.getEpochSecond)))

    val colNames = "(alertid, orgid, siteid, name, alarmtype, nodeid, severity, msg, category, " +
      "active, created, updated)"

    val key = nodeId.trim + alarmType.trim + orgId.trim + siteId.trim
    val query = s"""
                   | INSERT INTO $keyspace.$alerts ${colNames} VALUES
                   | ('${alertId}','${orgId}','${siteId}', '${name}', '${alarmType}','${nodeId}', '${severity}',
                   |  '${formattedMsg}', '${category}', ${active},
                   | '${created}', '${updated}'
                   | )
                   |
      """.stripMargin
    (key, query)
  }

  def processString(value: Any): String =
    if (value == null || value.equals("") || value.equals("null") || value.equals("\'\'") || value.equals("\"\""))
      ""
    else {
      try { value.toString } catch {
        case ex: Exception => ""
      }
    }

  def processInt(value: Any): Int =
    if (value == null || value.equals("") || value.equals("null") || value.equals("\'\'") || value.equals("\"\""))
      0
    else {
      try { value.toString.toInt } catch {
        case ex: Exception => 0
      }
    }

  def processLong(value: Any): Long =
    if (value == null || value.equals("") || value.equals("null") || value.equals("\'\'") || value.equals("\"\""))
      0
    else {
      try { value.toString.toLong } catch {
        case ex: Exception => 0
      }
    }

  def processDouble(value: Any): Any =
    if (value == null || value.equals("") || value.equals("null") || value.equals("\'\'") || value.equals("\"\""))
      null
    else {
      try { value.toString.toDouble } catch {
        case ex: Exception => null
      }
    }

  def processBoolean(value: Any): Boolean =
    if (value == null || value.equals("") || value.equals("null") || value.equals("\'\'") || value.equals("\"\""))
      false
    else {
      try { value.toString.toBoolean } catch {
        case ex: Exception => false
      }
    }

  def processSet(value: Any): Set[String] =
    if (value == null || value == "" || value == "null" || value.equals("\'\'") || value.equals("\"\""))
      Set.empty
    else
      value
        .asInstanceOf[java.util.Collection[String]]
        .asScala
        .toSet
        .filterNot(y => (y == null || y.equals("") || y.equals("null") || y.equals("\'\'") || y.equals("\"\"")))

  def gpsQuery(row: Map[String, Any]): String = {
    //Column values
    val orgId          = processString(row.get("orgid").getOrElse("Unknown"))
    val siteId         = processString(row.get("siteid").getOrElse("Unknown"))
    val nodeId         = processString(row.get("nodeid").get)
    val name           = "GpsSample"
    val lat            = processDouble(row.get("latitude").getOrElse(""))
    val lon            = processDouble(row.get("longitude").getOrElse(""))
    val latitude_user  = processDouble(row.get("latUserAdded").getOrElse(""))
    val longitude_user = processDouble(row.get("lonUserAdded").getOrElse(""))
    val created        = processLong(row.get("created").getOrElse(""))
    val updated        = processLong(row.get("updated").getOrElse(""))

    val colNames = "(orgid, siteid, nodeid, name, lat, lon, latuseradded, lonuseradded, created, updated)"

    s"""
       | INSERT INTO $keyspace.$gps ${colNames} VALUES
       | ('${orgId}','${siteId}','${nodeId}', '${name}', ${lat}, ${lon}, ${latitude_user}, ${longitude_user}, ${created}, ${updated}
       | )
       |
      """.stripMargin
  }
}