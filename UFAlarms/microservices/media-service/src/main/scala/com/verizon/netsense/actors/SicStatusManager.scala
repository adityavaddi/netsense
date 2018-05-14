package com.verizon.netsense.actors

import akka.actor.{Actor, Props, Scheduler}
import com.outworkers.phantom.connectors.CassandraConnection
import org.joda.time.{DateTime, DateTimeZone}
import com.datastax.driver.core.utils.UUIDs

import scala.concurrent.duration._
import com.verizon.netsense.actors.SicStatusManager._
import com.verizon.netsense.entity.SicStatus
import com.verizon.netsense.database.SicStatusModel
import com.verizon.netsense.service.SICService
import com.verizon.netsense.util.Logging
import org.apache.cassandra.utils.UUIDGen

class SicStatusManager(val connector: CassandraConnection) extends Actor with Logging {
  import context._

  val msgTimeout = 30 seconds // Todo: Create a TimeOut Map, for different kind fo requests.
  object sicStatusModel extends SicStatusModel with connector.Connector

  override def receive: Receive =  {
    case ISReqReceived(msg) =>
      val id = UUIDGen.getTimeUUID(System.currentTimeMillis())
      val ts = new DateTime(id.timestamp(),DateTimeZone.UTC)
      val newState = SicStatus( id, msg.get("msgId").get, "IS",
        MsgType.REQ.toString, MsgState.RCVD.toString, ts, "init")
      sicStatusModel.store(newState)
      // If no RESP received, Send Timeout on "Service" to IS.
      self ! system.scheduler.scheduleOnce(60 seconds, self, UpdateStatus(msg.get("msgId").get, "IS",
        MsgType.REQ, MsgState.RCVD, ts))

    case ISRespSent(msg) =>
      val id = UUIDs.timeBased()
      val ts = new DateTime(id.timestamp(),DateTimeZone.UTC)
      val newState = SicStatus(id, msg.get("msgId").get, "IS",
        MsgType.RESP.toString, MsgState.SENT.toString, ts, "init")
      sicStatusModel.store(newState)

    case SicReqSent(msg) =>
      val id = UUIDs.timeBased()
      val ts = new DateTime(id.timestamp(),DateTimeZone.UTC)
      val newState = SicStatus(id, msg.get("msgId").get, "SIC_KAFKA",
         MsgType.REQ.toString, MsgState.SENT.toString, ts, "init")
      sicStatusModel.store(newState)
      // If no ACK Received, Send time out on "MQTT/Device ACK" to IS.
      self ! system.scheduler.scheduleOnce(15 seconds, self, UpdateStatus(msg.get("msgId").get, "SIC_KAFKA",
        MsgType.REQ, MsgState.SENT, ts))

    case SicAckReceived(msg) =>
      val id = UUIDs.timeBased()
      val ts = new DateTime(id.timestamp(),DateTimeZone.UTC)
      val newState = SicStatus(id, msg.get("msgId").get, "SIC_KAFKA",
        MsgType.ACK.toString, MsgState.RCVD.toString, ts, "init")
      sicStatusModel.store(newState)
      // If no Resp received, timeout on "MQTT / Device Image" to IS.
      self ! system.scheduler.scheduleOnce(15 seconds, self, UpdateStatus(msg.get("msgId").get, "SIC_KAFKA",
        MsgType.REQ, MsgState.SENT, ts))

    case SicRespReceived(msg) =>
      val id = UUIDs.timeBased()
      val ts = new DateTime(id.timestamp(),DateTimeZone.UTC)
      val newState = SicStatus(id, msg.get("msgId").get, "SIC_KAFKA",
         MsgType.RESP.toString, MsgState.RCVD.toString, ts, "init")
      val rs = sicStatusModel.store(newState)

    case CheckStatus(msgId, msgSrc, msgType) =>
      val rs = sicStatusModel.getByType(msgId, msgSrc, msgType.toString)
      rs.map {  x => sender ! x.get }

    case UpdateStatus(msgId, msgSrc, mType, mstate, ts) => {
      log.debug(s"Check Msg State and determine TimeOut for: [ ${msgId}, ${msgSrc}, ${mType} ]  ")
      msgSrc match {
        case "IS" => {
          mType match {
            case MsgType.REQ =>
              val msgTimedout = checkDBForTimeout(msgId, msgSrc, "", MsgType.RESP.toString, MsgState.RCVD.toString, "IS Req Timed out." )
              if (msgTimedout) {
                sender ! SICService.StatusTimeOut(Map("msgId" -> msgId, "status" -> "IS request Timed Out."))
                //sender ! (("Timeout", s"${msgSrc} ${msgType.RESP.toString} Not ${msgState.RCVD.toString}"))
              }
          }
        }
        case "SIC_KAFKA" => {
          mType match {
            case MsgType.REQ  =>
              val msgTimedout = checkDBForTimeout(msgId, msgSrc, "", MsgType.ACK.toString, MsgState.RCVD.toString, "SIC Request to Device/MQTT Timed out." )
              if (msgTimedout) {
                sender ! SICService.StatusTimeOut(Map("msgId" -> msgId, "status" -> "MQTT ACK Timed Out."))
              }

            case MsgType.ACK =>
              val msgTimedout = checkDBForTimeout(msgId, msgSrc, "", MsgType.RESP.toString,  MsgState.RCVD.toString, "SIC Image from Device/MQTT Timed out." )
              if (msgTimedout) {
                sender ! SICService.StatusTimeOut(Map("msgId" -> msgId, "status" -> "MQTT (Image) Response Timed Out."))
              }
          }
        }
        case _ => // Do Nothing
      }
    }
  }

  protected def checkDBForTimeout(msgId: String, msgSrc: String, topic: String, mType: String, mState: String, reason: String) = {
    // if (RESP/ACK).RCVD then do nothing, else mark (RESP/ACK) as FAILED
    var hasTimedout = false
    val id = UUIDs.timeBased()
    val ts = new DateTime(id.timestamp(),DateTimeZone.UTC)

    sicStatusModel.getByType(msgId, msgSrc, mType).map {
      case Some(x) => {
        if  (x.state equals(mState))
          { log.debug(s"CheckDBForTimeout: Msg Already Processed: ${msgId}, ${msgSrc}, ${mType}") }
        else {
          val newState = SicStatus(id, msgId, msgSrc, mType, MsgState.FAILED.toString, ts, reason)
          sicStatusModel.store(newState)
          log.debug(s"CheckDBForTimeout: Msg Timed out: ${msgId}, ${msgSrc}, ${mType}")
          hasTimedout = true
        }
      }
      case None => // No Initial Status Message?? Not right... Log warning
        log.warn((s"CheckDBForTimeout - No Initial Status in DB for: ${msgId}, ${msgSrc}, ${mType} "))
    }
    hasTimedout
  }

  def scheduler: Scheduler = context.system.scheduler

}

object SicStatusManager {

  val props = Props[SicStatusManager]
  sealed abstract class MessageType

  case class ISReqReceived(value: Map[String, String] ) extends MessageType              // IS -> Media Service
  case class ISRespSent(value: Map[String, String]) extends MessageType                  // Media Service --> IS
  case class SicReqSent(value: Map[String, String]) extends MessageType                  // Media Service --> Kafka --> MQTT
  case class SicAckReceived(value: Map[String, String]) extends MessageType              // MQTT --> Kafka --> Media Service
  case class SicRespReceived(value: Map[String, String]) extends MessageType             // MQTT --> Kafka --> Media Service
  case class DeviceStatusReqSent(value: Map[String, String]) extends MessageType         // Media Serivce --> Kafka --> Device Service : Todo - complete implementation when support available in Device Service
  case class DeviceStatusRespReceived(value: Map[String, String]) extends MessageType    // Device Service --> Kafka --> Media Service : Todo - complete implementation when support available in Device Service
  case class CheckStatus(msgId: String, msgSrc: String,
                         mType: MsgType.Value) extends MessageType                       // Self --> Check MSG status in Farallones.media_service
  case class UpdateStatus(msgId: String, msgSrc: String, mType: MsgType.Value,
                          mState: MsgState.Value, ts: DateTime) extends MessageType      // Self --> Update state in Farallones.media_service

}

object MsgState extends Enumeration {
  val RCVD = Value("RCVD")
  val SENT = Value("SENT")
  val FAILED = Value("FAILED")
}

object MsgType extends Enumeration {
  val REQ = Value("REQ")
  val RESP = Value("RESP")
  val ACK = Value("ACK")
}
