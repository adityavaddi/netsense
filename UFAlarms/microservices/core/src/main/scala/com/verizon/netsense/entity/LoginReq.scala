package com.verizon.netsense.entity

import com.verizon.netsense.utils.Deserializer
import org.json4s.{DefaultFormats, _}
import org.json4s.jackson.JsonMethods.parse
import org.slf4j.LoggerFactory

case class LoginReq(
    nid: String, // Node ID String - Uniquely identifies the node. Typically derived from the device's MAC address or IMEI number
    protoc: Int,
    dev: String, // Device Type String - Product version i.e. unode-v4, unode-v5 etc.
    cid: String,
    ssid: String, // WiFi SSID String - Service Set Identifier i.e. SensityDefault
    profile: String,
    ch: Int, // WiFi channel Number uint32 - Currently selected WiFi channel number
    tok: String, // Token Identifier String - Unique identifier to indicate the device's current configuration
    ip: String,
    t: Long, // Timestamp int64 - Indicates the current time in nanoseconds since epoch
    bssid: String, // WiFi BSSID Address String - Access Point's (AP) MAC address
    mac: String, // WiFi MAC address String - Device's (node) MAC address
    sec: String, // WiFi Security Type String - Security Type i.e WPA_PSK
    subtype: String,
    modemRevEd: String
) extends Entity // IP Address String - Dynamically allocated device IP address provided by either the WiFI AP or cell network

case class LSSLoginRequest(uuid: String,
                           sid: String,
                           f: String,
                           a: String,
                           p: String,
                           l: LSSLoginPayload) extends Entity

case class LSSLoginPayload(n: String, t: String, m: String) extends Entity

object LSSLoginRequest {
  def fromLoginPreamble(loginPreamble: PreambleLoginReq, loginReq: LoginReq) = {
    LSSLoginRequest(loginPreamble.uuid, loginPreamble.sid,
      loginPreamble.f, loginPreamble.a, loginPreamble.p, l = LSSLoginPayload(loginReq.nid, loginReq.tok, loginReq.dev))
  }
}

case class PreambleLoginReq(uuid: String, //  UUID
                            sid: String,  // hostname
                            f: String,    // flags
                            a: String,    // Action GET, POST, etc
                            p: String     // path
                           )

object LoginReq {

  implicit val formats = DefaultFormats

  val log = LoggerFactory.getLogger(this.getClass)

  def fromJson(msg: String): LoginReq = {
    log.info(s"LoginReq JSON payload $msg")
    val parsedMessage = parse(msg)

    LoginReq(
      (parsedMessage \ "nid").extract[String],
      if ((parsedMessage \ "protoc").extractOrElse("0").isEmpty) 0
      else  (parsedMessage \ "protoc").extractOrElse("0").toInt,
      (parsedMessage \ "dev").extract[String],
      (parsedMessage \ "cid").extractOrElse(""),
      (parsedMessage \ "ssid").extractOrElse(""),
      (parsedMessage \ "profile").extractOrElse(""),
      if ((parsedMessage \ "ch").extractOrElse("0").isEmpty) 0
      else  (parsedMessage \ "ch").extractOrElse("0").toInt,
      (parsedMessage \ "tok").extract[String],
      (parsedMessage \ "ip").extract[String],
      (parsedMessage \ "t").extract[Long],
      (parsedMessage \ "bssid").extractOrElse(""),
      (parsedMessage \ "mac").extractOrElse(""),
      (parsedMessage \ "sec").extractOrElse(""),
      (parsedMessage \ "subtype").extractOrElse(""),
      (parsedMessage \ "modemRevEd").extractOrElse("")
    )
  }

  def fromJsonPreamble(msg: String): PreambleLoginReq = {
    val parsedMessage = parse(msg)

    PreambleLoginReq((parsedMessage \ "uuid").extract[String],
                     (parsedMessage \ "sid").extract[String],
                     (parsedMessage \ "f").extract[String],
                     (parsedMessage \ "a").extract[String],
                     (parsedMessage \ "p").extract[String])
  }

  def fromJsonArray(msg: Array[Byte]): LoginReq = {
    Deserializer.jsonMapper.readValue(msg, classOf[LoginReq])
  }

  def fromMsgPack(msg: Array[Byte]): LoginReq = {

    log.info(s"LoginReq msgpack payload ${new String(msg)}")

    val hashmap = Deserializer.msgpackMapper.readValue(msg, classOf[Map[String, Object]])

    LoginReq(
      hashmap("nid").toString,
      hashmap.getOrElse("protoc", "0").toString.toInt,
      hashmap("dev").toString,
      hashmap.getOrElse("cid", "").toString,
      hashmap.getOrElse("ssid", "").toString,
      hashmap.getOrElse("profile", "").toString,
      hashmap.getOrElse("ch", "0").toString.toInt,
      hashmap("tok").toString,
      hashmap("ip").toString,
      hashmap("t").toString.toLong,
      hashmap.getOrElse("bssid", "").toString,
      hashmap.getOrElse("mac", "").toString,
      hashmap.getOrElse("sec", "").toString,
      hashmap.getOrElse("subtype", "").toString,
      hashmap.getOrElse("modemRevEd", "").toString
    )
  }
}

case class Preamble(a: String, // Action GET, POST, etc
                    uuid: String, //  UUID
                    f: String, // flags
                    sid: String, // hostname
                    p: String, // path
                    d: String, // Timestamp ISO-8601
                    l: LoginReq) // Payload

object Preamble {

  val log = LoggerFactory.getLogger(this.getClass)

  implicit val formats = DefaultFormats

  def fromJson(msg: Array[Byte]): Preamble = {
    log.info(s"Preamble JSON payload ${new String(msg)}")
    val parsedMessage = parse(new String(msg))
    Preamble(
      (parsedMessage \ "a").extract[String],
      (parsedMessage \ "uuid").extract[String],
      (parsedMessage \ "f").extract[String],
      (parsedMessage \ "sid").extract[String],
      (parsedMessage \ "p").extract[String],
      (parsedMessage \ "d").extract[String],
      LoginReq(
        (parsedMessage \ "l" \ "nid").extract[String],
        (parsedMessage \ "l" \ "protoc").extractOrElse("0").toInt,
        (parsedMessage \ "l" \ "dev").extract[String],
        (parsedMessage \ "l" \ "cid").extractOrElse(""),
        (parsedMessage \ "l" \ "ssid").extractOrElse(""),
        (parsedMessage \ "l" \ "profile").extractOrElse(""),
        (parsedMessage \ "l" \ "ch").extractOrElse("0").toInt,
        (parsedMessage \ "l" \ "tok").extract[String],
        (parsedMessage \ "l" \ "ip").extract[String],
        (parsedMessage \ "l" \ "t").extract[Long],
        (parsedMessage \ "l" \ "bssid").extractOrElse(""),
        (parsedMessage \ "l" \ "mac").extractOrElse(""),
        (parsedMessage \ "l" \ "sec").extractOrElse(""),
        (parsedMessage \ "l" \ "subtype").extractOrElse(""),
        (parsedMessage \ "l" \ "modemRevEd").extractOrElse("")
      )
    )
  }

  def fromMsgPack(msg: Array[Byte]): Preamble = {

    log.info(s"Preamble msgpack payload ${new String(msg)}")

    val hashmap = Deserializer.msgpackMapper.readValue(msg, classOf[Map[String, Object]])

    log.info(s" Preamble hashmap ${hashmap}")

    val l = hashmap("l").asInstanceOf[Array[Byte]]

    val loginReq = LoginReq.fromMsgPack(l)

    Preamble(hashmap("a").toString,
             hashmap("uuid").toString,
             hashmap("f").toString,
             hashmap("sid").toString,
             hashmap("p").toString,
             hashmap("d").toString,
             loginReq)
  }
}
