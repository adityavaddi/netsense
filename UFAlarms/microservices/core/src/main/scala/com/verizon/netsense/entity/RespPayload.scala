package com.verizon.netsense.entity

import com.verizon.netsense.utils.Deserializer
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods.parse
import org.slf4j.LoggerFactory

/**
 * Created by davor on 3/22/2017.
 */
case class RespPayloadMQTT(
    a: String, // Action
    p: String, // Path
    sid: String, // sid
    d: String, // date
    s: Int, // Status Code
    e: String, // Error
    uuid: String, // UUID of the message
    l: Array[Byte] // Payload of LoginResp
) extends Entity

object RespPayloadMQTT {
  def toMsgPack(rp: RespPayloadMQTT): Array[Byte] =
    Deserializer.msgpackMapper.writeValueAsBytes(rp)
  def fromMsgPack(resp: IndexedSeq[Byte]): RespPayloadMQTT = {
    val hashmap = Deserializer.msgpackMapper.readValue(resp.toArray, classOf[Map[String, Object]])

    RespPayloadMQTT(
      hashmap("a").toString,
      hashmap("p").toString,
      hashmap("sid").toString,
      hashmap("d").toString,
      hashmap("s").asInstanceOf[Int],
      hashmap("e").toString,
      hashmap("uuid").toString,
      hashmap("l").asInstanceOf[Array[Byte]]
    )
  }
}
case class RespPayloadJSON(a: String, p: String, sid: String, d: String, uuid: String, s: Int, e: String, l: String)
    extends Entity

object RespPayloadJSON {

  implicit val formats = DefaultFormats

  val log = LoggerFactory.getLogger(this.getClass)

  def toJSON(rp: RespPayloadJSON): Array[Byte] =
    Deserializer.jsonMapper.writeValueAsBytes(rp)
  def fromJSON(resp: String): RespPayloadJSON = {

    log.info(s"Received: ${resp}")

    val parsedMessage = parse(resp)
    RespPayloadJSON(
      (parsedMessage \ "a").extractOrElse(""),
      (parsedMessage \ "p").extractOrElse(""),
      (parsedMessage \ "sid").extract[String],
      (parsedMessage \ "d").extractOrElse(""),
      (parsedMessage \ "uuid").extractOrElse(""),
      (parsedMessage \ "s").extractOrElse(-1),
      (parsedMessage \ "e").extractOrElse(""),
      (parsedMessage \ "l").extract[String]
    )
  }
}
