package com.verizon.netsense.entity

import com.verizon.netsense.entity.Preamble.log
import com.verizon.netsense.utils.Deserializer
import org.json4s._
import org.json4s.jackson.JsonMethods.{parse, _}

/**
  * Created by davor on 3/22/2017.
  */
case class ReqPayloadMQTT(a: String, // Action GET, POST, etc
                          p: String,
                          sid: String,
                          d: String,
                          uuid: String, //  UUID
                          f: String,
                          l: Array[Byte] // LoginReq Payload
                         ) extends Entity

object ReqPayloadMQTT {
  def toMsgPack(rp: ReqPayloadMQTT): Array[Byte] =
    Deserializer.msgpackMapper.writeValueAsBytes(rp)
  def fromMsgPack(msg: IndexedSeq[Byte]): ReqPayloadMQTT = {
    val hashmap = Deserializer.msgpackMapper.readValue(msg.toArray, classOf[Map[String, Object]])

    ReqPayloadMQTT(
      hashmap("a").toString,
      hashmap("p").toString,
      hashmap("sid").toString,
      hashmap("d").toString,
      hashmap("uuid").toString,
      hashmap("f").toString,
      hashmap("l").asInstanceOf[Array[Byte]] // LoginReq Payload
    )
  }
}

case class RestPackRequestJSON(a: String, // Action GET, POST, etc
                               p: String, // Topic
                               sid: String, // Nodeid
                               d: String, // Date
                               uuid: String, // UUID
                               f: String,
                               l: Map[String, Any] // LoginReq Payload
                              ) extends Entity

object RestPackRequestJSON {

  implicit val formats = DefaultFormats


  def toJSON(rp: RestPackRequestJSON): Array[Byte] =
    Deserializer.jsonMapper.writeValueAsBytes(rp)
  def fromJSON(msg: Array[Byte]): RestPackRequestJSON = {


    log.info(s"ReqPayloadJSON JSON payload ${new String(msg)}")
    val parsedMessage = parse(new String(msg))
    RestPackRequestJSON(
      (parsedMessage \ "a").extract[String],
      (parsedMessage \ "p").extract[String],
      (parsedMessage \ "sid").extract[String],
      (parsedMessage \ "d").extract[String],
      (parsedMessage \ "uuid").extract[String],
      (parsedMessage \ "f").extract[String],
      (parsedMessage \ "l").extract[Map[String, Any]]
    )
  }
}
