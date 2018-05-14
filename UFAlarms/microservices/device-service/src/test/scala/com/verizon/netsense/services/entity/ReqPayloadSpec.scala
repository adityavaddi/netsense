package com.verizon.netsense.services.entity

import java.util.Base64

import com.verizon.netsense.entity._
import org.joda.time.DateTime
import org.json4s.DefaultFormats
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by davor on 4/11/2017.
 */
class ReqPayloadSpec extends FlatSpec with Matchers {

  //val respEncoded =
  //  "hqFhpFBPU1ShcL52MS9OMDMwOTk0ODAvb3V0L1BPU1QvbG9naW5SZXGhaKlsb2NhbGhvc3SkdXVpZNkkNTFlNDgxYjItMWY0ZS00YTZmLTliNGUtYjExYzg3YjdkODg3oWagoWzE8N4AEKF0zxSpXV9BHv5eo25pZKlOMDMwOTk0ODCiY2gYo3NlY6dXUEFfUFNLpWJzc2lkt0FEOkZGOkJFOkNCOkFCOkVGOkJCOkFBo21hY7dCQTpGQzpBRjpCRjpEQTpGRDpBRTpDQqNkZXaodW5vZGUtdjekc3NpZK5TZW5zaXR5RGVmYXVsdKNuZXSgo2FwbqCkaW1laaClaWNjaWSgpGltc2mgo3Rva9ktQVNGRUY2NTQyMTNRV0VRV0VRV0FGRFM1NjMyMTMyNDY1ZnE0dzVlNjQzMjQxo2NpZKdkZmE4OGVyomlwqTEyNy4wLjAuMQ"

  //val reqByteArray = Base64.getDecoder.decode(respEncoded)

  implicit val formats = DefaultFormats


  val loginReq = LoginReq(
    nid = "N03099480",
    protoc = 1,
    dev = "unode-v7",
    cid = "1",
    ssid = "SensityDefault",
    profile = "",
    ch = 24,
    tok = "ASFEF654213QWEQWEQWAFDS5632132465fq4w5e643241",
    ip = "127.0.0.1",
    t = 1488823815518486110L,
    bssid = "AD:FF:BE:CB:AB:EF:BB:AA",
    mac = "BA:FC:AF:BF:DA:FD:AE:CB",
    sec = "WPA_PSK",
    subtype = "",
    modemRevEd = ""
  )

  val loginReqMap = Map(
    "t" -> System.currentTimeMillis,
    "nid" -> "N03099480",
    "ch" -> 24,
    "sec" -> "WPA_PSK",
    "bssid" -> "AD:FF:BE:CB:AB:EF:BB:AA",
    "mac" -> "BA:FC:AF:BF:DA:FD:AE:CB",
    "dev" -> "unode-v7",
    "ssid" -> "SensityDefault",
    "net" -> "",
    "apn" -> "",
    "imei" -> "",
    "iccid" -> "",
    "imsi" -> "",
    "tok" -> "ASFEF654213QWEQWEQWAFDS5632132465fq4w5e643241",
    "cid" -> "dfa88er",
    "ip" -> "127.0.0.1"
  )
  val reqPayload = ReqPayloadMQTT(
    a = "POST",
    p = "v1/N03099480/out/POST/loginReq",
    sid = "localhost",
    d = DateTime.now.toDateTimeISO.toString,
    uuid = "51e481b2-1f4e-4a6f-9b4e-b11c87b7d887",
    f = "",
    l = loginReq.toMsgPack
  )

  val reqPayloadJSON = RestPackRequestJSON(
    a = "POST",
    p = "v1/N03099480/out/POST/loginReq",
    sid = "localhost",
    d = DateTime.now.toDateTimeISO.toString,
    uuid = "51e481b2-1f4e-4a6f-9b4e-b11c87b7d887",
    f = "",
    l = loginReqMap
  )

  val bytes = ReqPayloadMQTT.toMsgPack(reqPayload)

  val str = new String(Base64.getEncoder.encode(bytes))

  "ReqPayload" should "decode msgpack" in {

    val resp = ReqPayloadMQTT.fromMsgPack(reqPayload.toMsgPack)
    val lReq = LoginReq.fromMsgPack(resp.l)

    resp.a should be(reqPayload.a)
    resp.p should be(reqPayload.p)
    resp.sid should be(reqPayload.sid)
    resp.f should be(reqPayload.f)
    resp.uuid should be(reqPayload.uuid)
    lReq should be(loginReq)
  }

  it should "decode JSON" in {
    val resp = RestPackRequestJSON.fromJSON(reqPayloadJSON.toJsonArray)

    resp.a should be(reqPayloadJSON.a)
    resp.p should be(reqPayloadJSON.p)
    resp.sid should be(reqPayloadJSON.sid)
    resp.f should be(reqPayloadJSON.f)
    resp.uuid should be(reqPayloadJSON.uuid)
    //lReq should be(loginReq)

  }

}
