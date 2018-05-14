package com.verizon.netsense.services.entity

import com.verizon.netsense.entity.{LoginReply, RespPayloadMQTT}
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by davor on 4/11/2017.
 */
class RespPayloadSpec extends FlatSpec with Matchers {

  //val encodedResp =
  //  "iKFhpFBPU1ShcL92MS9OMDMwOTk0ODAvb3V0L1BPU1QvbG9naW5SZXNwoWipbG9jYWxob3N0oXPMyKFloKFmoKR1dWlk2SQ1MWU0ODFiMi0xZjRlLTRhNmYtOWI0ZS1iMTFjODdiN2Q4ODehbMQMgaF0zwAAAVtdkci3"

  //val respByteArray = Base64.getDecoder.decode(encodedResp)

  val loginResp = LoginReply(1491923486903L)

  val respPayload = RespPayloadMQTT(
    "POST",
    "v1/N03099480/out/POST/loginResp",
    "N03099480",
    "51e481b2-1f4e-4a6f-9b4e-b11c87b7d887",
    200,
    "",
    "",
    loginResp.toMsgPack
  )

  "RespPayload" should "decode msgpack" in {

    val resp  = RespPayloadMQTT.fromMsgPack(respPayload.toMsgPack)
    val lResp = LoginReply.fromMsgPack(resp.l)

    resp.a should be(respPayload.a)
    resp.p should be(respPayload.p)
    resp.sid should be(respPayload.sid)
    resp.s should be(respPayload.s)
    resp.uuid should be(respPayload.uuid)
    lResp should be(loginResp)
  }
}
