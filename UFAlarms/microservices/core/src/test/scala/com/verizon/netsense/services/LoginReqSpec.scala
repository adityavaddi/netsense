package com.verizon.netsense.services

import java.util.Base64

import com.verizon.netsense.entity.{LoginReq, Preamble}
import org.scalatest.{FlatSpec, Matchers}

class LoginReqSpec extends FlatSpec with Matchers {

  val preambleEncoded =
    "hqFhpFBPU1ShcL52MS9OMDMwOTk0ODAvb3V0L1BPU1QvbG9naW5SZXGhaKlsb2NhbGhvc3SkdXVpZNkkNTFlNDgxYjItMWY0ZS00YTZmLTliNGUtYjExYzg3YjdkODg3oWagoWzE0YujbmlkqU4wMzA5OTQ4MKF00xSpXV9BHv5eomNoGKNzZWOnV1BBX1BTS6Vic3NpZLdBRDpGRjpCRTpDQjpBQjpFRjpCQjpBQaRzc2lkrlNlbnNpdHlEZWZhdWx0o21hY7dCQTpGQzpBRjpCRjpEQTpGRDpBRTpDQqNkZXaodW5vZGUtdjejdG9r2S1BU0ZFRjY1NDIxM1FXRVFXRVFXQUZEUzU2MzIxMzI0NjVmcTR3NWU2NDMyNDGjY2lkp2RmYTg4ZXKiaXCpMTI3LjAuMC4x"

  val preambleByteArray = Base64.getDecoder.decode(preambleEncoded)

  val loginReqEncoded =
    "i6NuaWSpTjAzMDk5NDgwoXTTFKldX0Ee/l6iY2gYo3NlY6dXUEFfUFNLpWJzc2lkt0FEOkZGOkJFOkNCOkFCOkVGOkJCOkFBpHNzaWSuU2Vuc2l0eURlZmF1bHSjbWFjt0JBOkZDOkFGOkJGOkRBOkZEOkFFOkNCo2Rldqh1bm9kZS12N6N0b2vZLUFTRkVGNjU0MjEzUVdFUVdFUVdBRkRTNTYzMjEzMjQ2NWZxNHc1ZTY0MzI0MaNjaWSnZGZhODhlcqJpcKkxMjcuMC4wLjE="

  val loginReqByteArray = Base64.getDecoder.decode(loginReqEncoded)

  /**
  val loginReq = LoginReq(
    t = 1488823815518486110L,
    nid = "N03099480",
    ch = 24,
    sec = "WPA_PSK",
    bssid = "AD:FF:BE:CB:AB:EF:BB:AA",
    mac = "BA:FC:AF:BF:DA:FD:AE:CB",
    dev = "unode-v7",
    ssid = "SensityDefault",
    net = "",
    apn = "",
    imei = "",
    iccid = "",
    imsi = "",
    tok = "ASFEF654213QWEQWEQWAFDS5632132465fq4w5e643241",
    cid = "dfa88er",
    ip = "127.0.0.1"
  )

  val preamble = Preamble(a = "POST",
                          uuid = "51e481b2-1f4e-4a6f-9b4e-b11c87b7d887",
                          f = "",
                          sid = "localhost",
                          l = loginReq,
                          p = "v1/N03099480/out/POST/loginReq",
                          d = "")

  ignore should "decode msgpack" in {
//  "LoginReq" should "decode msgpack" in {

    Preamble.fromMsgPack(preambleByteArray) should be(preamble)

    LoginReq.fromMsgPack(loginReqByteArray) should be(loginReq)

  }
    */
}
