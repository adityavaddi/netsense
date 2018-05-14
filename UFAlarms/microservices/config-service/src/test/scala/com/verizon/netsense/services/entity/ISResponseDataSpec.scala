package com.verizon.netsense.services.entity

import com.fasterxml.jackson.annotation.JsonProperty
import com.verizon.netsense.entity.{ISResponseAllConfig, ISResponseConfig, ISResponsePayload, ISResponseToken}
import org.scalatest.{FlatSpec, Matchers}
import org.json4s._
import org.json4s.jackson.JsonMethods.{parse, _}


class ISResponseDataSpec extends FlatSpec with Matchers {

  implicit val formats = DefaultFormats


  val responseConfig = ISResponseConfig(java.util.UUID.randomUUID().toString,
                                        true,
                                        "2017-08-25",
                                        Map("debugmode" -> true, "server" -> "nsn-local.sensity.com"), List())

  val responseAllConfig = ISResponseAllConfig(java.util.UUID.randomUUID().toString,
                                              true,
                                              "2017-08-25",
                                              List(Map("debugmode" -> true, "server" -> "nsn-local.sensity.com")))

  val responseToken = ISResponseToken(java.util.UUID.randomUUID().toString,
                                              true,
                                              "2017-08-25",
    "Message",
    java.util.UUID.randomUUID().toString)


  "ReqPayload" should "decode is response config" in {
      val resp = responseConfig.toJSON

    val decoded = parse(resp)

    (decoded \ "requestid").extract[String] should be(responseConfig.requestid)
    (decoded \ "success").extract[Boolean] should be(responseConfig.success)
    (decoded \ "timestamp").extract[String] should be(responseConfig.timestamp)
    (decoded \ "config").extract[Map[String,Any]] should be(responseConfig.config)
  }

  it should "decode IS response all config" in {
    val resp = responseAllConfig.toJSON

    val decoded = parse(resp)

    (decoded \ "requestid").extract[String] should be(responseAllConfig.requestid)
    (decoded \ "success").extract[Boolean] should be(responseAllConfig.success)
    (decoded \ "timestamp").extract[String] should be(responseAllConfig.timestamp)
    (decoded \ "items").extract[List[Map[String,Any]]] should be(responseAllConfig.items)
  }

  it should "decode IS response token" in {
    val resp = responseToken.toJSON

    val decoded = parse(resp)

    (decoded \ "requestid").extract[String] should be(responseToken.requestid)
    (decoded \ "success").extract[Boolean] should be(responseToken.success)
    (decoded \ "timestamp").extract[String] should be(responseToken.timestamp)
    (decoded \ "message").extract[String] should be(responseToken.message)
    (decoded \ "token").extract[String] should be(responseToken.token)
  }
}
