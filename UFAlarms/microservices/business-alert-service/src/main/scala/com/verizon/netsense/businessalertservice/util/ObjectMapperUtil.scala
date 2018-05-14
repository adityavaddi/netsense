package com.verizon.netsense.businessalertservice.util

import com.fasterxml.jackson.annotation.JsonInclude.Include
import com.fasterxml.jackson.databind.exc.InvalidDefinitionException
import com.fasterxml.jackson.databind.{DeserializationFeature, ObjectMapper}
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper
import com.verizon.netsense.businessalertservice.model.{AppRequest, MessageIdAndResponseTopic, RequestBody}

import scala.concurrent.{ExecutionContext, Future}

object ObjectMapperUtil {

  val mapper = new ObjectMapper() with ScalaObjectMapper
  mapper.registerModule(DefaultScalaModule)
  mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false)
  mapper.setSerializationInclusion(Include.NON_ABSENT)

  def toJson(value: Any): String =
    mapper.writeValueAsString(value)

  def fromJson[T](json: String)(implicit m: Manifest[T]): T =
    mapper.readValue[T](json)

  def generateAppRequestForValidationError(msgid: String, responsetopic: String, errormsg: String): AppRequest =
    AppRequest(
      msgid,
      responsetopic,
      RequestBody(null, null, null, `type` = "validationfailed", model = null, null, None, null, null, None)
    )

  def jsonToAppRequest(json: String)(implicit ec: ExecutionContext, m: Manifest[AppRequest]): Future[AppRequest] =
    Future {
      try {
        mapper.readValue[AppRequest](json)
      } catch {
        case e: InvalidDefinitionException => {
          val messageId: MessageIdAndResponseTopic = mapper.readValue[MessageIdAndResponseTopic](json)
          generateAppRequestForValidationError(messageId.messageid, messageId.responsetopic, e.getCause.getMessage)
        }
      }
    }

  def fromJsonByteArrayAsync[T](json: Array[Byte])(implicit ec: ExecutionContext, m: Manifest[T]): Future[Option[T]] =
    Future {
      Option(mapper.readValue[T](json))
    }

  def fromJsonSensorSampleValueAsync[T](msg: Array[Byte])(implicit ec: ExecutionContext,
                                                          m: Manifest[T]): Future[(Option[T], Array[Byte])] = Future {
    (Option(mapper.readValue[T](msg)), msg)
  }
  def isEmpty(x: String): Boolean = x == null || x.trim.isEmpty

}
