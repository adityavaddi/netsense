package com.vz.nsp.util

import com.fasterxml.jackson.annotation.JsonInclude.Include
import com.fasterxml.jackson.databind.exc.InvalidDefinitionException
import com.fasterxml.jackson.databind.{DeserializationFeature, ObjectMapper}
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper
import com.vz.nsp.trigger.model.app.TriggerResource
import com.vz.nsp.trigger.model.casel.{AppRequest, MessageIdAndResponseTopic}

import scala.concurrent.{ExecutionContext, Future}
import com.vz.nsp.trigger.helper.CommonHelper.generateAppRequestForValidationError

/**
  * Created by maidapr on 4/7/17.
  */
object ObjectMapperUtil {

  implicit val ec = scala.concurrent.ExecutionContext.Implicits.global

  val mapper = new ObjectMapper() with ScalaObjectMapper
  mapper.registerModule(DefaultScalaModule)
  mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false)
  mapper.setSerializationInclusion(Include.NON_ABSENT)

  def toJson(value: Any): String =
    mapper.writeValueAsString(value)

  def toJsonBytes(value: Any): Array[Byte] =
    toJson(value).getBytes()

  def fromJson[T](json: String)(implicit m: Manifest[T]): T =
    mapper.readValue[T](json)

  def fromJsonWithNoOption[T](json: String)(implicit m: Manifest[T]): Future[T] =
    Future {
      mapper.readValue[T](json)
    }

  def fromJsonAsync[T](json: String)(implicit m: Manifest[T]): Future[Option[T]] = Future {
    Option(mapper.readValue[T](json))
  }

  def fromJsonByteArrayAsync[T](json: Array[Byte])(implicit m: Manifest[T]): Future[Option[T]] =
    Future {
      Option(mapper.readValue[T](json))
    }

  def fromJsonSensorSampleValueAsync[T](msg: Array[Byte])(
    implicit m: Manifest[T]): Future[(Option[T], Array[Byte])] = Future {
    (Option(mapper.readValue[T](msg)), msg)
  }

  def jsonToAppRequest(json: String)(implicit ec: ExecutionContext,
                                     m: Manifest[AppRequest]): Future[AppRequest] = Future {
    try {
      mapper.readValue[AppRequest](json)
    } catch {

      case e: InvalidDefinitionException => {
        val messageId: MessageIdAndResponseTopic = mapper.readValue[MessageIdAndResponseTopic](json)
        generateAppRequestForValidationError(messageId.messageid, messageId.responsetopic, e.getCause.getMessage)
      }
    }
  }

  def jsonToResourceList(json: String)(
    m: Manifest[TriggerResource]): List[TriggerResource] = {
    mapper.readValue[List[TriggerResource]](json)
  }


}
