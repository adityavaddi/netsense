package com.vz.ns.ts.service.util

import com.fasterxml.jackson.databind.{DeserializationFeature, ObjectMapper}
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper

import scala.concurrent.{ExecutionContext, Future}
import scala.util.parsing.json.{JSONArray, JSONObject}

object ObjectMapperUtil {

  val mapper = new ObjectMapper() with ScalaObjectMapper
  mapper.registerModule(DefaultScalaModule)
  mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false)

  def toJson(value: Any): String =
    mapper.writeValueAsString(value)

  def toMap[V](json: String)(implicit m: Manifest[V]) = fromJson[Map[String, V]](json)

  def fromJson[T](json: String)(implicit m: Manifest[T]): T =
    mapper.readValue[T](json)

  def fromJsonAsync[T](json: String)(implicit ec: ExecutionContext, m: Manifest[T]): Future[Option[T]] = Future {
    Option(mapper.readValue[T](json))
  }

  def toJson(m: Map[String, Any]): String =
    JSONObject(
      m.mapValues {
        case mp: Map[String, Any]       => JSONObject(mp)
        case lm: List[Map[String, Any]] => JSONArray(lm.map(JSONObject(_)))
        case x                          => x
      }
    ).toString
}
