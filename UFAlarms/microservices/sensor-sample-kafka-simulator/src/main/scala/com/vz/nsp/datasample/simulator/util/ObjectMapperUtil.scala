package com.vz.nsp.datasample.simulator.util

import com.fasterxml.jackson.databind.{DeserializationFeature, ObjectMapper}
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper

import scala.concurrent.{ExecutionContext, Future}

/**
 * Created by maidapr on 4/7/17.
 */
object ObjectMapperUtil {

  val mapper = new ObjectMapper() with ScalaObjectMapper
  mapper.registerModule(DefaultScalaModule)
  mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false)

  def toJson(value: Map[Symbol, Any]): String =
    toJson(value map { case (k, v) => k.name -> v })

  def toJson(value: Any): String =
    mapper.writeValueAsString(value)

  def toJsonAsync(value: Any)(implicit ec: ExecutionContext): Future[String] = Future {
    mapper.writeValueAsString(value)
  }

  def toMap[V](json: String)(implicit m: Manifest[V]) = fromJson[Map[String, V]](json)

  def fromJson[T](json: String)(implicit m: Manifest[T]): T =
    mapper.readValue[T](json)

  def fromJsonAsync[T](json: String)(implicit ec: ExecutionContext, m: Manifest[T]): Future[Option[T]] = Future {
    Option(mapper.readValue[T](json))
  }

}
