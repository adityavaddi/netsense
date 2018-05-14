package com.verizon.netsense.helper

import java.net.ConnectException
import akka.Done
import akka.kafka.scaladsl.Consumer.Control
import akka.kafka.scaladsl.{Consumer, Producer}
import akka.kafka.{ConsumerSettings, ProducerSettings, Subscriptions}
import akka.stream.Supervision
import akka.stream.scaladsl.{Sink, Source}
import com.datastax.driver.core.exceptions.{NoHostAvailableException}
import com.fasterxml.jackson.databind.JsonMappingException
import com.verizon.netsense.metrics.{Instrumented}
import com.verizon.netsense.util.Logging
import com.verizon.netsense.util.ObjectMapperUtil
import com.verizon.netsense.utils.Z85Mapper
import org.apache.kafka.clients.consumer.ConsumerRecord
import org.apache.kafka.clients.producer.ProducerRecord
import scala.collection.immutable.HashMap.HashTrieMap
import scala.collection.mutable.ListBuffer
import scala.concurrent.Future
import scala.util.control.NonFatal

/**
  * created by drajput 02/12/2018
  */
trait Common extends Instrumented with Logging {

  lazy val parallelism = 1000

  lazy val systemDecider: Supervision.Decider = {
    case ex: ConnectException => log.error("Unable to connect to Graphite " + ex.getMessage); Supervision.Resume
    case ex: NoHostAvailableException => log.error("Unable to connect to DB " + ex.getMessage); Supervision.Resume
    case NonFatal(ex) => log.error("Exception found in the Stream " + ex.getMessage); Supervision.Resume
    case ex: Exception => log.error("Unhandled Exception seen " + ex.getMessage); Supervision.stop;
  }

  lazy val flowDecider: Supervision.Decider = {
    case ex: JsonMappingException => log.error("Unable to unpack the element", ex); Supervision.Resume
    case ex: NullPointerException => log.error("Null element in stream " + ex.getMessage); Supervision.Restart
    case ex: Exception => log.error("Unhandled exception in stream" + ex.getMessage); Supervision.Resume
  }

  lazy val convertToJson: (Any) => String = e => ObjectMapperUtil.toJson(e)

  def configKafkaSource[K, V](_consumerSettings: ConsumerSettings[K, V],
                              _topics: Set[String]): Source[ConsumerRecord[K, V], Control] =
    Consumer.plainSource(_consumerSettings, Subscriptions.topics(_topics))

  def configKafkaProducer[K, V](_producerSettings: ProducerSettings[K, V]): Sink[ProducerRecord[K, V], Future[Done]] =
    Producer.plainSink(_producerSettings)

  /**
    * Decodes all Z85'd UUIDs in a given Map (recursively).
    * The Message is a Map of [String, Any] - where Value Types may belong to
    *   - String
    *   - Vector[ Strings, Map[String, Any], Vector[] ]
    *   - HashTrieMap[String, Any]
    *   - Map[String, Any] :
    * @param x
    * @return Map[Any,Any]
    */
  def decodeUUID(x: Map[Any,Any]): Map[Any,Any] = { // Todo: Move to Core.
    var updatedMap = x
    updatedMap.foreach { k =>

      // Takes care of "uuid" as key in the Map
      (k._1) match {
        case "uuid" => {
          val uuid_decoded = Z85Mapper.decode(k._2.toString).toUpperCase
          updatedMap = updatedMap.updated(k._1, uuid_decoded)
        }

        // "o" - This is a Vector of Maps - Recurse on each Map, the internal Maps may have a "uuid" somewhere in there.
        case "o" => {
          var updatedV = new ListBuffer[Any]
          k._2 match {
            case v: Vector[_]  => {
              v.foreach { e =>
                e match {
                  case m: Map[_,_] =>
                    val z = decodeUUID(m.asInstanceOf[Map[Any,Any]])
                    updatedV += z
                  case _ =>
                    updatedV += e
                }
              }
            }
            case _ => // Not a Vector - Do Nothing
          }
          updatedMap = updatedMap.updated(k._1, updatedV.toVector)
        }
        case _ => // Do Nothing
      }

      // Takes care of Nested Maps - recursively.
      (k._2) match {
        case htm: HashTrieMap[_, _] =>
          val z = decodeUUID(htm.asInstanceOf[HashTrieMap[Any, Any]])
          updatedMap = updatedMap.updated(k._1, z)
        case m: Map[_,_] =>
          val z = decodeUUID(m.asInstanceOf[Map[Any,Any]])
          updatedMap = updatedMap.updated(k._1, z)
        case _ => // Do Nothing
      }
    }
    updatedMap
  }


}
