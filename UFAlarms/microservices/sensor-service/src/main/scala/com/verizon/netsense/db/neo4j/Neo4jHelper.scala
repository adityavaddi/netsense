package com.verizon.netsense.db.neo4j

import java.util

import com.verizon.netsense.constants.SensorSampleConstants
import com.verizon.netsense.exceptions.CustomExceptions.{EmptyResultSetException, UnableToConvertGraphResultToDTO}
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.utils.MarshallableImplicits._
import com.verizon.netsense.utils.ObjectMapperUtil
import nl.grons.metrics.scala.Timer
import org.neo4j.driver.v1.Record

/**
  * Created by maidapr on 1/24/18.
  */
class Neo4jHelper extends Neo4jProductionDb with Instrumented {

  private[this] val SSNeo4jReadTimer: Timer = metrics.timer("ss-neo4j-read-flow-timer")

  implicit class NullOccludingMap[K, V](private val underlying: Map[K, V]) {

    def getNonNullOrElse(key: K, ex: Throwable): Some[V] = {
      try {
        underlying.get(key) match {
          case v@Some(value) if value != "" => v
          case _ => throw ex
        }
      } catch {
        case e: NullPointerException => throw ex
        case _: Throwable => throw ex
      }
    }
  }

  /**
    * Important Neo4jResultWrapper to convert the Values from Neo4j MapAccessors to JSON String &
    * below replace operations will work in conjuction with [[ObjectMapperUtil.fromJson]]
    * and ObjectMapper Properties
    * */
  implicit class Neo4jDataConverter(records: List[Record]) {

    val itemsStr = "items"
    def convertResultToDTO[T]()(implicit m: Manifest[T]): List[T] = {
      val formatted = records.map {
        case a if !a.fields().isEmpty && a.containsKey(itemsStr) => a.get(itemsStr).toString
        case x@a => a.fields().get(0).value() match {
          case value if !value.isEmpty && !value.isNull => value.toString.replace("NULL", "null")
              .replace("TRUE", "true").replace("FALSE", "false")
          case value if value.asList.isEmpty => throw new EmptyResultSetException()
          case _ => throw new UnableToConvertGraphResultToDTO(x.toJson)
        }
      }
      formatted.map{ x => x.fromJson[T]}
    }
  }

  def getDataFromGraphDbToDTO[T](cypher: String, innerParams: util.HashMap[String, Object])(implicit m: Manifest[T]): List[T] = {
    SSNeo4jReadTimer.time {
      try {
        val records: List[Record] = getDataFromGraphDb(cypher, innerParams)
        records match {
          case elements if elements.nonEmpty => elements
          case _ => List[Record]()
        }
        records.convertResultToDTO()
      } catch {
        case ex: Exception => throw ex
      }
    }
  }


  def getDataFromGraphDb(cypher: String, innerParams: util.HashMap[String, Object]): List[Record] = {
    SSNeo4jReadTimer.time {
      val propsMap = new util.HashMap[String, Object]()
      propsMap.putIfAbsent(SensorSampleConstants.PROPS, innerParams)
      execute(cypher, propsMap).toEither match {
        case Left(l) => throw l
        case Right(resultSet) => resultSet
      }
    }
  }

}
