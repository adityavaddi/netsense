package com.verizon.netsense.services.datamigration.migration

import java.io.{FileNotFoundException, IOException}

import com.fasterxml.jackson.annotation.JsonInclude.Include
import com.fasterxml.jackson.core.JsonParseException
import com.fasterxml.jackson.databind.{DeserializationFeature, ObjectMapper}
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper
import com.verizon.netsense.services.datamigration.connectors.CassandraConnector.session
import com.verizon.netsense.services.datamigration.model.{RawUFAlarm, UfAlarm}
import com.verizon.netsense.services.datamigration.utils.Configuration._

import scala.io.{BufferedSource, Source}

object UFAlarmLoader extends Migration {
  private var ufAlarmsCounter = 0

  def loadUFAlarms(): Unit = {
    try {
      val rawFileData: BufferedSource = Source.fromResource(ufAlarmsFile)
      val ufAlarmJson = mapper.readValue[List[RawUFAlarm]](rawFileData.reader())
      val ufAlarmMap = getUFAlarmMap(ufAlarmJson)
      ufAlarmMap.foreach { ufAlarm =>
        val queries = getInsertQuery(ufAlarm._2)
        log.debug("Uf Alarm By ID Insert Query: " + queries._2)
        log.debug("Uf Alarm Insert Query: " + queries._1)
        session.execute(queries._1)
        session.execute(queries._2)
        ufAlarmsCounter = ufAlarmsCounter + 1
      }
      rawFileData.close()
      printUFAlarmsCounters
    } catch {
      case ex: FileNotFoundException =>
        log.error(s"Couldn't find the file: $ufAlarmsFile. in resources directory\n" + ex); throw ex;
      case ex: IOException             => log.error("Got an IOException!" + ex); throw ex;
      case ex: JsonParseException      => log.error("Error in parsing JSON: " + ex); throw ex;
      case ex: InvalidUFAlarmException => log.error(ex.getMessage); throw ex;
      case ex: Exception               => log.error(ex.getMessage); throw ex;
    }
  }

  val mapper = new ObjectMapper() with ScalaObjectMapper
  mapper.registerModule(DefaultScalaModule)
  mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false)
  mapper.setSerializationInclusion(Include.NON_ABSENT)

  private def formatList(l: Set[String]): String =
    l.map(x => s"'${x}'").mkString("{", ", ", "}")

  def getUFAlarmMap(rawUfAlarmList: List[RawUFAlarm]) = {
    val ufAlarmList = rawUfAlarmList.map { rawUfAlarm =>
      log.debug("UfAlarm: " + rawUfAlarm)
      UfAlarm.apply(rawUfAlarm)
    }
    ufAlarmList
      .map { ufAlarm =>
        log.debug("UfAlarm: " + ufAlarm)
        val key = ufAlarm.alarmtype
        val value = ufAlarm
        (key, value)
      }
      .toMap[String, UfAlarm]
  }

  def getInsertQuery(ufAlarm: UfAlarm): (String, String) = {

    val colNames = "(mappingid, alarmtype, nodemodels, ufname, description, " +
      "displaytopartner, displaytocustomer, created, updated)"

    val ufAlarmQuery = s"""
                       | INSERT INTO $keyspace.$uf_alarms ${colNames} VALUES
                       | ('${ufAlarm.mappingid}','${ufAlarm.alarmtype}',${formatList(ufAlarm.nodemodels)},
                       | '${ufAlarm.ufname}','${ufAlarm.description}', ${ufAlarm.displaytopartner},
                       | ${ufAlarm.displaytopartner}, ${ufAlarm.created}, ${ufAlarm.updated}
                       | );
                       | """.stripMargin

    val ufAlarmByIdQuery = s"""
                           | INSERT INTO $keyspace.$uf_alarmsById ${colNames} VALUES
                           | ('${ufAlarm.mappingid}','${ufAlarm.alarmtype}',${formatList(ufAlarm.nodemodels)},
                           | '${ufAlarm.ufname}','${ufAlarm.description}', ${ufAlarm.displaytopartner},
                           | ${ufAlarm.displaytopartner}, ${ufAlarm.created}, ${ufAlarm.updated}
                           | );
                           | """.stripMargin


    (ufAlarmQuery, ufAlarmByIdQuery)
  }

  def printUFAlarmsCounters() = log.info(s"UF Alarm Records loaded to cassandra: $ufAlarmsCounter. ")

  override def migrate(): Unit = {
    loadUFAlarms()
  }
}

class InvalidUFAlarmException(
    message: String = "Required values are missing from UfAlarm Props",
    cause: Throwable = None.orNull)
    extends Exception(message, cause)
