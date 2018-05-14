package com.verizon.netsense.helper

import java.time.{Instant, ZoneId}
import java.util

import com.verizon.netsense.connector.Neo4jConnector
import com.verizon.netsense.exceptions.CustomExceptions.{NodeModelNotFoundException, SiteTimeZoneNotFoundException}
import com.verizon.netsense.model.{SensorSamplePayload, SensorSampleQueryPayload, _}
import com.verizon.netsense.utils.TimeConverter

import scala.concurrent.{ExecutionContext, Future}
import scalaz.NonEmptyList

/**
  * Created by maidapr on 6/1/17.
  */
object EventWrapper extends Common with TimeConverter{


  implicit class NullOccludingMap[K, V](private val underlying: Map[K, V]) extends AnyVal{
    def getNonNullOrElse(key: K, ex: Throwable): Some[V] ={
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

  @deprecated
  def fetchNodeTypeSiteTimeZone(rQ: SensorQueryEnvelope)
                               (implicit ec: ExecutionContext): Future[QueryRequestBinded] =
    Future {
      val cypher = "MATCH(s:Site {siteid:{siteId}})-[:HAS]-> " +
        "(n:Node {nodeid:{nodeId}})" +
        " RETURN " +
        " n.model AS model," +
        " s.time_zone AS time_zone"

      val map = new util.HashMap[String, Object]
      map.putIfAbsent("siteId", rQ.request.siteprops.siteid)
      map.putIfAbsent("nodeId", rQ.request.nodeprops.nodeid)
      Neo4jConnector.execute(cypher, map).toEither match {
        case Left(l) => throw l
        case Right(r) =>
          val resultMap = r.mapValues(_.toString)
          QueryRequestBinded(rQ,
            NodeModelTimeZone(resultMap.getNonNullOrElse("model",
              NodeModelNotFoundException(rQ.toJSON)),
              resultMap.getNonNullOrElse("time_zone",
                SiteTimeZoneNotFoundException(rQ.toJSON))))
      }
    }

  def convertSensorValueWithTimeStamp(c: (Long, Double),
                                      rQ: SensorQueryEnvelope,
                                      timeZone: ZoneId,
                                      sensorId: String): SensorSamplePayload =
    SensorSamplePayload(sensorId, Instant.ofEpochMilli(c._1.toLong / 1000).atZone(timeZone).format(ISODateFormat), c._2)

  /**
    * Helper to Fold the response from the DB call to the ResponsePayload
    *
    * @param rQ - RequestQuery received from the IS and NodeTimeZoneType from GraphDb
    * @param eL - EventList received from DB Query
    * @param ec - Execution Context
    * @return Future[ResponseMsg]
    */
  def buildResponsePayload(rQ: (SensorQueryEnvelope, NodeModelTimeZone), eL: Future[List[List[(Long, Double)]]])(
    implicit ec: ExecutionContext
  ): Future[(SensorQueryEnvelope, SensorSampleQueryResponse)] = {
    buildResponseMessage(
      rQ._1,
      eL.map {
        case x@Nil => Vector[SensorSamplePayload]()
        case x =>
          val timeZone = ZoneId.of(rQ._2
            .timeZone.getOrElse("UTC"))
          mergeMultipleSensorResults(rQ, x, timeZone)
      }
    )
  }

  def mergeMultipleSensorResults(rQ: (SensorQueryEnvelope, NodeModelTimeZone),
                                 l: List[List[(Long, Double)]], timeZone: ZoneId): Vector[SensorSamplePayload] = {
    if (l.size > 1) {
      val sensorList: Array[String] = rQ._1.request.extprops.sensorid.split(",")
      val zippedDatapoints = l.zip(sensorList)
      zippedDatapoints.foldLeft(Vector[SensorSamplePayload]()) { (a, c) =>
        c._1.headOption match {
          case None => a
          case Some(dataPoint) => a.:+(convertSensorValueWithTimeStamp(dataPoint, rQ._1, timeZone, c._2))
        }
      }
    }
    else {
      l.headOption match {
        case None => Vector[SensorSamplePayload]()
        case Some(dataPointList) => buildSensorPayloadInParallel(dataPointList, rQ._1, timeZone)
      }
    }
  }

  def buildSensorPayloadInParallel(datapoints: List[(Long, Double)], sqe: SensorQueryEnvelope, timeZone: ZoneId): Vector[SensorSamplePayload] = {
    val datapointsAsVector = datapoints.toVector
    datapointsAsVector.par.foldLeft(Vector[SensorSamplePayload]()) { (a, c) =>
      a.:+(convertSensorValueWithTimeStamp(c, sqe, timeZone, null))
    }
  }

  def buildNodeResponsePayload(
                                rQ: SensorQueryEnvelope,
                                eL: Future[List[(String, String, String, Double, Double, Double, Double, Double)]]
                              )(implicit ec: ExecutionContext): Future[(SensorQueryEnvelope, SensorEnergyNodeResponse)] =
    buildResponseMessageNode(rQ,
      eL.map(
        x =>
          x.foldLeft(List[EnergySavingsNodePayloadItem]()) { (a, c) =>
            a.:+((EnergySavingsNodePayloadItem.apply _).tupled(c))
          }
      ))

  def buildSiteResponsePayload(
                                rQ: SensorQueryEnvelope,
                                eL: Future[List[(String, String, Double, Double, Double, Double, Double)]]
                              )(implicit ec: ExecutionContext): Future[(SensorQueryEnvelope, SensorEnergySiteResponse)] =
    buildResponseMessageSite(rQ,
      eL.map(
        x =>
          x.foldLeft(List[EnergySavingsSitePayloadItem]()) { (a, c) =>
            a.:+((EnergySavingsSitePayloadItem.apply _).tupled(c))
          }
      ))


  //  def aggregateSensorIdsForResponse(items: List[SensorSamplePayload]): String = items.flatMap(x => x.sensorid).mkString(",")

  /**
    * Helper to construct the ResponseMessage
    *
    * @param rQ - RequestQuery received from the IS
    * @param rP - Response Payload for the Request Query received
    * @param ec - Execution Context
    * @return Future[ResponseMsg]
    */

  def buildResponseMessage(rQ: SensorQueryEnvelope,
                           rP: Future[Vector[SensorSamplePayload]])(implicit ec: ExecutionContext) = {
    rP.map(
      e =>
        (rQ,
          SensorSampleQueryResponse(
            rQ.messageid,
            SensorSampleQueryPayload(rQ.request.requestid,
              success = true,
              rQ.request.timestamp,
              null,
              sensorid = rQ.request.extprops.sensorid,
              items = e)
          ))
    )
  }


  def buildResponseMessageNode(rQ: SensorQueryEnvelope,
                               rP: Future[List[EnergySavingsNodePayloadItem]])(implicit ec: ExecutionContext) =
    rP.map(
      e =>
        (rQ,
          SensorEnergyNodeResponse(
            rQ.messageid,
            SensorEnergyNodePayload(rQ.request.requestid, success = true, rQ.request.timestamp, e)
          ))
    )

  def buildResponseMessageSite(rQ: SensorQueryEnvelope,
                               rP: Future[List[EnergySavingsSitePayloadItem]])(implicit ec: ExecutionContext) =
    rP.map(
      e =>
        (rQ,
          SensorEnergySiteResponse(
            rQ.messageid,
            SensorEnergySitePayload(rQ.request.requestid, success = true, rQ.request.timestamp, e)
          ))
    )

}
