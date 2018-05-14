package com.vz.ns.ts.service.service

import java.util.concurrent.TimeUnit

import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{ContentTypes, HttpMethods, HttpRequest, HttpResponse}
import com.github.nscala_time.time.Imports.DateTime
import com.google.common.base.Charsets
import com.google.common.io.BaseEncoding
import com.verizon.netsense.metrics.Instrumented
import com.vz.ns.ts.service.AdapterApp
import com.vz.ns.ts.service.config.ConfigLoader._
import com.vz.ns.ts.service.db.PhantomService
import com.vz.ns.ts.service.model.{Mapping, TsDevice, TsDeviceInfo}
import com.vz.ns.ts.service.util.CacheUtil.scalaCache
import com.vz.ns.ts.service.util._
import nl.grons.metrics.scala.Timer
import org.apache.kafka.clients.producer.{ProducerRecord, RecordMetadata}

import scala.concurrent.Future
import scala.util.{Failure, Success}
import scalacache._

class TsEventService(tsDeviceUtil: TsDeviceService, httpRequestService: HttpRequestService, _akkaRuntime: AkkaRuntime)
    extends Logging
    with Instrumented {

  import _akkaRuntime._

  private[this] val kafkasourceproducerevent   = metrics.timer("kafka-source-producer-event")
  private[this] val PostEventsUsingRest: Timer = metrics.timer("event-post-rest")

  /**
   * Check for nodeId from payload
   * if present then process otherwise skip
   */
  def checkPayloadForNodeIdAndProcess(payload: Map[String, Any]) = {
    log.info("Entering TsEventUtil::checkPayloadForNodeIdAndProcess")
    payload.get(nodeid) match {
      case Some(value) => masterMethodToPushEventsBasedOnCacheValue(value.toString, payload)
      case None        => log.error("NodeId not found in the payload")
    }
  }

  /**
   * This method depends on cache value for nodeId
   * if value is in progress then it calls inProgressStateWhileOtherProcessCreatingTsDevice
   * if Some(tsid) then calls generatePayloadAndPostEventToTs
   * if None then updates cache with key=>nodeId value=>InProgress to stop other processes to duplicate tsid creation
   */
  private def masterMethodToPushEventsBasedOnCacheValue(nodeId: String, payload: Map[String, Any]): Unit = {
    log.info("Entering TsEventUtil::masterMethodToPushEventsBasedOnCacheValue")
    CacheUtil
      .searchCacheForKeyNodeId(nodeId)
      .foreach(x => {
        x match {
          case Some(tsId) => {
            generatePayloadAndPostEventToTs(tsId, payload)
          }
          case None => {
            queryCasssandraForDeviceIdAndProcess(nodeId, payload)
          }
        }
      })
  }

  /**
   * Query cassandra for tsdeviceid using nodeid
   * if Some(deviceMapping) then update cache and call generatePayloadAndPostEventToTs
   * if none then call searchTsWithRefIdAndPushEvents
   */
  private def queryCasssandraForDeviceIdAndProcess(nodeId: String, payload: Map[String, Any]) = {
    log.info("Entering TsEventUtil::queryCasssandraForDeviceIdAndProcess")
    CassandraUtil
      .searchCassandraForKeyNodeId(nodeId)
      .foreach(x => {
        x match {
          case Some(deviceMapping) => {
            log.debug(s"Result from cassandra => nodeID: $nodeId -> TSDeviceId: $deviceMapping.tsDeviceId")
            generatePayloadAndPostEventToTs(deviceMapping.tsDeviceId, payload)
            //UPDATE CACHE
            put(nodeId)(deviceMapping.tsDeviceId)
          }
          case None => {
            log.debug(s"Result from cassandra => nodeID: $nodeId -> TSDeviceId: None")
            searchTsWithRefIdAndPushEvents(nodeId, payload)
          }
        }
      })
  }

  /**
   * Query cassandra for all tsdeviceid/nodeid mapping
   */
  def getAllMapping = {
    log.info("Entering TsEventUtil::getAllMapping")
    CassandraUtil.getAllMapping.foreach(x => {
      x.foreach(
        mapList =>
          mapList match {
            case deviceMapping: Mapping => {
              log.debug(
                s"Result from cassandra => nodeID: $deviceMapping.nodeId -> TSDeviceId: $deviceMapping.tsDeviceId"
              )
              put(deviceMapping.nodeId)(deviceMapping.tsDeviceId)
            }
        }
      )
    })
  }

  /**
   * Search TS with refId as nodeId
   * If present then call generatePayloadAndPostEventToTs
   * else call createTsDeviceAssignAndPush
   */
  private def searchTsWithRefIdAndPushEvents(nodeId: String, payload: Map[String, Any]) = {
    log.info("Entering TsEventUtil::searchTsWithRefIdAndPushEvents")
    val id: Future[Option[String]] = tsDeviceUtil.getTsDeviceByRefId(nodeId)
    id.foreach(x => {
      x match {
        case Some(tsId) => {
          log.debug(s"Result from TS Query => nodeID: $nodeId -> TSDeviceId: $tsId")
          //UPDATE CACHE AND CASSANDRA
          log.debug(s"Updating cache and cassandra for nodeID: $nodeId => tsDeviceID: $tsId")
          PhantomService.storeEvent(mapping(nodeId, tsId))
          put(nodeId)(tsId)

          generatePayloadAndPostEventToTs(tsId, payload)
        }
        case None => {
          log.debug(s"Result from TS Query => nodeID: $nodeId -> TSDeviceId: None")
          createTsDeviceAssignAndPush(nodeId, payload)
        }
      }
    })
  }

  /**
   *
   * It builds tsDeviceInfo and calls createDevice to create device in ts
   * Calls generatePayloadAndPostEventToTs to publish events
   */
  private def createTsDeviceAssignAndPush(nodeId: String, payload: Map[String, Any]): Unit = {
    log.info("Entering TsEventUtil::createTsDeviceAssignAndPush")
    val tsDeviceInfo = TsDeviceInfo(
      Some(devicekind),
      Some(deviceversion),
      Some(payload.get(name).getOrElse("dummyname").toString).orElse(null),
      Some(providerid),
      Some(nodeId),
      Some(payload.get(sensorType).getOrElse("dummySensorType").toString).orElse(null),
      deviceState,
      AdapterApp.accountId.get
    )

    //CREATING DEVICE
    val device: Future[TsDevice] = tsDeviceUtil.createDevice(tsDeviceInfo)
    device.foreach(tsDevice => {
      log.debug("TS Device id after creating: " + tsDevice.id.get)

      //Persisting NODEID->TSDEVICEID to DB
      PhantomService.storeEvent(mapping(nodeId, tsDevice.id.get))

      //ADDING NODEID->TSDEVICEID to CACHE
      put(nodeId)(tsDevice.id.get)

      generatePayloadAndPostEventToTs(tsDevice.id.get.toString, payload)
    })
  }

  /**
   *
   * It builds ts event payload
   * calls postEventToTS to publish event
   */
  private def generatePayloadAndPostEventToTs(tsDeviceId: String, payload: Map[String, Any]): Unit = {

    log.info("Entering TsEventUtil::generatePayloadAndPostEventToTs")
    log.debug("##########Posting to TS device id: " + tsDeviceId)
    val tsEventPayload = Map("deviceid" -> tsDeviceId,
                             "state"   -> eventState,
                             "kind"    -> eventKind,
                             "version" -> eventVersion,
                             "fields"  -> payload)

    this.pushEventsToTSKafka(tsEventPayload)
    //postEventToTsUsingRestPost(tsEventPayload)
  }

  /*
   * This function is used to post events to TS Kafka
   *  - get the payload
   *  - transform to json object
   *  - push to kafka events topic
   */
  private def pushEventsToTSKafka(payload: Map[String, Any]): Unit = kafkasourceproducerevent.time {
    log.info("Entering TsEventUtil::postEventToTS")
    //kafkasourceproducerevent.count
    val kafkaPayload: Map[String, Any] = Map {
      "data" -> BaseEncoding.base64().encode(ObjectMapperUtil.toJson(payload).getBytes(Charsets.UTF_8))
    }
    log.trace("payload to ts kafka => " + ObjectMapperUtil.toJson(kafkaPayload))
    val data                                                = new ProducerRecord[String, String](kafkaTopic, ObjectMapperUtil.toJson(kafkaPayload))
    val result: java.util.concurrent.Future[RecordMetadata] = KafkaPropertiesUtil.producer.send(data)

    try {
      result.get(2, TimeUnit.SECONDS)
    } catch {
      case x: Exception => log.error("Exception while posting to TS Kafka - " + x.getMessage)
      case _            => log.error("Error posting to TS Kafka")
    }
  }

  /*
   * Deprecated Method
   * This function is used to post events to TS
   *  - get the payload
   *  - transform to json object
   *  - make a POST call to TS API
   *  - get success/failure message to validate the request
   */

  private def postEventToTsUsingRestPost(payload: Map[String, Any]): Unit = {
    log.info("Entering TsEventUtil::postEventToTsUsingRestPost")
    log.trace("final payload to TS => " + payload)
    PostEventsUsingRest.time()
    val request: Future[HttpResponse] = Http().singleRequest {
      HttpRequest(method = HttpMethods.POST, uri = s"$tsSouthApi/south/v2/events")
        .withEntity(ContentTypes.`application/json`, ObjectMapperUtil.toJson(payload))
    }
    request onComplete {
      case Success(content) => {
        log.debug("Successful response from posting event to TS -> " + content)
      }
      case Failure(t) => {
        log.debug("Error occurred while posting events to TS -> " + t.getMessage)
      }
    }
  }

  private def mapping(nodeId: String, tsDeviceId: String) = Mapping(
    tsDeviceId = tsDeviceId,
    nodeId = nodeId,
    createdOn = DateTime.now()
  )
}

object TsEventService {
  def apply(tsDeviceUtil: TsDeviceService,
            httpRequestService: HttpRequestService,
            _akkaRuntime: AkkaRuntime): TsEventService =
    new TsEventService(tsDeviceUtil, httpRequestService, _akkaRuntime)
}
