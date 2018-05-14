package com.vz.nsp.parking.util

import java.util.Properties

import com.verizon.netsense.utils.Logging
import com.vz.nsp.parking.model._
import org.apache.kafka.clients.producer.{KafkaProducer, ProducerRecord}

import scala.concurrent.ExecutionContext

object APIInitializer extends Logging {

  implicit val ec = ExecutionContext.Implicits.global

  sealed trait Application

  case object PolicyPhantom extends Application

  case object TagPhantom extends Application

  case object UserdataPhantom extends Application

  case object GroupPolicyPhantom extends Application

  /**
    * This call is to fix the bug NSN-10768 (After deployment first parking optimization api fails)
    * All Parking apps are api services so this method will initialize a warm-up api
    * In long run we will find the root cause and fix the issue
    */
  def apiInitializer(application: Application, topic: String, bootstrapservers: String,
                     responseTopic: String) = {
    log.info("Initializing warmup API")
    application match {
      case PolicyPhantom =>
        publishToKafka(topic, bootstrapservers, generateAppRequest(responseTopic, "getAllParkingPolicy"))
      case TagPhantom =>
        publishToKafka(topic, bootstrapservers, generateAppRequest(responseTopic, "getAllPolicyCategory"))
      case UserdataPhantom =>
        publishToKafka(topic, bootstrapservers, generateAppRequest(responseTopic, "getAllAppUserData"))
      case GroupPolicyPhantom =>
        publishToKafka(topic, bootstrapservers, generateAppRequest(responseTopic, "getAllMetadataForParkingSpot"))
      case _ => log.error("Unknown parking service")
    }
  }

  def publishToKafka(topic: String, bootstrapservers: String, message: String) = {
    val props = new Properties()
    props.put("bootstrap.servers", bootstrapservers)
    props.put("key.serializer", "org.apache.kafka.common.serialization.StringSerializer")
    props.put("value.serializer", "org.apache.kafka.common.serialization.StringSerializer")
    val producer = new KafkaProducer[String, String](props)
    log.info("sending sample request to topic " + topic)
    val record = new ProducerRecord(topic, "key", message)
    producer.send(record)
    producer.close()
  }

  def generateAppRequest(responseTopic: String,
                         reqtype: String): String = {
    val orgProps = OrgProps("orgId")
    val siteProps = SiteProps("siteId")
    val appUserDataProps = AppUserDataProps(None, "userid", "appid", None)
    val configProps =
      ConfigProps(None, None, None, None, None, None, None, None, None, None, None, None, None, None)
    val requestBody = RequestBody(instanceid = "instanceid", requestid = "requestid", timestamp = "time", `type` = reqtype,
      model = "model", action = "action", user = None, orgprops = orgProps, siteprops = siteProps, configprops = configProps,
      appuserdataprops = appUserDataProps, parkingspaceids = Some(Set("space1")))
    val appRequest = AppRequest("messageid", responseTopic, requestBody)
    ObjectMapperUtil.toJson(appRequest)
  }
}
