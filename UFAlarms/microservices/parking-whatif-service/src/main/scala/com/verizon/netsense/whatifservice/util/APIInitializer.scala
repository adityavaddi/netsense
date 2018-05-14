package com.verizon.netsense.whatifservice.util

import java.util.Properties

import com.verizon.netsense.utils.Logging
import com.verizon.netsense.whatifservice.model.casel.{UserProps, _}
import org.apache.kafka.clients.producer.{KafkaProducer, ProducerRecord}

object APIInitializer extends Logging {

  implicit val ec = scala.concurrent.ExecutionContext.Implicits.global

  sealed trait Application

  case object WhatIFPhantom extends Application


  /**
    * This call is to fix the bug NSN-10768 (After deployment first api fails)
    * What If is api service so this method will initialize a warm-up api
    * In long run we will find the root cause and fix the issue
    */
  def apiInitializer(application: Application, topic: String, bootstrapservers: String,
                     responseTopic: String) = {
    log.info("Initializing warmup API")
    application match {
      case WhatIFPhantom =>
        publishToKafka(topic, bootstrapservers, generateAppRequest(responseTopic, "getAllWhatIfJobs"))
      case _ => log.error("Unknown service")
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
    val userProps = UserProps(Some("userEmail"))
    val requestBody = RequestBody(instanceid = "instanceid", requestid = "requestid", timestamp = "time", `type` = reqtype,
      model = "model", action = "action", user = None, orgprops = orgProps, siteprops = siteProps,userprops = userProps )
    val appRequest = AppRequest("messageid", responseTopic, requestBody)
    ObjectMapperUtil.toJson(appRequest)
  }
}
