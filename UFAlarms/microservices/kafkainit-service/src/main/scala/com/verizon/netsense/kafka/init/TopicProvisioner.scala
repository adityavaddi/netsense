package com.verizon.netsense.kafka.init

import java.util.Properties

import com.verizon.netsense.kafka.init.ConfigLoader.{envSuffix, _}
import com.verizon.netsense.utils.Logging
import kafka.admin.AdminUtils
import kafka.utils.ZkUtils
import org.I0Itec.zkclient.{ZkClient, ZkConnection}

object TopicProvisioner extends Logging{

  val zkClient: ZkClient = new ZkClient(zkHost, zkSessionTimeout, zkConnectionTimeout,  ZKStringSerializer)
  val zkUtils = new ZkUtils(zkClient, new ZkConnection(zkHost), false)

  def create(topicName: String, partitions: Int = 1, replicationFactor: Int = 1): Unit = {
    log.info(s"Creating Kafka topic ${topicName} ...")
    if (AdminUtils.topicExists(zkUtils, topicName)) {
      log.warn(s"Kafka topic already exists ${topicName}")
    } else {
      AdminUtils.createTopic(zkUtils, topicName, partitions, replicationFactor, new Properties())
      log.info(s"Topic created successfully. Topic Name:${topicName}, Partitions:${partitions},ReplicationFactor:${replicationFactor}")
    }
  }

  def closeConnection(): Unit ={
    log.info("Closing the ZK connection")
    zkClient.close()
  }

}
