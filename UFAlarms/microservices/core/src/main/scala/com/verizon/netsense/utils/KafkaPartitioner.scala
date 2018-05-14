package com.verizon.netsense.utils

import java.util

import org.apache.kafka.clients.producer.Partitioner
import org.apache.kafka.clients.producer.internals.DefaultPartitioner
import org.apache.kafka.common.Cluster

/**
 * Created by brefsdal on 4/12/17.
 */
class KafkaPartitioner extends Partitioner {

  override def configure(configs: util.Map[String, _]) = ???

  override def partition(topic: String,
                         key: Any,
                         keyBytes: Array[Byte],
                         value: Any,
                         valueBytes: Array[Byte],
                         cluster: Cluster): Int = ???

  override def close(): Unit = ???
}
