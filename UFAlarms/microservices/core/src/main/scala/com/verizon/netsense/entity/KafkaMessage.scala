package com.verizon.netsense.entity

/**
 * Created by davor on 5/25/17.
 */
case class KafkaMessage(sendTo: String, msg: Array[Byte]) extends Entity
