package com.vz.ns.ts.service.config

import com.typesafe.config.ConfigFactory
import com.vz.ns.ts.service.model.{TsProvider, TsProviderRoute}

object ConfigLoader {

  private val config = ConfigFactory.load()

  // AMQP configuration
  lazy val amqp        = config.getConfig("amqpp")
  lazy val queueName   = amqp.getString("queue-name")
  lazy val rmqusername = amqp.getString("username")
  lazy val rmqpassword = amqp.getString("password")
  lazy val rmqhost     = amqp.getString("rmqhost")
  lazy val rmqport     = amqp.getInt("rmqport")

  //TS configuration
  lazy val ts                = config.getConfig("ts")
  lazy val tsNorthApi        = ts.getString("north-api")
  lazy val tsSouthApi        = ts.getString("south-api")
  lazy val clientId          = ts.getString("netsense-client-id")
  lazy val clientSecret      = ts.getString("netsense-client-secret")
  lazy val scope             = ts.getString("scope")
  lazy val tsAccountUserName = ts.getString("netsense-acc-user-name")
  lazy val tsAccountPswd     = ts.getString("netsense-acc-pwd")
  lazy val tsAccountScope    = ts.getString("user-scope")
  lazy val tsEvents          = tsNorthApi + ts.getString("events")

  //REST SERVICES configuration
  lazy val http     = config.getConfig("http")
  lazy val httpHost = http.getString("host")
  lazy val httpPort = http.getInt("port")

  //PROVIDER configuration
  lazy val provider = config.getConfig("provider")

  val inventory = new TsProviderRoute(
    Some(provider.getConfig("inventory").getString("alias")),
    Some(provider.getConfig("inventory").getString("basePath")),
    Some(provider.getConfig("inventory").getString("relativePath")),
    Some(provider.getConfig("inventory").getString("hostAndPort"))
  )

  val providerInfo = new TsProvider(
    Some(provider.getString("id")),
    Some(provider.getString("devicekind")),
    Some(provider.getString("kind")),
    Some(provider.getString("version")),
    Some(provider.getString("name")),
    Some(provider.getString("description")),
    Some(inventory),
    null,
    null
  )

  // EVENT configuration
  lazy val event         = config.getConfig("event")
  lazy val eventDeviceId = event.getString("deviceid")
  lazy val eventState    = event.getString("state")
  lazy val eventKind     = event.getString("kind")
  lazy val eventVersion  = event.getString("version")

  // DEVICE configuration
  lazy val device         = config.getConfig("device")
  lazy val devicekind     = device.getString("kind")
  lazy val deviceversion  = device.getString("version")
  lazy val providerid     = device.getString("providerid")
  lazy val queryForFilter = device.getString("queryForFilter")
  lazy val deviceState    = device.getString("state")

  // kafka producer
  lazy val kafka       = config.getConfig("kafka")
  lazy val hostAndPort = kafka.getString("hostAndPort")
  lazy val kafkaTopic  = kafka.getString("topic")

  // Cassandra Properties
  lazy val cassandra    = config.getConfig("cassandra")
  val cassandraHost     = cassandra.getString("host")
  val cassandraPort     = cassandra.getInt("port")
  val cassandraKeyspace = cassandra.getString("keyspace")
  val cassandraTable    = cassandra.getString("table")

  // constants
  lazy val nodeid     = "nodeid"
  lazy val name       = "name"
  lazy val sensorType = "sensor"

  // graphite configuration
  lazy val graphiteConfig = config.getConfig("graphite")
  lazy val graphiteHost   = graphiteConfig.getString("host")
  lazy val graphitePort   = graphiteConfig.getInt("port")
}
