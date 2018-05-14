package com.verizon.netsense.sse.specs.config

import com.verizon.netsense.sse.config.SSEConfigLoader
import org.scalatest._

class ConfigLoaderSpec extends FlatSpecLike with BeforeAndAfterAll with MustMatchers {
  override protected def beforeAll(): Unit =
    super.beforeAll()

  override def afterAll(): Unit =
    super.afterAll()

  "ConfigLoader" should "load the config" in {
    val configUnderTest        = SSEConfigLoader.config
    val expectSomeGraphiteHost = configUnderTest.getString("graphite.host")
    val expectSomeGraphitePort = configUnderTest.getString("graphite.port")
    expectSomeGraphiteHost mustNot be(null)
    expectSomeGraphitePort mustNot be(null)
  }

  "Kafka Config Loader" should "load the kafka config" in {
    val expectBootStrapper    = SSEConfigLoader.kafkaServersParam
    val expectTopicNames      = SSEConfigLoader.kafkaTopicNamesParam
    val expectFilterTopicName = SSEConfigLoader.sseFilterTopicParam
    expectBootStrapper mustNot be(null)
    expectTopicNames mustBe Set("login", "sensor", "gps", "corenode.sensor", "alert", "connectionstatus")
    expectFilterTopicName mustBe "ms.request.sse"
  }

  "Cassandra Config Loader" should "load the cassandra config" in {
    val configCassandraTest                        = SSEConfigLoader.cassandraConfig
    val expectedcassandraPortParam                 = configCassandraTest.getInt("port")
    val expectedcassandraHostParam                 = configCassandraTest.getString("host")
    val expectedcassandraKeyspaceParam             = configCassandraTest.getString("keyspace")
    val expectedsseLookupTable                     = configCassandraTest.getString("sse_lookup_table")
    val expectedsseStreamSubscribeTtlTable         = configCassandraTest.getString("sse_stream_subscribe_ttl_table")
    val expectedsseStreamBusinessSubscribeTtlTable = configCassandraTest.getString("sse_stream_businnessalerts_table")
    val expectedsseStreamTtl                       = configCassandraTest.getLong("sse_stream_subscribe_ttl")

    expectedcassandraPortParam mustNot be(0)
    expectedcassandraHostParam mustBe "127.0.0.1"
    expectedcassandraKeyspaceParam mustBe "farallones"
    expectedsseLookupTable mustBe "orghierarchy_by_nodeid"
    expectedsseStreamSubscribeTtlTable mustBe "sse_filter"
    expectedsseStreamBusinessSubscribeTtlTable mustBe "sse_filter_businessalerts"
    expectedsseStreamTtl mustNot be(0)

  }

}
