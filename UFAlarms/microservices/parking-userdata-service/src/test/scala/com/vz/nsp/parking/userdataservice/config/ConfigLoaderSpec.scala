package com.vz.nsp.parking.userdataservice.config

import com.vz.nsp.parking.config.ServiceConfig
import com.vz.nsp.parking.userdataservice.util.BaseSpec


class ConfigLoaderSpec extends BaseSpec {

  override protected def beforeAll(): Unit =
    super.beforeAll()

  override def afterAll(): Unit =
    super.afterAll()

  "ConfigLoader" should "load the config" in {
    val configUnderTest        = ServiceConfig.config
    val expectSomeGraphiteHost = configUnderTest.getString("graphite.host")
    val expectSomeGraphitePort = configUnderTest.getString("graphite.port")
    expectSomeGraphiteHost mustNot be(null)
    expectSomeGraphitePort mustNot be(null)

  }

}
