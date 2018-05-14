package com.vz.nsp.parking.tagservice.config

import com.vz.nsp.parking.config.ServiceConfig
import com.vz.nsp.parking.tagservice.util.BaseSpec

/**
 * Created by maidapr on 5/5/17.
 */
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
