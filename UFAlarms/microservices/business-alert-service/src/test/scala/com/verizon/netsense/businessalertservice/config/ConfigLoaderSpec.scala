package com.verizon.netsense.businessalertservice.config

import com.verizon.netsense.config.ConfigLoader
import com.verizon.netsense.businessalertservice.util.BaseSpec

/**
 * Created by jittara on 27/02/18.
 */
class ConfigLoaderSpec extends BaseSpec {

  override protected def beforeAll(): Unit =
    super.beforeAll()

  override def afterAll(): Unit =
    super.afterAll()

  "ConfigLoader" should "load the config" in {
    val configUnderTest        = ConfigLoader.config
    val expectSomeGraphiteHost = configUnderTest.getString("graphite.host")
    val expectSomeGraphitePort = configUnderTest.getString("graphite.port")
    expectSomeGraphiteHost mustNot be(null)
    expectSomeGraphitePort mustNot be(null)

  }

}
