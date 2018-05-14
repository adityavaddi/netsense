package com.verizon.netsense.config

import com.verizon.netsense.util.BaseSpec

/**
 * Created by maidapr on 5/5/17.
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
