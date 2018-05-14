package com.vz.ns.ts.service.config

import com.vz.ns.ts.service.util.BaseSpec

/**
 *  Created by dasarst on 5/09/17.
 */
class ConfigLoaderTest extends BaseSpec {

  /**
   * Test the AMQP config
   */
  " AMQP Configuration " should "should load from the configuration" in {

    ConfigLoader.amqp mustNot equal(null)

  }

  /**
   * Test the TS config
   */
  " TS Configuration " should "should load from the configuration" in {

    ConfigLoader.ts mustNot equal(null)

  }

  /**
   * Test the REST SERVICES config
   */
  " REST SERVICES Configuration " should "should load from the configuration" in {

    ConfigLoader.http mustNot equal(null)

  }

  /**
   * Test the PROVIDER configuration config
   */
  " PROVIDER Configuration " should "should load from the configuration" in {

    ConfigLoader.provider mustNot equal(null)
  }

  /**
   * Test the EVENT configuration  config
   */
  " EVENT Configuration " should "should load from the configuration" in {

    ConfigLoader.event mustNot equal(null)
  }

  /**
   * Test the DEVICE configuration  config
   */
  " DEVICE Configuration " should "should load from the configuration" in {

    ConfigLoader.device mustNot equal(null)
  }

  /**
   * Test the kafka producer  config
   */
  " kafka producer " should "should load from the configuration" in {

    ConfigLoader.kafka mustNot equal(null)
  }
}
