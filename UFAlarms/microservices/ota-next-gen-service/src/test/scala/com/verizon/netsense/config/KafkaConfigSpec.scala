package com.verizon.netsense.config

import org.scalatest._
import org.scalatest.concurrent.ScalaFutures

class KafkaConfigLoaderSpec
  extends FunSuiteLike
    with MustMatchers
    with ScalaFutures
    with BeforeAndAfterAll
    with BeforeAndAfterEach {

  override protected def beforeAll(): Unit =
    super.beforeAll()

  override def afterAll(): Unit =
    super.afterAll()

  test("Should load Kafka configs") {
    assert(OTAKafkaConfig.otaTopic == "ms.ota.job")
  }
}
