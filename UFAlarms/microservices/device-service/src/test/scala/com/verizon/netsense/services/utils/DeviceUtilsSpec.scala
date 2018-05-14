package com.verizon.netsense.services.utils

import com.verizon.netsense.utils.HttpServices
import org.scalatest.concurrent.ScalaFutures
import org.scalatest._

class DeviceUtilsSpec extends FlatSpecLike
  with MustMatchers
  with Inspectors
  with ScalaFutures
  with OptionValues
  with BeforeAndAfterAll {

    it should "Create the route" in {

      //assert(HttpServices.devProvRoute != null)
      HttpServices.route mustNot equal(null)
    }

}
