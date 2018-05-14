package com.vz.nsp.parking.policyservice.util

import org.scalatest._
import org.scalatest.concurrent.ScalaFutures
import org.specs2.mock.Mockito

abstract class BaseSpec
    extends FlatSpecLike
    with MustMatchers
    with Mockito
    with Inspectors
    with ScalaFutures
    with OptionValues
    with BeforeAndAfterAll
