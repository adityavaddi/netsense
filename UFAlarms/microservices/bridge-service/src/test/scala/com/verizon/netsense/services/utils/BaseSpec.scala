package com.verizon.netsense.services.utils

import org.scalatest._
import org.scalatest.concurrent.ScalaFutures

abstract class BaseSpec
    extends FlatSpecLike
    with MustMatchers
    with Inspectors
    with ScalaFutures
    with OptionValues
    with BeforeAndAfterAll
