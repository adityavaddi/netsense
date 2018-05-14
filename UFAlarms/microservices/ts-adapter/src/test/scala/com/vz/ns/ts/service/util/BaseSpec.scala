package com.vz.ns.ts.service.util

import org.scalamock.scalatest.MockFactory
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures

abstract class BaseSpec
    extends FlatSpec
    with MustMatchers
    with MockFactory
    with Inspectors
    with ScalaFutures
    with OptionValues
    with BeforeAndAfterAll
