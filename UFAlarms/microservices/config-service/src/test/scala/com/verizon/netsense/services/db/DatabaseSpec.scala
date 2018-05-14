package com.verizon.netsense.services.db

import org.scalatest._
import org.scalatest.concurrent.ScalaFutures

abstract class DatabaseSpec
    extends FlatSpec
    with MustMatchers
    with Inspectors
    with ScalaFutures
    with OptionValues
    with BeforeAndAfterAll
