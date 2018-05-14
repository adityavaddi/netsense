package com.vz.nsp.parking.policyservice.db

import org.scalatest._
import org.scalatest.concurrent.ScalaFutures
import org.specs2.mock.Mockito

abstract class DatabaseSpec
    extends FlatSpec
    with MustMatchers
    with Inspectors
    with ScalaFutures
    with OptionValues
    with BeforeAndAfterAll

trait BaseSpec
  extends FlatSpecLike
    with MustMatchers
    with Mockito
    with Inspectors
    with ScalaFutures
    with OptionValues
    with BeforeAndAfterAll
