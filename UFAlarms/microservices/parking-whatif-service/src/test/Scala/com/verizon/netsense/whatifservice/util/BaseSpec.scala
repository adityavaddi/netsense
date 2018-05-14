package com.verizon.netsense.whatifservice.util

/**
  * Created by maleva on 4/19/18.
  */
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures
import org.specs2.mock.Mockito


trait BaseSpec
  extends FlatSpecLike
    with MustMatchers
    with Mockito
    with Inspectors
    with ScalaFutures
    with OptionValues
    with BeforeAndAfterAll
