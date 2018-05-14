package com.vz.nsp.parking.common

import org.scalatest.concurrent.ScalaFutures
import org.scalatest._
import org.specs2.mock.Mockito

/**
  * Created by thangth on 10/5/17.
  */
abstract class BaseSpec
  extends FlatSpecLike
    with MustMatchers
    with Mockito
    with Inspectors
    with ScalaFutures
    with OptionValues
    with BeforeAndAfterAll
