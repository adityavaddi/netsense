package com.verizon.netsense.businessalertservice.util

import org.scalatest._
import org.scalatest.concurrent.ScalaFutures
import org.specs2.mock.Mockito

/**
 * Created by jittara on 02/27/18.
 */
trait BaseSpec
    extends FlatSpecLike
    with MustMatchers
    with Mockito
    with Inspectors
    with ScalaFutures
    with OptionValues
    with BeforeAndAfterAll



