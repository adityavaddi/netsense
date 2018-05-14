package com.verizon.netsense.util

import org.scalatest._
import org.scalatest.concurrent.ScalaFutures
import org.specs2.mock.Mockito

/**
 * Created by maidapr on 4/10/17.
 */
abstract class BaseSpec
    extends FlatSpecLike
    with MustMatchers
    with Mockito
    with Inspectors
    with ScalaFutures
    with OptionValues
    with BeforeAndAfterAll
