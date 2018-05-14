package com.vz.nsp.datasample.service.db

import org.scalatest._
import org.scalatest.concurrent.ScalaFutures

/**
 * Created by nalamte on 5/24/17.
 */
trait DatabaseSpec
    extends FlatSpecLike
    with MustMatchers
    with Inspectors
    with ScalaFutures
    with OptionValues
    with BeforeAndAfterAll
    with BeforeAndAfterEach
