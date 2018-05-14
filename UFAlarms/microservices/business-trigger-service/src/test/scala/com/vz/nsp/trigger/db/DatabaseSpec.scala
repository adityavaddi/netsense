package com.vz.nsp.trigger.db

/**
  * Created by maleva on 2/20/18.
  */
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures


trait DatabaseSpec
  extends FlatSpecLike
    with MustMatchers
    with Inspectors
    with ScalaFutures
    with OptionValues
    with BeforeAndAfterAll
    with BeforeAndAfterEach
