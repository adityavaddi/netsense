package com.verizon.netsense.helper

import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{BeforeAndAfterEach, _}

/**
  * Created by maidapr on 1/14/18.
  */
trait BaseSpec extends FlatSpecLike
  with MustMatchers
  with Inspectors
  with ScalaFutures
  with OptionValues
  with BeforeAndAfterAll
  with BeforeAndAfterEach {

  def time[R](block: => R): R = {
    val t0 = System.currentTimeMillis()
    val result = block
    println("Elapsed time: " + (System.currentTimeMillis() - t0) + "ms")
    result
  }
}