package com.verizon.netsense.database

import java.util

import akka.actor.{ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}
import com.verizon.netsense.entity.Message
import org.scalatest.{BeforeAndAfterAll, FlatSpec, Matchers, WordSpecLike}

/**
 * Created by brefsdal on 3/17/17.
 */
//class CustomerSpec extends TestKit(ActorSystem("CustomerSpec")) with ImplicitSender with FlatSpec
//  with Matchers with BeforeAndAfterAll {

class CustomerSpec extends FlatSpec with Matchers {

  //override def afterAll: Unit = {
  //  TestKit.shutdownActorSystem(system)
  // }

  "Customer" should "add customer" in {

    val orgprops = new util.HashMap[String, Object]()
    orgprops.put("orgid", "foo")

    val props = new util.HashMap[String, Object]()
    props.put("props", orgprops)

    //Customer.addCustomer(props)
  }
}
