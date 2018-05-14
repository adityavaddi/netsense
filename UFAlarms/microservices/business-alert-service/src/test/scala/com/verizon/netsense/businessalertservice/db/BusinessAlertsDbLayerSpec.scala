package com.verizon.netsense.businessalertservice.db

import akka.actor.ActorSystem
import akka.testkit.TestKit
import com.outworkers.phantom.dsl.ResultSet
import com.outworkers.phantom.dsl._
import com.verizon.netsense.businessalertservice.data.TestDataFeed._
import com.verizon.netsense.businessalertservice.util.BaseSpec
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

class BusinessAlertsDbLayerSpec
    extends TestKit(ActorSystem("Business-Test-System"))
    with BaseSpec
    with ProductionDatabase {

  object DatabaseService {
    def init() = {
      val create = Future.sequence(
        List(
          database.BusinessAlertTable.create.ifNotExists().future(),
          database.BusinessAlertHistoryTable.create.ifNotExists().future(),
          database.BusinessTriggerTable.create.ifNotExists().future()
        )
      )
      Await.ready(create, 5.seconds)
    }

    def cleanup(): Future[List[ResultSet]] = {
      val truncate = Future.sequence(
        List(
          database.BusinessAlertTable.truncate.future(),
          database.BusinessAlertHistoryTable.truncate().future(),
          database.BusinessTriggerTable.truncate().future()
        )
      )
      Await.ready(truncate, 5.seconds)
    }
  }

  override def beforeAll(): Unit = {
    DatabaseService.init()
    session.init()
  }

  override def afterAll(): Unit =
    DatabaseService.cleanup()

  implicit val ec = ExecutionContext.Implicits.global

  val spyDb = spy(new BusinessAlertsDbLayer(new BAPhantomService with ProductionDatabase {}))

  "BusinessAlertsDbLayer" should "Get All Business Alerts with 1 trigger and 1 alert" in {
    DatabaseService.cleanup()
    Await.result(database.BusinessAlertTable.storeBusinessAlert(businessAlert(businessAlertId, triggerId, resourceId)),
                 remainingOrDefault)
    Await.result(database.BusinessTriggerTable.storeTrigger(buildTrigger(triggerId, List(resourceId))),
                 remainingOrDefault)
    val chain = for {
      dbAlerts <- spyDb.getAllBusinessAlerts(orgId, siteId)
    } yield dbAlerts

    whenReady(chain) { res =>
      assert(res.size == 1)
    }
  }

  it should "Get All Business Alerts with 1 deleted trigger and 1 alert" in {
    DatabaseService.cleanup()
    Await.result(database.BusinessAlertTable.storeBusinessAlert(businessAlert(businessAlertId, triggerId, resourceId)),
                 remainingOrDefault)
    Await.result(
      database.BusinessTriggerTable.storeTrigger(buildTrigger(triggerId, List(resourceId)).copy(isdeleted = true)),
      remainingOrDefault
    )
    val chain = for {
      dbAlerts <- spyDb.getAllBusinessAlerts(orgId, siteId)
    } yield dbAlerts

    whenReady(chain) { res =>
      assert(res.isEmpty)
    }
  }

  it should "Get All Business Alerts should choose latest timestamp " +
  "with 1 trigger and 2 alerts(same trigger)" in {
    DatabaseService.cleanup()
    Await.result(database.BusinessAlertTable.storeBusinessAlert(
                   businessAlert("ba1", triggerId, resourceId)
                     .copy(lastUpdated = Some(123l))
                 ),
                 remainingOrDefault)
    Await.result(database.BusinessAlertTable.storeBusinessAlert(
                   businessAlert("ba2", triggerId, resourceId)
                     .copy(lastUpdated = Some(124l))
                 ),
                 remainingOrDefault)
    Await.result(database.BusinessTriggerTable.storeTrigger(buildTrigger(triggerId, List(resourceId))),
                 remainingOrDefault)
    val chain = for {
      dbAlerts <- spyDb.getAllBusinessAlerts(orgId, siteId)
    } yield dbAlerts

    whenReady(chain) { res =>
      assert(res.size == 1)
      assert(res.head.businessAlertId == "ba2")
    }
  }

  it should "Get All Business Alerts with no trigger and 1 alert" in {
    DatabaseService.cleanup()
    Await.result(database.BusinessAlertTable.storeBusinessAlert(businessAlert(businessAlertId, triggerId, resourceId)),
                 remainingOrDefault)
    val chain = for {
      dbAlerts <- spyDb.getAllBusinessAlerts(orgId, siteId)
    } yield dbAlerts

    whenReady(chain) { res =>
      assert(res.isEmpty)
    }
  }

  it should "Get All Business Alerts with 1 trigger and no alert" in {
    DatabaseService.cleanup()
    Await.result(database.BusinessTriggerTable.storeTrigger(buildTrigger(triggerId, List(resourceId))),
                 remainingOrDefault)
    val chain = for {
      dbAlerts <- spyDb.getAllBusinessAlerts(orgId, siteId)
    } yield dbAlerts

    whenReady(chain) { res =>
      assert(res.isEmpty)
    }
  }

  it should "Get All Business Alerts with 1 trigger(2 resources) and 3 alerts" in {
    DatabaseService.cleanup()
    Await.result(database.BusinessAlertTable.storeBusinessAlert(businessAlert("alert1", "trig1", "res1")),
                 remainingOrDefault)
    Await.result(database.BusinessAlertTable.storeBusinessAlert(businessAlert("alert2", "trig1", "res2")),
                 remainingOrDefault)
    Await.result(database.BusinessAlertTable.storeBusinessAlert(businessAlert("alert3", "trig3", "res3")),
                 remainingOrDefault)
    Await.result(database.BusinessTriggerTable.storeTrigger(buildTrigger("trig1", List("res1", "res2"))),
                 remainingOrDefault)
    val chain = for {
      dbAlerts <- spyDb.getAllBusinessAlerts(orgId, siteId)
    } yield dbAlerts

    whenReady(chain) { res =>
      assert(res.size == 2)
      val alertIds = res.map(_.businessAlertId)
      assert(alertIds.contains("alert1"))
      assert(alertIds.contains("alert2"))
    }
  }

  it should "Get All Business Alerts with 1 trigger(2 resources) and 3 alerts(same triggerid)" in {
    DatabaseService.cleanup()
    Await.result(database.BusinessAlertTable.storeBusinessAlert(businessAlert("alert1", "trig1", "res1")),
                 remainingOrDefault)
    Await.result(database.BusinessAlertTable.storeBusinessAlert(businessAlert("alert2", "trig1", "res2")),
                 remainingOrDefault)
    Await.result(database.BusinessAlertTable.storeBusinessAlert(businessAlert("alert3", "trig1", "res2")),
                 remainingOrDefault)
    Await.result(database.BusinessTriggerTable.storeTrigger(buildTrigger("trig1", List("res1", "res2"))),
                 remainingOrDefault)
    val chain = for {
      dbAlerts <- spyDb.getAllBusinessAlerts(orgId, siteId)
    } yield dbAlerts

    whenReady(chain) { res =>
      assert(res.size == 2)
      val alertIds = res.map(_.businessAlertId)
      assert(alertIds.contains("alert1"))
      assert(alertIds.contains("alert2"))
    }
  }

  it should "Get All Business Alerts with 2 trigger and 4 alerts" in {
    DatabaseService.cleanup()
    Await.result(database.BusinessAlertTable.storeBusinessAlert(businessAlert("alert1", "trig1", "res1")),
                 remainingOrDefault)
    Await.result(database.BusinessAlertTable.storeBusinessAlert(businessAlert("alert2", "trig1", "res2")),
                 remainingOrDefault)
    Await.result(database.BusinessAlertTable.storeBusinessAlert(businessAlert("alert3", "trig2", "res1")),
                 remainingOrDefault)
    Await.result(database.BusinessAlertTable.storeBusinessAlert(businessAlert("alert4", "trig2", "res2")),
                 remainingOrDefault)
    Await.result(database.BusinessAlertTable.storeBusinessAlert(businessAlert("alert5", "trig3", "res1")),
                 remainingOrDefault)
    Await.result(database.BusinessTriggerTable.storeTrigger(buildTrigger("trig1", List("res1", "res2"))),
                 remainingOrDefault)
    Await.result(database.BusinessTriggerTable.storeTrigger(buildTrigger("trig2", List("res1", "res2"))),
                 remainingOrDefault)
    val chain = for {
      dbAlerts <- spyDb.getAllBusinessAlerts(orgId, siteId)
    } yield dbAlerts

    whenReady(chain) { res =>
      assert(res.size == 4)
      val alertIds = res.map(_.businessAlertId)
      assert(alertIds.contains("alert1"))
      assert(alertIds.contains("alert2"))
      assert(alertIds.contains("alert3"))
      assert(alertIds.contains("alert4"))
    }
  }

  it should "Get All Business Alerts without dangling alerts" in {
    DatabaseService.cleanup()
    Await.result(database.BusinessAlertTable.storeBusinessAlert(businessAlert("alert1", "trig1", "res1")),
                 remainingOrDefault)
    Await.result(database.BusinessAlertTable.storeBusinessAlert(businessAlert("alert2", "trig1", "res2")),
                 remainingOrDefault)
    Await.result(database.BusinessAlertTable.storeBusinessAlert(businessAlert("alert3", "trig1", "res1")),
                 remainingOrDefault)
    Await.result(database.BusinessAlertTable.storeBusinessAlert(businessAlert("alert4", "trig1", "res2")),
                 remainingOrDefault)
    Await.result(database.BusinessTriggerTable.storeTrigger(buildTrigger("trig1", List("res1", "res2"))),
                 remainingOrDefault)
    val chain = for {
      dbAlerts <- spyDb.getAllBusinessAlerts(orgId, siteId)
    } yield dbAlerts

    whenReady(chain) { res =>
      assert(res.size == 2)
    }
  }

  /**
   * In code null will not be inserted into DB
   * This spec is only to test phantom optional fields functionality
   */
  it should "insert null into Business alert table and retrieve back" in {
    DatabaseService.cleanup()
    Await.result(database.BusinessAlertTable.storeBusinessAlert(businessAlertWithNull("alert1", "trig1", "res1")),
                 remainingOrDefault)
    Await.result(database.BusinessTriggerTable.storeTrigger(buildTrigger("trig1", List("res1"))), remainingOrDefault)
    val chain = for {
      dbAlerts <- spyDb.getAllBusinessAlerts(orgId, siteId)
    } yield dbAlerts
    whenReady(chain) { res =>
      assert(res.size == 1)
    }
  }

  /**
   * In code null will not be inserted into DB
   * This spec is only to test phantom optional fields functionality
   */
  it should "insert null into Business alert history table" in {
    DatabaseService.cleanup()
    val chain = for {
      dbAlerts <- spyDb.storeBusinessAlertHistory(
        businessAlertHistory("alert1", "Major").copy(triggerCategory = None, triggerName = None)
      )
    } yield dbAlerts
    whenReady(chain) { res =>
      assert(res.equals(true))
    }
  }

  /**
   * ############################################################################################
   * getBusinessAlertByIdSys Spec - This should return businessAlert iff related trigger is alive
   * ############################################################################################
   */
  it should "get business alert sys - positive" in {
    Await.result(database.BusinessAlertTable.storeBusinessAlert(businessAlert("alert1", "trig1", "res1")),
                 remainingOrDefault)
    Await.result(database.BusinessTriggerTable.storeTrigger(buildTrigger("trig1", List("res1"))), remainingOrDefault)
    val chain = for {
      dbAlerts <- spyDb.getBusinessAlertByIdSys(orgId, siteId, "alert1")
    } yield dbAlerts
    whenReady(chain) { res =>
      assert(res.get.businessAlertId == "alert1")
      assert(res.get.triggerId.get == "trig1")
    }
  }

  it should "get business alert sys with deleted trigger - Negative" in {
    Await.result(database.BusinessAlertTable.storeBusinessAlert(businessAlert("alert1", "trig1", "res1")),
                 remainingOrDefault)
    Await.result(database.BusinessTriggerTable.storeTrigger(buildTrigger("trig1", List("res1")).copy(isdeleted = true)),
                 remainingOrDefault)
    val chain = for {
      dbAlerts <- spyDb.getBusinessAlertByIdSys(orgId, siteId, "alert1")
    } yield dbAlerts
    whenReady(chain) { res =>
      assert(res.isEmpty)
    }
  }

  it should "get business alert sys with no trigger in DB - Negative" in {
    Await.result(database.BusinessAlertTable.storeBusinessAlert(businessAlert("alert1", "trig1", "res1")),
                 remainingOrDefault)
    val chain = for {
      dbAlerts <- spyDb.getBusinessAlertByIdSys(orgId, siteId, "alert1")
    } yield dbAlerts
    whenReady(chain) { res =>
      assert(res.isEmpty)
    }
  }

  it should "get business alert sys without alert in DB - Negative" in {
    val chain = for {
      dbAlerts <- spyDb.getBusinessAlertByIdSys(orgId, siteId, "alert1")
    } yield dbAlerts
    whenReady(chain) { res =>
      assert(res.isEmpty)
    }
  }

}
