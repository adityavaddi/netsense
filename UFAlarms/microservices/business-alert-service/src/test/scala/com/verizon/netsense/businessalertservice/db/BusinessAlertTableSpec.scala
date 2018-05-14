package com.verizon.netsense.businessalertservice.db

import java.util.Calendar

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.testkit.TestKit
import com.outworkers.phantom.dsl._
import com.verizon.netsense.businessalertservice.data.TestDataFeed._
import com.verizon.netsense.businessalertservice.util.BaseSpec
import org.apache.cassandra.service.{CassandraDaemon, EmbeddedCassandraService}

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._

/**
 * Created by jittara on 02/28/18.
 */
class BusinessAlertTableSpec
    extends TestKit(ActorSystem("ScheduleServiceHeplerSpec1-System"))
    with BaseSpec
    with EmbeddedDatabase {

  implicit val ec = ExecutionContext.Implicits.global

  implicit val mat = ActorMaterializer()(system)

  lazy val embeddedCassandraServiceObj = new EmbeddedCassandraService
  lazy val cassandraDaemon             = new CassandraDaemon

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

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    val start: Unit = embeddedCassandraServiceObj.start()
    DatabaseService.init()
  }

  override protected def afterAll() = {
    cassandraDaemon.stop()
    cassandraDaemon.stopNativeTransport()
    DatabaseService.cleanup()
    super.afterAll()
  }

  "BusinessAlertHistoryTable" should "Store the alert data in History table" in {

    val future =
      database.BusinessAlertHistoryTable.storeBusinessAlertHistory(businessAlertHistory(businessAlertId, "Minor"))
    whenReady(future) { result =>
      result isExhausted () mustBe true
      result wasApplied () mustBe true
      result one ()
    }
  }

  "BusinessAlertTable" should "Store the alert data in business alert table" in {

    val future = database.BusinessAlertTable.storeBusinessAlert(businessAlert(businessAlertId, "Minor"))
    whenReady(future) { result =>
      result isExhausted () mustBe true
      result wasApplied () mustBe true
      result one ()
    }
  }

  it should "Get Business Alert by id" in {
    val chain = for {
      store <- this.database.BusinessAlertTable.storeBusinessAlert(businessAlert(businessAlertId, "Minor"))
      get   <- database.BusinessAlertTable.getBusinessAlertById(orgId, siteId, businessAlertId)

    } yield get
    whenReady(chain) { res =>
      res mustBe Some(businessAlert(businessAlertId, "Minor"))
    }
  }

  it should "Get all Business Alerts" in {
    val chain = for {
      store <- this.database.BusinessAlertTable.storeBusinessAlert(businessAlert(businessAlertId, "Minor"))
      store <- this.database.BusinessAlertTable.storeBusinessAlert(businessAlert(businessAlertId2, "Minor"))
      get   <- database.BusinessAlertTable.getAllBusinessAlerts(orgId, siteId)

    } yield get
    whenReady(chain) { res =>
      res.size mustBe 2
    }
  }

  it should "Dismiss Business Alert" in {
    val timeNow = Calendar.getInstance().getTimeInMillis
    val chain = for {
      store <- this.database.BusinessAlertTable.storeBusinessAlert(businessAlert(businessAlertId, "Clear"))
      store <- this.database.BusinessAlertTable.dismissByBusinessAlertId(orgId,
                                                                         siteId,
                                                                         businessAlertId,
                                                                         "Clear",
                                                                         "userId",
                                                                         100L)
      get <- database.BusinessAlertTable.getBusinessAlertById(orgId, siteId, businessAlertId)

    } yield get
    whenReady(chain) { res =>
      res.get.severity.get mustBe "Clear"
    }
  }

}
