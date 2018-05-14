package com.verizon.netsense.whatifservice.db

import com.outworkers.phantom.ops.QueryContext

import scala.concurrent.duration._
import com.verizon.netsense.whatifservice.data.TestDataFeed._
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.testkit.TestKit
import com.outworkers.phantom.dsl._

import scala.concurrent.{Await, ExecutionContext, Future}
import com.verizon.netsense.whatifservice.util.BaseSpec
import com.verizon.netsense.whatifservice.db
import org.apache.cassandra.service.{CassandraDaemon, EmbeddedCassandraService}

/**
 * Created by maleva on 4/21/18.
 */
class WhatIfJobTableSpec extends TestKit(ActorSystem()) with BaseSpec with EmbeddedDatabase {

  implicit val ec = ExecutionContext.Implicits.global

  implicit val mat = ActorMaterializer()(system)

  lazy val embeddedCassandraServiceObj = new EmbeddedCassandraService
  lazy val cassandraDaemon             = new CassandraDaemon

  object DatabaseService {
    def init() = {
      val create = Future.sequence(
        List(
          database.WhatIfJobTable.create.ifNotExists().future()
        )
      )
      Await.ready(create, 5.seconds)
    }

    def cleanup(): Future[List[ResultSet]] = {
      val truncate = Future.sequence(
        List(
          database.WhatIfJobTable.truncate.future()
        )
      )
      Await.ready(truncate, 5.seconds)
    }
  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    DatabaseService.init()
  }

  override protected def afterAll() = {
    DatabaseService.cleanup()
    cassandraDaemon.stop()
    cassandraDaemon.stopNativeTransport()
    super.afterAll()
  }

  "WhatIfJobTable" should "Store the What if data in History table" in {

    val future = database.WhatIfJobTable.storeJobStatus(whatIf)
    whenReady(future) { result =>
      result isExhausted () mustBe true
      result wasApplied () mustBe true
      result one ()
    }
  }

  it should "Get What If Job by id" in {
    val chain = for {
      store <- this.database.WhatIfJobTable.storeJobStatus(whatIf)
      get   <- database.WhatIfJobTable.getJobId(whatIf.orgid, whatIf.siteid, whatIf.jobid)
    } yield get
    whenReady(chain) { res =>
      res mustBe Some(whatIf)
    }
  }

  it should "get All What If job" in {
    val chain = for {
      store <- this.database.WhatIfJobTable.storeJobStatus(whatIf)
      store <- this.database.WhatIfJobTable.storeJobStatus(whatIf2)
      get   <- database.WhatIfJobTable.getAllJobs(whatIf.orgid, whatIf.siteid)
    } yield get
    whenReady(chain) { res =>
      res.size mustBe 2
    }
  }

  it should "delete What If job by id" in {
    val chain = for {
      delete <- this.database.WhatIfJobTable.deleteByJobId(whatIf.orgid, whatIf.siteid, whatIf.jobid)
    } yield delete
    whenReady(chain) { res =>
      res.wasApplied()
    }
  }

}
