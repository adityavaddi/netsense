package com.vz.nsp.trigger.service

/**
  * Created by maleva on 2/20/18.
  */


import com.outworkers.phantom.dsl._
import com.verizon.netsense.connector.CassandraConnector
import com.vz.nsp.trigger.data.TestData
import com.vz.nsp.trigger.db.{DatabaseSpec, EmbeddedDatabase}
import com.vz.nsp.trigger.util.BaseSpec
import org.apache.cassandra.service.{CassandraDaemon, EmbeddedCassandraService}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext


 class TriggerSpec
   extends EmbeddedDatabase
   with BaseSpec
     with TestData {

   implicit val ec = ExecutionContext.Implicits.global


   lazy val embeddedCassandraServiceObj = new EmbeddedCassandraService
   lazy val cassandraDaemon = new CassandraDaemon


   override protected def beforeAll(): Unit = {
     super.beforeAll()
     val start: Unit = embeddedCassandraServiceObj.start()

   }

   override protected def afterAll() = {
     cassandraDaemon.stop()
     cassandraDaemon.stopNativeTransport()
     super.afterAll()
   }


   "Db" should "Store trigger events to cassandra" in {
     val future = database.triggerServiceTableTest.store(triggerSampleEvent)
     whenReady(future) { result =>
       result isExhausted() mustBe true
       result wasApplied() mustBe true
       result one()
     }
   }


   it should "get trigger data for triggerId" in {
     val chain = for {
       store <- database.triggerServiceTableTest.store(triggerSampleEvent)
       get <- database.triggerServiceTableTest.getTriggerById(triggerId, siteId, orgId)

     } yield get
     whenReady(chain) { res =>
       res.getOrElse(triggerSampleEvent)
     }
   }

   it should "get all trigger records " in {
     val chain = for {
       store <- database.triggerServiceTableTest.store(triggerSampleEvent)
       store <- database.triggerServiceTableTest.store(triggerSampleEvent1)
       getAll <- database.triggerServiceTableTest.getAllTriggerId(siteId, orgId)
     } yield getAll
     whenReady(chain) { result =>
       result.mustBe(List(triggerSampleEvent, triggerSampleEvent1))
     }
   }

   it should "Get all Business Alerts" in {
     val chain = for {
       store <- this.database.triggerServiceTableTest.store(triggerSampleEvent)
       store <- this.database.triggerServiceTableTest.store(triggerSampleEvent1)
       get <- database.triggerServiceTableTest.getAllTriggerId(orgId, siteId)

     } yield get
     whenReady(chain) { res =>
       res.size mustBe 2
     }
   }

 }
