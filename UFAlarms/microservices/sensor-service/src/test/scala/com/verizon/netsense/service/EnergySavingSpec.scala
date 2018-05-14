package com.verizon.netsense.service

/**
 * Created by nalamte on 6/20/17.
 */
import com.outworkers.phantom.dsl._
import com.verizon.netsense.connector.CassandraConnector
import com.verizon.netsense.data.TestData
import com.verizon.netsense.model._
import com.vz.nsp.datasample.service.db.{DatabaseSpec, EmbeddedDatabase}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

/**
 * Created by nalamte on 6/12/17.
 */
class EnergySavingSpec
    extends DatabaseSpec
    with EmbeddedDatabase
    with CassandraConnector.testConnector.Connector
    with TestData {

  object DatabaseService {
    def init(): Future[List[ResultSet]] = {
      val create = Future.sequence(
        List(
          database.energySavingsNodeTableTest.create.ifNotExists().future(),
          database.energySavingSiteTableTest.create.ifNotExists().future()
        )
      )
      Await.ready(create, 5.seconds)
    }

    def cleanup(): Future[List[ResultSet]] = {
      val truncate = Future.sequence(
        List(
          database.energySavingsNodeTableTest.truncate.future(),
          database.energySavingSiteTableTest.truncate().future()
        )
      )
      Await.ready(truncate, 5.seconds)
    }
  }

  override def beforeAll(): Unit = {
    DatabaseService.init()
    session.init()
  }

  override def afterAll(): Unit = {
    DatabaseService.cleanup()
  }

  val actualenergyconsumption = 100.1
  val ledenergyconsumption    = 100.2
  val legacyenergyconsumption = 100.3
  val savingslegacyvsactual   = 100.4
  val savingslegacyvsled      = 100.5
  val period                  = "15min"

  "DbLayer" should "Store the Historical data for Energy saving Node" in {
    val energySavingsNodeModel: EnergySavingsNodeModel = EnergySavingsNodeModel(
      siteId,
      nodeId,
      aggregationtype,
      starttimeInMicros,
      actualenergyconsumption,
      enddt,
      endtime,
      ledenergyconsumption,
      legacyenergyconsumption,
      savingslegacyvsactual,
      savingslegacyvsled,
      startdt,
      startdt,
      starthr,
      ts
    )
    val future = database.energySavingsNodeTableTest.storeEnergySavingNode(energySavingsNodeModel)
    whenReady(future) { result =>
      result isExhausted () mustBe true
      result wasApplied () mustBe true
      result one ()
    }
  }

  it should "Get the Node events from Energy saving node table" in {
    val energySavingsNodeModel: EnergySavingsNodeModel = EnergySavingsNodeModel(
      siteId,
      nodeId,
      aggregationtype,
      starttimeInMicros,
      actualenergyconsumption,
      enddt,
      endtime,
      ledenergyconsumption,
      legacyenergyconsumption,
      savingslegacyvsactual,
      savingslegacyvsled,
      startdt,
      startdt,
      starthr,
      ts
    )
    val requestQuery: SensorQueryPayload = SensorQueryPayload(requestId,
                                                        `type`,
                                                        model,
                                                        action,
                                                        instanceId,
                                                        timestamp,
                                                        userId,
                                                        service,
                                                        nodeProps,
                                                        siteProps,
                                                        orgProps,
                                                        extProps)
    val sensorQueryEnvelope: SensorQueryEnvelope = SensorQueryEnvelope(messageId,
                                                        responsetopicInRequest,
                                                        requestQuery)

    val storageQuery = database.energySavingsNodeTableTest.storeEnergySavingNode(energySavingsNodeModel)
    val fetchQuery = database.energySavingsNodeTableTest.getEnergySavingsNode(sensorQueryEnvelope)

    val executeStorageQuery = Await.result(storageQuery, 20.second)
    val getRecordsQuery = Await.result(fetchQuery, 20.second)

    getRecordsQuery mustBe List(
      (siteId,
        nodeId,
        startdt,
        legacyenergyconsumption,
        ledenergyconsumption,
        actualenergyconsumption,
        savingslegacyvsled,
        savingslegacyvsactual))
  }
  it should "store the Historical data for energy saving Site" in {

    val energySavingsSiteModel: EnergySavingsSiteModel = EnergySavingsSiteModel(
      siteId,
      aggregationtype,
      starttimeInMicros,
      startdt,
      actualenergyconsumption,
      ledenergyconsumption,
      legacyenergyconsumption,
      savingslegacyvsactual,
      savingslegacyvsled
    )

    val storageQuery = database.energySavingSiteTableTest.storeEnergySavingSite(energySavingsSiteModel)
    val executeStorageQuery = Await.result(storageQuery, 20.second)
    executeStorageQuery.wasApplied()

  }

    it should "Get the Node events from Energy saving site table" in {
    val energySavingsSiteModel: EnergySavingsSiteModel = EnergySavingsSiteModel(
      siteId,
      aggregationtype,
      starttimeInMicros,
      startdt,
      actualenergyconsumption,
      ledenergyconsumption,
      legacyenergyconsumption,
      savingslegacyvsactual,
      savingslegacyvsled
    )
    val requestQuery: SensorQueryPayload = SensorQueryPayload(requestId,
                                                              `type`,
                                                              model,
                                                              action,
                                                              instanceId,
                                                              timestamp,
                                                              userId,
                                                              service,
                                                              nodeProps,
                                                              siteProps,
                                                              orgProps,
                                                              extProps)
    val sensorQueryEnvelope: SensorQueryEnvelope = SensorQueryEnvelope(messageId, responsetopicInRequest, requestQuery)

    val storageQuery = database.energySavingSiteTableTest.storeEnergySavingSite(energySavingsSiteModel)
    val fetchQuery = database.energySavingSiteTableTest.getEnergySavingsSite(sensorQueryEnvelope)

      val executeStorageQuery = Await.result(storageQuery, 20.second)
      val getRecordsQuery = Await.result(fetchQuery, 20.second)

      getRecordsQuery mustBe List(
        (siteId,
         startdt,
         legacyenergyconsumption,
         ledenergyconsumption,
         actualenergyconsumption,
         savingslegacyvsled,
         savingslegacyvsactual)
      )
  }
}
