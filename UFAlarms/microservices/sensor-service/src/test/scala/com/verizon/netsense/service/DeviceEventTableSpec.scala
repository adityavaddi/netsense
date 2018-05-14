package com.verizon.netsense.service

import com.outworkers.phantom.dsl._
import com.verizon.netsense.config.TestSuiteConfig
import com.verizon.netsense.connector.CassandraConnector
import com.verizon.netsense.data.TestData
import com.vz.nsp.datasample.service.db.{DatabaseSpec, EmbeddedDatabase}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

/**
 * Created by nalamte on 5/24/17.
 */
class DeviceEventTableSpec
    extends DatabaseSpec
    with EmbeddedDatabase
    with CassandraConnector.testConnector.Connector
    with TestSuiteConfig
    with TestData {

  override def beforeAll(): Unit = {
    DatabaseService.init()
    session.init()
  }

  override def afterAll(): Unit = {
    DatabaseService.cleanup()
  }

  object DatabaseService {
    def init(): Future[List[ResultSet]] = {
      val create = Future.sequence(
        List(
          database.deviceSensorSampleTableTest.create.ifNotExists().future()
        )
      )
      Await.ready(create, 5.seconds)
    }

    def cleanup(): Future[List[ResultSet]] = {
      val truncate = Future.sequence(
        List(
          database.deviceSensorSampleTableTest.truncate.future()
        )
      )
      Await.ready(truncate, 5.seconds)
    }
  }

  "Db" should "Store device events to cassandra" in {
    val future = database.deviceSensorSampleTableTest.store(sensorSampleEvent)
    whenReady(future) { result =>
      result isExhausted () mustBe true
      result wasApplied () mustBe true
      result one ()
    }
  }
  it should "getting Historical data for Sensor" in {
    val chain = for {
      store <- database.deviceSensorSampleTableTest.store(sensorSampleEvent)
      get   <- database.deviceSensorSampleTableTest.getHistoricalDataForSensor(requestQueryWithHeaders)

    } yield get
    whenReady(chain) { res =>
      res mustBe List(sensorSamplePayloadTuple)
      DatabaseService.cleanup()
    }
  }


  it should "getting Historical data for Sensor by injecting 3 records" in {
    val chain = for {
      store <- database.deviceSensorSampleTableTest.store(constructSensorSampleEvent(starttimeInMicros - 1l))
      store <- database.deviceSensorSampleTableTest.store(constructSensorSampleEvent(starttimeInMicros - 3l))
      store <- database.deviceSensorSampleTableTest.store(constructSensorSampleEvent(starttimeInMicros - 6l))
      get   <- database.deviceSensorSampleTableTest.getHistoricalDataForSensor(requestQueryWithHeaders)

    } yield get
    whenReady(chain) { res =>
      res mustBe List((starttimeInMicros - 1l,sensorValue),
                      (starttimeInMicros - 3l,sensorValue),
                      (starttimeInMicros - 6l,sensorValue))
    }
  }
}
