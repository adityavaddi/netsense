package com.vz.nsp.parking.grouppolicyservice.db

import com.outworkers.phantom.dsl.ResultSet
import com.vz.nsp.parking.db.EmbeddedDatabase
import com.vz.nsp.parking.grouppolicyservice.BaseSpec
import com.vz.nsp.parking.grouppolicyservice.constants.TestDataFeed._

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}

/**
  * Created by thangth on 8/17/17.
  *
  **/
class ParkingSpaceSpec extends BaseSpec with EmbeddedDatabase {
  private implicit val ec = ExecutionContext.Implicits.global

  override def beforeAll() = {
    Database.init()

    session.init()
  }

  override def afterAll() = {
    Database.cleanup()

  }

  object Database {
    def init() {
      Await.ready(parkingServicesDbTest.ParkingSpaceTable.customTypesSpace,
                  5.seconds)
      Await.ready(parkingServicesDbTest.ParkingSpaceTable.createSpaceTable,
                  5.seconds)
    }

    def cleanup(): Future[ResultSet] = {
      Await.ready(parkingServicesDbTest.ParkingSpaceTable.truncateSpaceTable,
                  5.seconds)
    }

  }

  "Concrete ParkingSpace" should "Store ParkingSpace" in {

    val storedParkingSpot =
      database.ParkingSpaceTable.storeParkingSpace(parkingspot, 125671L)
    whenReady(storedParkingSpot) { result =>
      result isExhausted () mustBe true
      result wasApplied () mustBe true
      result one ()
    }
  }

  it should "getting ParkingSpace by id" in {
    val chain = for {
      store <- this.database.ParkingSpaceTable
        .storeParkingSpace(parkingspot, 125671L)
      get <- database.ParkingSpaceTable.getParkingSpaceById(parkingSpotId)

    } yield get
    whenReady(chain) { res =>
      res mustBe Some(125671L)
    }
  }

  it should "get all ParkingSpaces" in {
    val chain = for {
      store <- this.database.ParkingSpaceTable
        .storeParkingSpace(parkingspot, 125671L)
      store <- this.database.ParkingSpaceTable
        .storeParkingSpace(parkingSpotTwo, 125671L)
      get <- database.ParkingSpaceTable.getParkingSpacesByIds(
        List(parkingSpotId, parkingSpotIdTwo))

    } yield get
    whenReady(chain) { res =>
      res.size mustBe 2
    }
  }

  it should "delete ParkingSpace by id" in {
    val chain = for {
      store <- this.database.ParkingSpaceTable
        .storeParkingSpace(parkingspot, 125671L)
      delete <- this.database.ParkingSpaceTable
        .deleteByParkingSpaceId(List(parkingSpotId))

    } yield delete
    whenReady(chain) { res =>
      res wasApplied () mustBe true
    }
  }

}
