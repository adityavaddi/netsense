package com.vz.nsp.parking.grouppolicyservice.service

import com.outworkers.phantom.dsl.ResultSet
import com.vz.nsp.parking.db.{PhantomService, ProductionDatabase}
import com.vz.nsp.parking.dblayer.DbLayer
import com.vz.nsp.parking.grouppolicyservice.BaseSpec
import com.vz.nsp.parking.grouppolicyservice.constants.TestDataFeed.{parkingSpot, _}
import com.vz.nsp.parking.grouppolicyservice.db.parkingServicesDbTest
import com.vz.nsp.parking.model._
import org.scalatest.time.{Seconds, Span}

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

/**
 * Created by nagarna on 9/26/17.
 */
class ParkingSpaceServiceSpec extends BaseSpec {

  val dbLayer           = spy(new DbLayer(new PhantomService with ProductionDatabase {}))
  val parkingSpotHelper = spy(new ParkingSpaceService(dbLayer))

  private implicit val ec = scala.concurrent.ExecutionContext.Implicits.global

  override def beforeAll() = {
    super.beforeAll()
    Database.init()
  }

  override def afterAll() = {
    Database.cleanup()
    super.afterAll()

  }

  object Database {
    def init() {
      Await.ready(parkingServicesDbTest.ParkingSpaceTable.customTypesSpace, 5.seconds)
      Await.ready(parkingServicesDbTest.ParkingSpaceTable.createSpaceTable(), 5.seconds)
    }

    def cleanup(): Future[ResultSet] =
      Await.ready(parkingServicesDbTest.ParkingSpaceTable.truncateSpaceTable, 5.seconds)

  }

  /* Method: updateParkingSpotProcess */
  "updateParkingSpaceProcess" should "Return Failure" in {

    //doReturn(Future(List(parkingSpot))).when(dbLayer).getParkingSpot(orgid, siteid, parkingSpotId)
    doReturn(futureSuccessspot)
      .when(dbLayer)
      .updateSpaceAttributes(parkingSpot, Future(List(parkingSpot)))

    val chain = for {
      appResponse <- parkingSpotHelper.updateParkingSpaceProcess(appRequestParkingSpot)
    } yield appResponse

    whenReady(chain, timeout(Span(5, Seconds))) { res =>
      assert(
        res
          .asInstanceOf[AppSuccessResponse[AppRequest]]
          .messageid
          .equals(messageId)
      )
    }
  }

  /* Method: getAllParkingSpotsProcess */
  "getAllParkingSpacesProcess by parkingspaceids" should "Return Success" in {
    doReturn(Future(List(parkingspot)))
      .when(dbLayer)
      .getAllParkingSpaces(List(parkingSpotId))

    val chain = for {
      appResponse <- parkingSpotHelper.getAllParkingSpacesProcess(appRequestParkingSpot)
    } yield appResponse

    whenReady(chain, timeout(Span(5, Seconds))) { res =>
      assert(
        res
          .asInstanceOf[AppSuccessResponse[List[ParkingSpaceResponse]]]
          .messageid
          .equals(messageId)
      )
      assert(
        res
          .asInstanceOf[AppSuccessResponse[List[ParkingSpaceResponse]]]
          .response
          .success
          .equals(true)
      )
    }
  }

  /* Method: deleteParkingSpotProcess */
  "deleteParkingSpaceProcess by parkingspaceid" should "Return Success" in {
    val mockResultSet = mock[ResultSet]
    doReturn(Future(List(parkingSpot.parkingspaceid)))
      .when(dbLayer)
      .getExistingParkingSpaceIds(List(parkingSpotId))
    doReturn(Future(true))
      .when(dbLayer)
      .deleteByParkingSpaceId(List(parkingSpotId))

    //doReturn(true).when(mockResultSet).wasApplied()

    val chain = for {
      appResponse <- parkingSpotHelper.deleteParkingSpaceProcess(appRequestParkingSpot)
    } yield appResponse

    whenReady(chain, timeout(Span(5, Seconds))) { res =>
      assert(
        res
          .asInstanceOf[AppSuccessResponse[AppRequest]]
          .messageid
          .equals(messageId)
      )
    }
  }

}
