package com.vz.nsp.parking.userdataservice.db

import java.util.UUID

import com.outworkers.phantom.dsl._
import com.vz.nsp.parking.db.EmbeddedDatabase
import com.vz.nsp.parking.model._
import com.vz.nsp.parking.userdataservice.constants.TestDataFeed._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class UserDataTableSpec extends DatabaseSpec with EmbeddedDatabase {

  object DatabaseService {
    def init() = {
      Await.ready(UserDataConnector.userdataTestTable.createTable(), 5.seconds)
    }

    def cleanup()= {
      Await.ready(UserDataConnector.userdataTestTable.truncateTable(), 5.seconds)
    }
  }

  override def beforeAll(): Unit = {
    DatabaseService.init()
    session.init()
  }

  override def afterAll(): Unit =
    DatabaseService.cleanup()

  "Store UserData" should "Store UserData" in {
    val future = database.UserDataTable.store(userData)
    whenReady(future) { result =>
      result isExhausted () mustBe true
      result wasApplied () mustBe true
      result one ()
    }
  }

 "UserData Id" should "get UserData by id" in {
   val userData1 = UserData(appId,userId,userDataId,"DataValue",timeInMillis, timeInMillis,false)
   val chain = for {
      store <- this.database.UserDataTable.store(userData1)
      get   <- database.UserDataTable.getUserDataById(appId,userId,userDataId)

    } yield get
    whenReady(chain) { res =>
      res mustBe Some(userData1)
    }
  }

 "UserData Id's" should "get all UserData Id's" in {

   DatabaseService.cleanup()
    val userDataId1 = UUID.randomUUID().toString
    val userDataId2 = UUID.randomUUID().toString
    val userData8 = UserData(appId, userId, userDataId1,"DataValue",timeInMillis, timeInMillis,false)
    val userData9 = UserData(appId, userId, userDataId2,"DataValue",timeInMillis, timeInMillis,false)

    val chain = for {
      store <- this.database.UserDataTable.store(userData8)
      store <- this.database.UserDataTable.store(userData9)
      get   <- database.UserDataTable.getAllUserData(appId, userId)

    } yield get
    whenReady(chain) { res =>
      res.size mustBe 2
    }
  }

  "update userData Value" should "update userData Value" in {

    val userData1 = UserData(appId, userId, userDataId,"DataValue", timeInMillis, timeInMillis,false)
    val userData2 = UserData(appId, userId, userDataId,"DataValue", timeInMillis, timeInMillis,false)
    val chain = for {
      store <- this.database.UserDataTable.store(userData1)
      store <- this.database.UserDataTable.updateByUserDataId(appId, userId, userDataId, userData2)
      get   <- database.UserDataTable.getUserDataById(appId, userId, userDataId)

    } yield get
    whenReady(chain) { res =>
      res mustBe Some(userData2)
    }
  }

  "delete UserData" should "delete UserData by id" in {

    val userData7 = UserData(appId, userId, userDataId,"DataValue", timeInMillis, timeInMillis,false)
    val chain = for {
      store  <- this.database.UserDataTable.store(userData7)
      delete <- this.database.UserDataTable.deleteByUserDataId(appId, userId, userDataId)

    } yield delete
    whenReady(chain) { res =>
      res wasApplied () mustBe true
    }
  }

}
