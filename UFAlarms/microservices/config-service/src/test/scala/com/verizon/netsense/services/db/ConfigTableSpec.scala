package com.verizon.netsense.services.db

import com.outworkers.phantom.dsl._
import com.verizon.netsense.connector.CassandraConnector
import com.verizon.netsense.database.ConfigEmbeddedDatabase
import com.verizon.netsense.entity.Config

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class ConfigTableSpec
    extends DatabaseSpec
    with ConfigEmbeddedDatabase
    with CassandraConnector.testConnector.Connector {

  object DatabaseService {
    def init(): Future[List[ResultSet]] = {
      val create = Future.sequence(
        List(
          database.configModel.create.ifNotExists().future()
        )
      )
      Await.ready(create, 5.seconds)
    }

    def cleanup(): Future[List[ResultSet]] = {
      val truncate = Future.sequence(
        List(
          database.configModel.truncate.future()
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

  val testConfigUUID = java.util.UUID.randomUUID().toString

  behavior of "Storing the  Config in database"
  ignore should "Store config to cassandra" in {
    //it should "Store device events to cassandra" in {
    val configElement: Config = Config("Config", testConfigUUID, "uberorg", "ubersite", "name", "12345", "unode", "{}")

    val chain = for {
      save <- database.configModel.storeConfig(configElement)
      res  <- database.configModel.getConfigByCfgid(testConfigUUID, "ubersite")
    } yield res

    whenReady(chain) { res =>
      res mustBe defined
      println("i am here")
      println(res)
    }
  }

  behavior of "Retrieving of config from database"
  ignore should "retrieve config by id" in {
    val chain = for {
      res <- database.configModel.getConfigByCfgid(testConfigUUID, "ubersite")
    } yield res

    whenReady(chain) { res =>
      res mustBe defined
      res.get.cfgid mustBe testConfigUUID
    }

  }

  behavior of "Update config in database"
  ignore should "Update config from cassandra" in {
    val chain = for {
      update <- database.configModel.updateConfigByCfgId(testConfigUUID, "{}", "45678")
      res    <- database.configModel.getConfigByCfgid(testConfigUUID, "ubersite")
    } yield res

    whenReady(chain) { res =>
      res mustBe defined
      res.get.token mustBe "45678"
      println(res)
    }
  }
  behavior of "Deleting config from database"
  ignore should "Delete config from cassandra" in {
    val future = database.configModel.deleteConfigByCfgId(testConfigUUID)
    whenReady(future) { result =>
      result isExhausted () mustBe true
      result wasApplied () mustBe true
      result one ()
      println("i AM here")
      println(result)
    }
  }

}
