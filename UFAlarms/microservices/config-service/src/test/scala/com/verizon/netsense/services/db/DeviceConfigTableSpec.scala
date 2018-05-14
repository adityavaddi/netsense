package com.verizon.netsense.services.db

import com.outworkers.phantom.dsl.ResultSet
import com.verizon.netsense.connector.CassandraConnector
import com.verizon.netsense.database.DeviceConfigEmbeddedDatabase

import scala.concurrent.{Await, ExecutionContextExecutor, Future}
import scala.concurrent.duration._

/**
 * Created by davor on 6/9/17.
 */
class DeviceConfigTableSpec
    extends DatabaseSpec
    with DeviceConfigEmbeddedDatabase
    with CassandraConnector.testConnector.Connector {
  object DatabaseService {

    val ec = scala.concurrent.ExecutionContext.Implicits.global

    def init(): Future[List[ResultSet]] = {

      implicit val executor = ec

      val create = Future.sequence(
        List(
          database.deviceConfigModel.create.ifNotExists().future()(session, ec.asInstanceOf[ExecutionContextExecutor])
        )
      )
      Await.ready(create, 5.seconds)
    }

    def cleanup(): Future[List[ResultSet]] = {

      implicit val executor = ec

      val truncate = Future.sequence(
        List(
          database.deviceConfigModel.truncate.future()(session, ec.asInstanceOf[ExecutionContextExecutor])
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
}
