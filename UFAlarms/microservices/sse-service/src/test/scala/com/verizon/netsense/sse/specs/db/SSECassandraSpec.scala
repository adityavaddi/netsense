package com.verizon.netsense.sse.specs.db

import akka.actor.ActorSystem
import akka.stream.scaladsl.{Keep, Sink, Source}
import akka.stream.{ActorMaterializer, ActorMaterializerSettings, Supervision}
import com.outworkers.phantom.connectors.CassandraConnection
import com.verizon.netsense.sse.db.database.SSEDatabase
import com.verizon.netsense.sse.model._
import com.verizon.netsense.sse.service.BaseSSEService
import com.verizon.netsense.sse.specs.data.SSEGenData
import com.verizon.netsense.sse.util.ObjectMapperUtil
import com.verizon.netsense.test.kafka.Utils.StageStoppingTimeout
import com.verizon.netsense.utils.Logging
import org.scalatest.{AsyncFlatSpec, BeforeAndAfterAll, MustMatchers}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.control.NonFatal

/*
 * Created by sundach on 7/9/17.
 */
class SSECassandraSpec
    extends AsyncFlatSpec
    with CassandraSpec
    with BeforeAndAfterAll
    with Logging
    with MustMatchers
    with SSEGenData
    with BaseSSEService {

  implicit val stageStoppingTimeout          = StageStoppingTimeout(15.seconds)
  implicit val system                        = ActorSystem.create("c")
  implicit val conn: CassandraConnection     = startDB()
  implicit val session                       = conn.session
  override implicit val ec                   = scala.concurrent.ExecutionContext.Implicits.global
  override def kafkaBootstrapServers: String = ???
  override def kafkaClientId: String         = ???
  override def kafkaGroupId: String          = ???
  override def kafkaOffsetReset: String      = ???

  val decider: Supervision.Decider = {
    case NonFatal(ex) =>
      log.error("Got non fatal exception in SSEActor flow", ex)
      Supervision.Resume
    case ex =>
      log.error("Got fatal exception in SSEActor flow, stream will be stopped", ex)
      Supervision.Stop
  }
  implicit val mate: ActorMaterializer = ActorMaterializer(
    ActorMaterializerSettings(system).withSupervisionStrategy(decider)
  )
  implicit val parallel = 2
  object EmbeddedDb extends SSEDatabase(conn)

  override def beforeAll() = {
    createSchemaAndTable(conn.session)
    insertIntoDevices(conn.session)
    insertIntoFilter(conn.session)
    insertIntoBusinessAlertFilter(conn.session)
    super.beforeAll()
  }

  override def afterAll() = {
    stopDB()
    super.afterAll()
    system.terminate()
  }

  def graphiteHost: String           = "test"
  def graphitePort: Int              = 1
  override def database: SSEDatabase = EmbeddedDb
  val i: Int                         = 1

  /*
   * LoginReq enrich flow with orgid and siteid
   */
  "LoginReq Test Cassandra flow" should "Enrich Flow should contain org and site for LoginReq" in {
    val LoginReqObj = ObjectMapperUtil.fromJson[LoginReq](loginReq(i))
    LoginReqObj.nodeid = "JSV7_0"
    LoginReqObj.name = "LoginReq"
    LoginReqObj.orgid = "O0"
    LoginReqObj.siteid = "S0"
    val sink = enrich().toMat(Sink.head)(Keep.right)
    val f    = Source(List(LoginReqObj)).runWith(sink)
    val x    = Await.result(f, 100.seconds)
    assert(x.orgid == "O0" && x.siteid == "S0")
  }

  /*
   * DeviceAlarm enrich flow with orgid and siteid
   */
  "DeviceAlarm Test Cassandra flow" should "Enrich Flow should contain org and site for DeviceAlarm" in {
    val DeviceAlarmObj = ObjectMapperUtil.fromJson[DeviceAlarm](deviceAlarm(i))
    DeviceAlarmObj.nodeid = "JSV7_1"
    val sink   = enrich().toMat(Sink.head)(Keep.right)
    val f      = Source(List(DeviceAlarmObj)).runWith(sink)
    val result = Await.result(f, 100.seconds)
    assert(result.orgid == "O1" && result.siteid == "S1")
  }

  /*
   * SensorSample enrich flow with orgid and siteid
   */
  "SensorSample Test Cassandra flow" should "Enrich Flow should contain org and site for SensorSample" in {
    val SensorSampleObj = ObjectMapperUtil.fromJson[SensorSample](sensorSample(i))
    SensorSampleObj.name = "SensorSample"
    SensorSampleObj.nodeid = "JSV7_0"
    SensorSampleObj.orgid = "O0"
    SensorSampleObj.siteid = "S0"
    val sink   = enrich().toMat(Sink.head)(Keep.right)
    val f      = Source(List(SensorSampleObj)).runWith(sink)
    val result = Await.result(f, 100.seconds)
    assert(result.orgid == "O0" && result.siteid == "S0")
  }

  /*
   * ConnectionStatus enrich flow with orgid and siteid
   */
  "ConnectionSatus Test Cassandra flow" should "Enrich Flow should contain org and site for ConnectionSatus" in {
    val ConnectionStatusObj = ObjectMapperUtil.fromJson[ConnectionStatus](connectionStatus(i))
    ConnectionStatusObj.nodeid = "JSV7_0"
    ConnectionStatusObj.name = "ConnectionStatus"
    ConnectionStatusObj.orgid = "O0"
    ConnectionStatusObj.siteid = "S0"
    val sink = enrich().toMat(Sink.head)(Keep.right)
    val f    = Source(List(ConnectionStatusObj)).runWith(sink)
    val x    = Await.result(f, 100.seconds)
    assert(x.orgid == "O0" && x.siteid == "S0")
  }

  /*
   * GpsSample enrich flow with orgid and siteid
   */
  "GpsSample Test Cassandra flow" should "Enrich Flow should contain org and site for GpsSample" in {
    val GpsSampleObj = ObjectMapperUtil.fromJson[GpsSample](gpsSample(i))
    GpsSampleObj.nodeid = "JSV7_0"
    GpsSampleObj.name = ""
    GpsSampleObj.orgid = "O0"
    GpsSampleObj.siteid = "S0"
    val sink = enrich().toMat(Sink.head)(Keep.right)
    val f    = Source(List(GpsSampleObj)).runWith(sink)
    val x    = Await.result(f, 100.seconds)
    assert(x.orgid == "O0" && x.siteid == "S0")
  }

  /*
   * LoginReq enrich flow with null
   */
  "LoginReq Test Cassandra flow" should "Enrich Flow should not contain org and site for LoginReq" in {
    val LoginReqObj = ObjectMapperUtil.fromJson[LoginReq](loginReq(i))
    LoginReqObj.nodeid = "JSV7_12"

    val sink = enrich().toMat(Sink.head)(Keep.right)
    val f    = Source(List(LoginReqObj)).runWith(sink)

    f.map(s => assert(s.orgid == null && s.siteid == null))
  }

  /*
   * DeviceAlarm enrich flow with null
   */
  "DeviceAlarm Test Cassandra flow" should "Enrich Flow should not contain org and site for DeviceAlarm " in {
    val DeviceAlarmObj = ObjectMapperUtil.fromJson[DeviceAlarm](deviceAlarm(i))
    DeviceAlarmObj.nodeid = "JSV7_12"
    val sink = enrich().toMat(Sink.head)(Keep.right)
    val f    = Source(List(DeviceAlarmObj)).runWith(sink)
    f.map(s => assert(s.orgid == "O1" && s.siteid == "S1"))
  }

  /*
   *SensorSample enrich flow with null
   */
  "SensorSample Test Cassandra flow" should "Enrich Flow should not contain org and site for SensorSample" in {
    val SensorSampleObj = ObjectMapperUtil.fromJson[SensorSample](sensorSample(i))
    SensorSampleObj.nodeid = "JSV7_12"

    val sink = enrich().toMat(Sink.head)(Keep.right)
    val f    = Source(List(SensorSampleObj)).runWith(sink)
    f.map(s => assert(s.orgid == null && s.siteid == null))

  }

  /*
   * ConnectionSatus enrich flow with null
   */
  "ConnectionSatus Test Cassandra flow" should "Enrich Flow should not contain org and site for ConnectionSatus" in {
    val ConnectionSatusObj = ObjectMapperUtil.fromJson[ConnectionStatus](connectionStatus(i))
    ConnectionSatusObj.nodeid = "JSV7_12"

    val sink = enrich().toMat(Sink.head)(Keep.right)
    val f    = Source(List(ConnectionSatusObj)).runWith(sink)
    f.map(s => assert(s.orgid == null && s.siteid == null))
  }

  /*
   * GpsSample enrich flow with null
   */
  "GpsSample Test Cassandra flow" should "Enrich Flow should not contain org and site for GpsSample" in {
    val GpsSampleObj = ObjectMapperUtil.fromJson[GpsSample](gpsSample(i))
    GpsSampleObj.nodeid = "JSV7_12"

    val sink = enrich().toMat(Sink.head)(Keep.right)
    val f    = Source(List(GpsSampleObj)).runWith(sink)
    f.map(s => assert(s.orgid == "O1" && s.siteid == "S1"))
  }

  /*
   *LoginReq filter flow with orgid and siteid
   */
  "LoginReq Test Cassandra flow" should "Filter Flow should contain org and site for LoginReq" in {
    val LoginReqObj = ObjectMapperUtil.fromJson[LoginReq](loginReq(i))
    LoginReqObj.orgid = "O0"
    LoginReqObj.siteid = "S0"
    LoginReqObj.nodeid = "JSV7_0"
    LoginReqObj.name = "LoginReq"

    Source(List { LoginReqObj })
      .runWith(filter().toMat(Sink.head)(Keep.right))
      .map(
        s =>
          assert(
            s.headOption
              .getOrElse(throw new Exception("Nothing found"))
              .topicName == "/streamv1/O0/S0/JSV7_0/LoginReq"
        )
      )

  }

  /*
   *DeviceAlarm filter flow with orgid and siteid
   */
  "DeviceAlarm Test Cassandra flow" should "Filter Flow should contain org and site for DeviceAlarm" in {
    val DeviceAlarmObj = ObjectMapperUtil.fromJson[DeviceAlarm](deviceAlarm(i))
    DeviceAlarmObj.orgid = "O0"
    DeviceAlarmObj.siteid = "S0"
    DeviceAlarmObj.nodeid = "JSV7_0"

    Source(List { DeviceAlarmObj })
      .runWith(filter().toMat(Sink.head)(Keep.right))
      .map(
        s =>
          assert(
            s.headOption
              .getOrElse(throw new Exception("Nothing found"))
              .topicName == "/streamv1/O0/S0/JSV7_0/DeviceAlarm"
        )
      )

  }

  /*
   *SensorSample filter flow with orgid and siteid
   */
  "SensorSample Test Cassandra flow" should "Filter Flow should contain org and site for SensorSample" in {
    val SensorSampleObj = ObjectMapperUtil.fromJson[SensorSample](sensorSample(i))
    SensorSampleObj.orgid = "O0"
    SensorSampleObj.siteid = "S0"
    SensorSampleObj.nodeid = "JSV7_0"
    SensorSampleObj.name = "SensorSample"

    Source(List { SensorSampleObj })
      .runWith(filter().toMat(Sink.head)(Keep.right))
      .map(
        s =>
          assert(
            s.headOption
              .getOrElse(throw new Exception("Nothing found"))
              .topicName == "/streamv1/O0/S0/JSV7_0/SensorSample"
        )
      )
  }

  /*
   *ConnectionStatus filter flow with orgid and siteid
   */
  "ConnectionStatus Test Cassandra flow" should "Filter Flow should contain org and site for ConnectionStatus" in {
    val ConnectionStatusObj = ObjectMapperUtil.fromJson[ConnectionStatus](connectionStatus(i))
    ConnectionStatusObj.orgid = "O0"
    ConnectionStatusObj.siteid = "S0"
    ConnectionStatusObj.nodeid = "JSV7_0"
    ConnectionStatusObj.name = "ConnectionStatus"

    Source(List { ConnectionStatusObj })
      .runWith(filter().toMat(Sink.head)(Keep.right))
      .map(
        s =>
          assert(
            s.headOption
              .getOrElse(throw new Exception("Nothing found"))
              .topicName == "/streamv1/O0/S0/JSV7_0/ConnectionStatus"
        )
      )
  }

  /*
   * GpsSample filter flow with orgid and siteid
   */
  "GpsSample Test Cassandra flow" should "Filter Flow should contain org and site for GpsSample" in {
    val GpsSampleObj = ObjectMapperUtil.fromJson[GpsSample](gpsSample(i))
    GpsSampleObj.orgid = "O0"
    GpsSampleObj.siteid = "S0"
    GpsSampleObj.nodeid = "JSV7_0"
    GpsSampleObj.name = "GpsSample"

    Source(List { GpsSampleObj })
      .runWith(filter().toMat(Sink.head)(Keep.right))
      .map(
        s =>
          assert(
            s.headOption
              .getOrElse(throw new Exception("Nothing found"))
              .topicName == "/streamv1/O0/S0/JSV7_0/GpsSample"
        )
      )
  }
  /*
   *BusinessAlert filter flow with orgid and siteid
   * /streamv1/orgid/siteid/businessalert/parking/trigger1
   */
  "BusinessAlert Test Cassandra flow" should "Filter Flow should contain org and site for BusinessAlert" in {
    val BusinessAlertObj = ObjectMapperUtil.fromJson[BusinessAlert](businessAlert(i))
    BusinessAlertObj.orgid = "O0"
    BusinessAlertObj.siteid = "S0"
    BusinessAlertObj.triggerCategory = "parking"
    BusinessAlertObj.triggerId = "trigger0"

    Source(List { BusinessAlertObj })
      .runWith(businessAlertfilter().toMat(Sink.head)(Keep.right))
      .map(
        s =>
          assert(
            s.headOption
              .getOrElse(throw new Exception("Nothing found"))
              .topicName == "/streamv1/O0/S0/businessalert/parking/trigger0"
        )
      )
  }

  /*
   * LoginReq filter flow with null
   */
  "LoginReq Test Cassandra flow" should "Filter Flow should not contain org and site for LoginReq" in {
    val LoginReqObj = ObjectMapperUtil.fromJson[LoginReq](loginReq(i))
    LoginReqObj.nodeid = "JSV7_12"
    LoginReqObj.orgid = "O12"
    LoginReqObj.siteid = "S12"

    Source(List { LoginReqObj })
      .runWith(filter().toMat(Sink.head)(Keep.right))
      .map(s => assert(s.isEmpty))

  }

  /*
   * DeviceAlarm filter flow with +
   */
  "DeviceAlarm Test Cassandra flow" should "Filter Flow should not contain org and site for DeviceAlarm" in {
    val DeviceAlarmObj = ObjectMapperUtil.fromJson[DeviceAlarm](deviceAlarm(i))
    DeviceAlarmObj.nodeid = "+"
    DeviceAlarmObj.orgid = "+"
    DeviceAlarmObj.siteid = "+"

    Source(List { DeviceAlarmObj })
      .runWith(filter().toMat(Sink.head)(Keep.right))
      .map(s => assert(s.size == 1))

  }

  /*
   * SensorSample filter flow with null
   */
  "SensorSample Test Cassandra flow" should "Filter Flow should not contain org and site for SensorSample" in {
    val SensorSampleObj = ObjectMapperUtil.fromJson[SensorSample](sensorSample(i))
    SensorSampleObj.nodeid = "JSV7_12"
    SensorSampleObj.orgid = "O12"
    SensorSampleObj.siteid = "S12"

    Source(List { SensorSampleObj })
      .runWith(filter().toMat(Sink.head)(Keep.right))
      .map(s => assert(s.isEmpty))
  }

  /*
   * ConnectionStatus filter flow with null
   */
  "ConnectionStatus Test Cassandra flow" should "Filter Flow should not contain org and site for ConnectionStatus" in {
    val ConnectionStatusObj = ObjectMapperUtil.fromJson[ConnectionStatus](connectionStatus(i))
    ConnectionStatusObj.nodeid = "JSV7_12"
    ConnectionStatusObj.orgid = "O12"
    ConnectionStatusObj.siteid = "S12"

    Source(List { ConnectionStatusObj })
      .runWith(filter().toMat(Sink.head)(Keep.right))
      .map(s => assert(s.isEmpty))
  }

  /*
   * GpsSample filter flow with +
   */
  "GpsSample Test Cassandra flow" should "Filter Flow should not contain org and site for GpsSample" in {
    val GpsSampleObj = ObjectMapperUtil.fromJson[GpsSample](gpsSample(i))
    GpsSampleObj.nodeid = "+"
    GpsSampleObj.orgid = "+"
    GpsSampleObj.siteid = "+"

    Source(List { GpsSampleObj })
      .runWith(filter().toMat(Sink.head)(Keep.right))
      .map(s => assert(s.size == 1))
  }

  /*
   *BusinessAlert filter flow with orgid and siteid
   * /streamv1/orgid/siteid/businessalert/parking/trigger1
   */
  "BusinessAlert Test Cassandra flow" should "Filter Flow should not contain org and site for BusinessAlert" in {
    val BusinessAlertObj = ObjectMapperUtil.fromJson[BusinessAlert](businessAlert(i))
    BusinessAlertObj.orgid = "O12"
    BusinessAlertObj.siteid = "S12"
    BusinessAlertObj.triggerCategory = "parking"
    BusinessAlertObj.triggerId = "trigger12"

    Source(List { BusinessAlertObj })
      .runWith(businessAlertfilter().toMat(Sink.head)(Keep.right))
      .map(s => assert(s.size == 1))
  }

  /*
   * Filter data must match with input
   */
  "Test Cassandra flow" should "Filter Flow should contain 4 rows" in {
    val LoginReqObj = ObjectMapperUtil.fromJson[LoginReq](loginReq(i))
    LoginReqObj.nodeid = "JSV7_1"
    LoginReqObj.orgid = "O1"
    LoginReqObj.siteid = "S1"

    Source(List { LoginReqObj })
      .runWith(filter().toMat(Sink.head)(Keep.right))
      .map(s => assert(s.size == 2))
  }

  /*
   * BusinessAlert Filter data must match with input
   */
  "BusinessAlert Test Cassandra flow" should "Filter Flow should contain 1 row with wildcard" in {
    val BusinessAlertObj = ObjectMapperUtil.fromJson[BusinessAlert](businessAlert(i))
    BusinessAlertObj.orgid = "O1"
    BusinessAlertObj.siteid = "S1"
    BusinessAlertObj.triggerCategory = "parking"
    BusinessAlertObj.triggerId = "+"

    Source(List { BusinessAlertObj })
      .runWith(businessAlertfilter().toMat(Sink.head)(Keep.right))
      .map(s => assert(s.size == 1))
  }
}
