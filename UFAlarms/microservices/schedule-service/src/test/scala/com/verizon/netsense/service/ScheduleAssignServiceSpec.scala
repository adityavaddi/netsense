package com.verizon.netsense.service

import java.util
import java.util.{Calendar, UUID}

import akka.actor.ActorSystem
import akka.http.scaladsl.model.DateTime
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Keep, Source}
import akka.stream.testkit.TestSubscriber.Probe
import akka.stream.testkit.scaladsl.TestSink
import akka.testkit.TestKit
import com.verizon.netsense.connector.Neo4jConnector
import com.verizon.netsense.constants.Constants
import com.verizon.netsense.cyphers.Neo4jCyphers
import com.verizon.netsense.database.{EmbeddedDatabase, MicroServicesDb, PhantomService, ProductionDb}
import com.verizon.netsense.exceptions.CustomExceptions.{NoNodesFoundToSendScheduleException, NoScheduleFoundException}
import com.verizon.netsense.helper.{BaseSpec, Neo4jHelper, Neo4jService}
import com.verizon.netsense.model._
import com.verizon.netsense.utils.Logging

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._


/**
  * Created by maidapr on 2/8/18.
  */
class ScheduleAssignServiceSpec extends TestKit(ActorSystem("ScheduleAssign-Test-System"))
  with BaseSpec
  with EmbeddedDatabase
  with Logging with Neo4jCyphers {

  implicit val ec = ExecutionContext.Implicits.global

  implicit val mat = ActorMaterializer()(system)

  implicit val dbLayer = new DbLayer(new PhantomService {
    override def database: MicroServicesDb = ProductionDb
  }, new Neo4jService(new Neo4jHelper) {})


  override protected def beforeAll(): Unit = {
    super.beforeAll()
    neo4jInit()
  }

  override protected def afterAll() = {
    neo4jCleanup()
    super.afterAll()
  }

  def generateRandomId = UUID.randomUUID().toString

  val scheduleId = Constants.DEFAULT
  val siteId = generateRandomId
  val nodeId = generateRandomId
  val orgid = generateRandomId
  val scheduleName = "Test schedule"
  val scheduleDescription = "Default site schedule"
  val actionList = new util.ArrayList[util.HashMap[String, Object]]
  val actionMap = new util.HashMap[String, Object]
  actionMap.putIfAbsent("time", "00:00:00")
  actionMap.putIfAbsent("level", 100.asInstanceOf[Object])
  actionMap.putIfAbsent("id", generateRandomId)
  actionList.add(actionMap)
  val actions = actionList
  private val daysList = util.Arrays.asList("mon", "tue", "wed", "thu", "fri", "sat", "sun")
  val event = new util.HashMap[String, Object]
  val eventList = new util.ArrayList[util.HashMap[String, Object]]
  event.putIfAbsent("days", daysList)
  event.putIfAbsent("actions", actions)
  event.putIfAbsent("photocell_enabled", true.asInstanceOf[Object])
  event.putIfAbsent("photocell_lowLevel", 0.asInstanceOf[Object])
  event.putIfAbsent("photocell_highLevel", 100.asInstanceOf[Object])
  eventList.add(event)

  val network = new util.HashMap[String, Object]
  network.putIfAbsent("highTime", "18:00:00")
  network.putIfAbsent("lowTime", "00:00:00")
  network.putIfAbsent("highLevel", Constants.DEFAULT_DRIVER_LEVEL_HIGH.asInstanceOf[Object])
  network.putIfAbsent("lowLevel", Constants.DEFAULT_DRIVER_LEVEL_LOW.asInstanceOf[Object])
  network.putIfAbsent("photocell_enabled", true.asInstanceOf[Object])
  network.putIfAbsent("photocell_lowLevel", 0.asInstanceOf[Object])
  network.putIfAbsent("photocell_highLevel", 100.asInstanceOf[Object])

  def neo4jCleanup(): Unit = {
    val innerMap = new util.HashMap[String, Object]
    val propsMap = new util.HashMap[String, Object]
    innerMap.putIfAbsent("scheduleid", scheduleId)
    propsMap.putIfAbsent(Constants.PROPS, innerMap)
    propsMap.putIfAbsent("siteid", siteId)
    propsMap.putIfAbsent("nodeid", nodeId)
    propsMap.putIfAbsent("orgid", orgid)
    lazy val deleteSite = s"MATCH (a:Site {siteid: {siteid}})" +
      s" DETACH DELETE a RETURN" +
      s" { success: true }"
    val deleteNode = s"MATCH (a:Node {nodeid: '$nodeId'})" +
      s" DETACH DELETE a RETURN" +
      s" { success: true }"
    val deleteOrg = s"MATCH (o:Org {orgid: '$orgid'})" +
      s" DETACH DELETE o RETURN" +
      s" { success: true }"

    lazy val neo4jJobList = List(
      (deleteOrg, propsMap),
      (delete_schedule, propsMap),
      (deleteSite, propsMap),
      (deleteNode, propsMap))

    lazy val neo4JSession = Neo4jConnector.driver.getOrElse(throw new RuntimeException("Neo4j Driver not found")).session()

    neo4jJobList.foreach ( x =>
      try {
        neo4JSession.run(x._1, x._2)
      } catch {
        case ex: Exception => log.error("Unable to cleanup the graph db " + ex)
      })
  }

  def neo4jInit() = {

    val createSchedule = create_schedule
    val innerProps = new util.HashMap[String, Object]()
    innerProps.putIfAbsent("events", eventList)
    innerProps.putIfAbsent("network", network)
    val schedulePropsMap = new util.HashMap[String, Object]()
    schedulePropsMap.putIfAbsent(Constants.PROPS, innerProps)
    schedulePropsMap.putIfAbsent("scheduleid", scheduleId)
    schedulePropsMap.putIfAbsent("siteid", siteId)
    schedulePropsMap.putIfAbsent("name", scheduleName)
    schedulePropsMap.putIfAbsent("description", scheduleDescription)


    val sitename = "Test Site 2"
    val nodename = "Sample Node 2"
    val siteid = siteId
    val nodeid = nodeId
    val street = "900 Chelmsford street"
    val city = "Lowell"
    val state = "MA"
    val zipcode = "01867"
    val country = "USA"
    val latitude = "42.614685"
    val longitude = "-71.324845"
    val timezone = "America/New_York"
    val nodetype = "cnext"

    val create_org = s"""WITH { props } AS props
                         |MERGE (o:Org:Active {
                         |orgid: props.orgprops.orgid,
                         |name: props.orgprops.name,
                         |type: props.orgprops.type,
                         |street1: props.orgprops.street1,
                         |city: props.orgprops.city,
                         |state: props.orgprops.state,
                         |postal_code: props.orgprops.postal_code,
                         |country: props.orgprops.country,
                         |contact_email: props.orgprops.contact_email,
                         |contact_phone: props.orgprops.contact_phone,
                         |contact_name: props.orgprops.contact_name,
                         |created: timestamp()
                         |}) return o
                         |""".stripMargin

    val orgType: String = "test"
    val orgName: String = "Sample Org"
    val orgEmail: String = "test@test.com"
    val orgPhone: String = "9999999999"
    val orgId: String = orgid
    val orgContactName: String = "TEST"
    val innerOrgPropsMap = new util.HashMap[String, Object]()
    val propsMap = new util.HashMap[String, Object]()
    val orgPropsMap = new util.HashMap[String, Object]()
    orgPropsMap.putIfAbsent("po", generateRandomId)
    orgPropsMap.putIfAbsent("orgid", orgId)
    orgPropsMap.putIfAbsent("name", orgName)
    orgPropsMap.putIfAbsent("type", orgType)
    orgPropsMap.putIfAbsent("street1", street)
    orgPropsMap.putIfAbsent("street2", "")
    orgPropsMap.putIfAbsent("city", city)
    orgPropsMap.putIfAbsent("state", state)
    orgPropsMap.putIfAbsent("postal_code", zipcode)
    orgPropsMap.putIfAbsent("country", country)
    orgPropsMap.putIfAbsent("contact_email", country)
    orgPropsMap.putIfAbsent("contact_phone", orgPhone)
    orgPropsMap.putIfAbsent("contact_name", orgContactName)
    innerOrgPropsMap.putIfAbsent("orgprops", orgPropsMap)
    innerOrgPropsMap.putIfAbsent("user", generateRandomId)
    propsMap.putIfAbsent(Constants.PROPS, innerOrgPropsMap)

    val create_site = s"MERGE (site:Site:Active {siteid: '$siteid', name: '$sitename', street1: '$street'," +
      s" city: '$city', state: '$state', postal_code: '$zipcode', country: '$country'," +
      s" latitude: '$latitude', longitude: '$longitude', time_zone: '$timezone'})"

    val createNode =
      s"MATCH (site: Site) WHERE site.siteid = '$siteid'" +
        s"MATCH (sch: Schedule) WHERE sch.scheduleid = '$scheduleId'" +
        s"MATCH (org: Org) WHERE org.orgid = '$orgId'" +
        s"MERGE (site)-[:HAS]->(node:Node:Active {nodeid: '$nodeid', name: '$nodename'," +
        s" building: '2', longitude: $longitude, latitude: $latitude, level: 3," +
        s" time_zone: '$timezone', model: '$nodetype'})-[:BELONGS_TO]->(site) " +
        s"MERGE (node)-[:BELONGS_TO]->(Org)"
    s"MERGE (sch)-[:HAS]->(node)"

    val nodePropsMap = new util.HashMap[String, Object]()
    val sitePropsMap = new util.HashMap[String, Object]()

    lazy val neo4jJobList = List(
      (create_org, propsMap),
      (create_site, sitePropsMap),
      (createSchedule, schedulePropsMap),
      (createNode, nodePropsMap)
    )
    lazy val neo4JSession = Neo4jConnector.driver.getOrElse(throw new RuntimeException("Neo4j Driver not found")).session()

    neo4jJobList.foreach ( x =>
      try {
        neo4JSession.run(x._1, x._2)
      } catch {
        case ex: Exception => log.error("***** Failed in Initializing Neo4j DB " + x._1 + " " + x._2 + " " + ex)
      }
    )
  }

  def randomUUID = UUID.randomUUID().toString
  val nodeid = nodeId
  val scheduleid = randomUUID
  val date = Calendar.getInstance().toInstant.toString
  val loginRequest = RestPack(a = "UNSOL",
    p = s"v1/$nodeid/out/UNSOL/loginReq",
    sid = nodeid,
    d = date,
    uuid = randomUUID,
    f = "",
    l = null)

  /*
  *  Integration Tests with Neo4j Dependency
  * */

  "ScheduleAssignGraph" should "apply default scheduleId for new LoginReq" in {

    lazy val GetDataFromNodeFlowUnderTest = ScheduleAssignService().getDataFromNodeFlow

    val probe = Source.single(loginRequest)
      .via(GetDataFromNodeFlowUnderTest).runWith(TestSink.probe)

    val request = probe.request(1)
    val next = request.requestNext(5.second)
    next.nodeids mustBe Vector(loginRequest.sid)
    next.scheduleid mustBe Some(Constants.DEFAULT)

  }

  it should "get the node hierarchy for new loginRequest" in {


    lazy val GetDataFromNodeFlowUnderTest = ScheduleAssignService().getDataFromNodeFlow
    lazy val GetScheduleToBeSendForNodeUnderTest = ScheduleAssignService().getScheduleToBeSendForNode
    lazy val ScheduleToSTSWrapperFlowUnderTest = ScheduleAssignService().wrapLightScheduleToSTS

    val probe = Source.single(loginRequest)
      .via(GetDataFromNodeFlowUnderTest)
      .via(GetScheduleToBeSendForNodeUnderTest)
      .via(ScheduleToSTSWrapperFlowUnderTest).runWith(TestSink.probe)

    val request = probe.request(1)

    val next = request.requestNext(10.seconds)
    next.nodeids mustBe Some(Vector(nodeid))

  }


  it should "unmarshalling elements using the class type" in {
    val unMarshallFlowUnderTest = ScheduleAssignService().eventUnmarshallFlow[RestPack]
    val testProbe = Source.single(loginRequest.toJsonArray).viaMat(unMarshallFlowUnderTest)(Keep.right)
      .runWith(TestSink.probe)
    val probeRequest: Probe[RestPack] = testProbe.request(2)
    val request1 = probeRequest.requestNext(10.seconds)
    request1 mustBe loginRequest
  }


  it should "handle errors while unmarshalling elements" in {

    val unMarshallFlowUnderTest = ScheduleAssignService().eventUnmarshallFlow[RestPack]
    val dummyMsgToTestTheFailure = "!@#$%^&".getBytes
    val sourceList = List(dummyMsgToTestTheFailure, loginRequest.toJsonArray)
    val testProbe = Source(sourceList).viaMat(unMarshallFlowUnderTest)(Keep.right).runWith(TestSink.probe)
    val probeRequest: Probe[RestPack] = testProbe.request(2)
    val request1 = probeRequest.requestNext(10.seconds)
    request1 mustBe loginRequest
  }

  it should "consume the CASEL trigger message and get the timezone for the timestamp" in {
    val timeStamp = "2018-02-13T00:10:29.285-05:00"
    val service: ScheduleAssignService = ScheduleAssignService()
    val unmarshallingFlowUnderTest = service.eventUnmarshallFlow[ScheduleLoopTrigger]
    val getTimezoneFlowUnderTest = service.getTimeZonesOfMidnightByTimeStampFlow
    val mockEvent = ScheduleLoopTrigger(randomUUID, randomUUID,
      request = ScheduleTriggerRequestProps(randomUUID, randomUUID,
        Calendar.getInstance().toInstant.toString,
      "applySchedule",
      "ScheduleModel",
        randomUUID,
        TriggerExtProps(Some(timeStamp))))
    val testProbe = Source.single(mockEvent.toJsonArray)
      .via(unmarshallingFlowUnderTest).via(getTimezoneFlowUnderTest).runWith(TestSink.probe)
    val request: Probe[Vector[String]] = testProbe.request(1)
    val next: Vector[String] = request.requestNext()
    assert(next.nonEmpty)
    assert(next.contains("America/New_York"))
  }

  it should "get the timezone(s) and schedules attached to the site that are midnight" in {
    val timeStamp = "2018-02-13T00:10:29.285-05:00"
    val service: ScheduleAssignService = ScheduleAssignService()
    val unmarshallingFlowUnderTest = service.eventUnmarshallFlow[ScheduleLoopTrigger]
    val getTimezoneFlowUnderTest = service.getTimeZonesOfMidnightByTimeStampFlow
    val getSchFromDbUnderTest = service.getSchedulesForMidnightTimezoneFlow
    val getSchObjForListofSchedules = service.getSendScheduleToNodeForMidNight
    val mockEvent = ScheduleLoopTrigger(randomUUID, randomUUID,
      request = ScheduleTriggerRequestProps(randomUUID, randomUUID,
        Calendar.getInstance().toInstant.toString,
      "applySchedule",
      "ScheduleModel",
        randomUUID,
        TriggerExtProps(Some(timeStamp))))
    val testProbe: Probe[EssentialNodeHierarchyFields] = Source.single(mockEvent.toJsonArray)
      .via(unmarshallingFlowUnderTest).via(getTimezoneFlowUnderTest)
      .via(getSchFromDbUnderTest).via(getSchObjForListofSchedules).runWith(TestSink.probe)
    val request = testProbe.request(1)
    val result = request.requestNext()
    assert(result.scheduleid.isDefined)
  }

}
