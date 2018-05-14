package com.verizon.netsense.helper

import java.util
import java.util.UUID

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.testkit.TestKit
import com.outworkers.phantom.connectors.KeySpace
import com.outworkers.phantom.dsl._
import com.verizon.netsense.connector.Neo4jConnector
import com.verizon.netsense.constants.Constants
import com.verizon.netsense.cyphers.Neo4jCyphers
import com.verizon.netsense.database.{EmbeddedDatabase, EmbeddedDb, MicroServicesDb, PhantomService}
import com.verizon.netsense.model.{Light, LightingSchedule}
import com.verizon.netsense.service.{DbLayer, ScheduleTransformer}
import com.verizon.netsense.utils.Logging
import org.apache.cassandra.service.{CassandraDaemon, EmbeddedCassandraService}
import org.joda.time.DateTime

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext}

/**
  * Created by maidapr on 1/14/18.
  */
class NodeHelperSpec extends TestKit(ActorSystem("ScheduleServiceHeplerSpec1-System"))
  with BaseSpec
  with Logging with Neo4jCyphers {

  implicit val ec = ExecutionContext.Implicits.global

  implicit val mat = ActorMaterializer()(system)

  lazy val embeddedCassandraServiceObj = new EmbeddedCassandraService
  lazy val cassandraDaemon = new CassandraDaemon


  override protected def beforeAll(): Unit = {
    super.beforeAll()
//    val start: Unit = embeddedCassandraServiceObj.start()
    neo4jInit()
  }

  override protected def afterAll() = {
//    cassandraDaemon.stop()
//    cassandraDaemon.stopNativeTransport()
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

  lazy val dbLayer = new DbLayer(phantomService = new PhantomService {
    override def database: MicroServicesDb = EmbeddedDb
  }, neo4jService = new Neo4jService(new Neo4jHelper){})

  "DbLayer" should "get the node org hierarchy data from Graph Db " in {

    val nodeId = "1e9613c2-6115-439f-9af9-e23263ce5b08"
    val result = dbLayer.getSiteOrgDataForNode(nodeId)
    result.head.nodeid.get mustBe nodeId
  }

  it should "get the send schedule data from Graph Db " in {

    val scheduleId = Constants.DEFAULT
    val result = dbLayer.getScheduleToSendFromDbById(scheduleId)
    val resultValue: List[LightingSchedule] = Await.result(result, 5.seconds)
    assert(resultValue.nonEmpty)
  }

  ignore should "store and get record from EmbeddedCassandra" in {

    val millis = System.currentTimeMillis()
    lazy val lightProps = Light("node", Some(1), harvest_trigger = false, isscheduled = false, "policy", Some(1),
      Some(scheduleId), Some(millis), Some(DateTime.now()))

    val result = for {
      store <- dbLayer.storeLatestLightMode(lightProps)
      get   <- dbLayer.getLatestLightMode(lightProps.nodeid)
    } yield get
    whenReady(result) { x =>
      x.get.nodeid mustBe lightProps.nodeid
    }
  }

}
