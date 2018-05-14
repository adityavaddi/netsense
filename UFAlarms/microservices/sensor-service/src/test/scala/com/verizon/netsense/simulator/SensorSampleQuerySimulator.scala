package com.verizon.netsense.simulator

import java.io.{InputStream, InputStreamReader}
import java.util
import java.util.{Calendar, UUID}

import akka.actor.ActorSystem
import akka.kafka.ProducerSettings
import akka.stream.scaladsl.{Sink, Source}
import akka.stream.{ActorMaterializer, ThrottleMode}
import com.github.tototoshi.csv.CSVReader
import com.typesafe.config.ConfigFactory
import com.verizon.netsense.connector.Neo4jConnector
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.model._
import com.verizon.netsense.utils.{Logging, TimeConverter}
import nl.grons.metrics.scala.Timer
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.ByteArraySerializer

import scala.concurrent.duration._
import scala.util.Random

/**
  * Created by maidapr on 12/14/17.
  */
object SensorSampleQuerySimulator extends App with Logging with Instrumented with TimeConverter {

  implicit val system = ActorSystem("Kafka-Producer-Actor-System")
  implicit val mat = ActorMaterializer()
  implicit val ec = system.dispatcher

  lazy val config = ConfigFactory.load()

  // Graphite Configuration
  lazy val graphiteConfig = config.getConfig("graphite")

  lazy val graphiteHost = graphiteConfig.getString("host")
  lazy val graphitePort = graphiteConfig.getInt("port")
  // Kafka Configuration
  lazy val kafkaConfig = config.getConfig("kafka")
  lazy val kafkaHost = kafkaConfig.getString("host")
  lazy val kafkaPort = kafkaConfig.getInt("port")

  lazy val kafkaRequestTopic = kafkaConfig.getString("request-topic")
  lazy val kafkaResponseTopic = kafkaConfig.getString("response-topic")

  log.info("Connecting to kafka: " + kafkaHost + ":" + kafkaPort)
  log.info("Using Kafka topic for simulation:" + kafkaRequestTopic)

  lazy val service = "simulator"

  lazy val throttleLevel: Int = config.getInt("simulator.query.throttle")

  private[this] val producerTimer: Timer = metrics.timer("kafka-producer-timer")

  case class NodeSensor(nodeid: String, sensor: String)

  private val inputStream: InputStream = this.getClass.getClassLoader.getResourceAsStream("nodesensors.csv")
  private val inputStreamReader: InputStreamReader = new InputStreamReader(inputStream)

  implicit val reader = CSVReader.open(inputStreamReader)
  implicit val elementList = reader.all().head

  val regex = "NodeSensor\\((.*),(.*)\\)".r

  object NodeSensorString {
    def unapply(str: String): Option[NodeSensor] = str match {
      case regex(n, s) => Some(NodeSensor(n, s))
      case _ => None
    }
  }

  def randomElement = NodeSensorString.unapply(elementList(Random.nextInt(999)))

  lazy val maxSimulatedCount = config.getString("simulator.query.maxlimit") match {
    case "none" => Long.MaxValue
    case x => x.toLong
  }

  lazy val userId = config.getString("simulator.query.userid").toLowerCase() match {
    case "none" => UUID.randomUUID().toString
    case x => x
  }

  lazy val producerSettings = ProducerSettings(system, new ByteArraySerializer, new ByteArraySerializer)
    .withBootstrapServers(kafkaHost + ":" + kafkaPort)

  lazy val siteId = config.getString("simulator.query.siteid")
  lazy val orgId = config.getString("simulator.query.orgid")

  def generateRandomEvent(userId: String): SensorQueryEnvelope = {

    lazy val currentTimeInMillis = System.currentTimeMillis()
    lazy val fromTimeStamp = convertMicrosToISO(currentTimeInMillis * 1000 - 10)
    lazy val toTimeStamp = convertMicrosToISO(currentTimeInMillis * 1000 + 10)
    lazy val randomNode = randomElement
    lazy val timestamp = Calendar.getInstance().toInstant.toString
    lazy val `type` = "NodeModel"
    lazy val model = "getSensorHistoryFromTo"
    lazy val action = "CAN_READ"
    lazy val requestId = UUID.randomUUID().toString
    val nodeProps = NodeProps(randomNode.get.nodeid)
    val siteProps = SiteProps(siteId)
    val orgProps = OrgProps(orgId)
    val extProps = ExtProps(randomNode.get.sensor, "", fromTimeStamp, Some(toTimeStamp), limit = 10,
      period = Some("15min"))

    SensorQueryEnvelope(
      UUID.randomUUID().toString,
      kafkaResponseTopic,
      SensorQueryPayload(
        requestId,
        `type`,
        model,
        action,
        instanceid = UUID.randomUUID().toString,
        timestamp,
        userId,
        service,
        nodeProps,
        siteProps,
        orgProps,
        extProps)
    )
  }

  def generateRandomId = UUID.randomUUID().toString

  def getAllNodes = elementList.map(x => NodeSensorString.unapply(x))

  val sitename = "Sample Site"
  val nodename = "Sample Node"
  val siteid = "ubserSite"
  val orgid = generateRandomId
  val nodeid = "uberNode"
  val street = "900 Chelmsford street"
  val city = "Lowell"
  val state = "MA"
  val zipcode = "01867"
  val country = "USA"
  val latitude = "42.614685"
  val longitude = "-71.324845"
  val timezone = "America/New_York"
  val nodetype = "unode-v7"


  def createSiteInGraphDb(_siteId: String = siteid) = {

    val initSiteSchema = "CREATE CONSTRAINT ON (site:Site) ASSERT site.siteid IS UNIQUE;"

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
    propsMap.putIfAbsent("props", innerOrgPropsMap)

    val create_site = s"MERGE (site:Site:Active {siteid: '$siteid', name: '$sitename', street1: '$street'," +
      s" city: '$city', state: '$state', postal_code: '$zipcode', country: '$country'," +
      s" latitude: '$latitude', longitude: '$longitude', time_zone: '$timezone'})"

    val sitePropsMap = new util.HashMap[String, Object]()

    lazy val neo4jJobList = List(
      (initSiteSchema, propsMap),
      (create_org, propsMap),
      (create_site, sitePropsMap)
    )
    lazy val neo4JSession = Neo4jConnector.driver.getOrElse(throw new RuntimeException("Neo4j Driver not found")).session()

    neo4jJobList.foreach ( x =>
      try {
        log.info("executing query " + x._1)
        neo4JSession.run(x._1, x._2)
      } catch {
        case ex: Exception => log.error("***** Failed in Initializing Neo4j DB " + x._1 + " " + x._2 + " " + ex)
      }
    )
  }

  def createNodeInGraphDb(_nodeid: String, sensorid: String) =  {

    val sitename = "Site for Simulator"
    val nodename = "Simulated Node "
    val siteId = siteid
    val nodeid = _nodeid
    val street = "900 Chelmsford street"
    val city = "Lowell"
    val state = "MA"
    val zipcode = "01867"
    val country = "USA"
    val latitude = "42.614685"
    val longitude = "-71.324845"
    val timezone = "America/New_York"
    val nodetype = "unode-v7"

    val createNode =
      s"""MATCH (site: Site) WHERE site.siteid = '$siteId'
         |MERGE (site)-[:HAS]->(node:Node:Active {nodeid: '$nodeid', name: '$nodename' + '$nodeid',
         |building: '2', longitude: $longitude, latitude: $latitude, level: 3,
         |time_zone: '$timezone', model: '$nodetype'})-[:BELONGS_TO]->(site)
         |MERGE (node)-[:BELONGS_TO]->(Org)""".stripMargin

    val nodePropsMap = new util.HashMap[String, Object]()
    val sitePropsMap = new util.HashMap[String, Object]()

    lazy val neo4jJobList = List(
      (createNode, nodePropsMap)
    )
    lazy val neo4JSession = Neo4jConnector.driver.getOrElse(throw new RuntimeException("Neo4j Driver not found")).session()

    neo4jJobList.foreach ( x =>
      try {
        log.info("executing query " + x._1 + " props: " + x._2)
        neo4JSession.run(x._1, x._2)
      } catch {
        case ex: Exception => log.error("***** Failed in Initializing Neo4j DB " + x._1 + " " + x._2 + " " + ex)
      }
    )
  }

  def neo4jSiteOrgCleanup(): Unit = {
    val innerMap = new util.HashMap[String, Object]
    val propsMap = new util.HashMap[String, Object]
    propsMap.putIfAbsent("props", innerMap)
    propsMap.putIfAbsent("siteid", siteid)
    propsMap.putIfAbsent("orgid", orgid)
    lazy val deleteSite =
      """MATCH (a:Site {siteid: {siteid}})
        | DETACH DELETE a RETURN
        | { success: true }
      """.stripMargin

    lazy val deleteOrg =
      s"""MATCH (o:Org {orgid: '$orgid'})
         | DETACH DELETE o RETURN
         | { success: true }
       """.stripMargin

    lazy val neo4jJobList = List(
      (deleteOrg, propsMap),
      (deleteSite, propsMap))

    lazy val neo4JSession = Neo4jConnector.driver.getOrElse(throw new RuntimeException("Neo4j Driver not found")).session()

    neo4jJobList.foreach ( x =>
      try {
        neo4JSession.run(x._1, x._2)
      } catch {
        case ex: Exception => log.error("Unable to cleanup the site and node graph db " + ex)
      })
  }

  def neo4jNodeCleanup(nodeId: String): Unit = {
    val innerMap = new util.HashMap[String, Object]
    val propsMap = new util.HashMap[String, Object]
    propsMap.putIfAbsent("props", innerMap)


    val deleteNode = s"""MATCH (a:Node {nodeid: '$nodeId'})
        | DETACH DELETE a RETURN
        | { success: true }
      """.stripMargin

    lazy val neo4jJobList = List(
      (deleteNode, propsMap))

    lazy val neo4JSession = Neo4jConnector.driver.getOrElse(throw new RuntimeException("Neo4j Driver not found")).session()

    neo4jJobList.foreach ( x =>
      try {
        log.info("executing query " + x._1 + " props: " + x._2)
        neo4JSession.run(x._1, x._2)
      } catch {
        case ex: Exception => log.error("Unable to cleanup the nodes from graph db " + ex)
      })
  }

  lazy val createTestNodesinGraphDb = {
    log.info("Creating test nodes")
    createSiteInGraphDb(siteId)
    elementList.foreach(x => createNodeInGraphDb(NodeSensorString.unapply(x).get.nodeid,
      NodeSensorString.unapply(x).get.sensor))
  }

  lazy val cleanUpTestNodesinGraphDb = {
    log.info("Cleaning up test nodes and site ")
    neo4jSiteOrgCleanup()
    elementList.foreach(x =>
      neo4jNodeCleanup(NodeSensorString.unapply(x).get.nodeid))
  }


  lazy val eventSimulator = Source
    .fromIterator(() => Iterator from 0)
    .limit(maxSimulatedCount)
    .map(_ => generateRandomEvent(userId))
    .map { e =>
      log.info("simulating events " + e)
      producerTimer.time(e)
      new ProducerRecord[Array[Byte], Array[Byte]](kafkaRequestTopic, e.toJSON.getBytes(), e.toJSON.getBytes())
    }
    .throttle(throttleLevel, 1.second, throttleLevel, ThrottleMode.Shaping).to(Sink.ignore)


  val task = config.getString("simulator.query.task")

  def doOperation(task: String) = task.toLowerCase match {
    case "cleanup" =>
      log.info("Cleaning up the testdata..")
      cleanUpTestNodesinGraphDb
      log.info(s"Done with task $task")
    case "simulate" =>
      log.info("Starting the simulator..")
      eventSimulator.run()
      log.info(s"Done with task $task")
    case "setup" =>
      log.info("Creating the testdata for quering..")
      createTestNodesinGraphDb
      log.info(s"Done with task $task")
    case _ => log.error(s"Unknown task for simulator.. check `ss_sim_query_task` env variable; currently set to $task")
  }


  doOperation(config.getString("simulator.query.task"))

}