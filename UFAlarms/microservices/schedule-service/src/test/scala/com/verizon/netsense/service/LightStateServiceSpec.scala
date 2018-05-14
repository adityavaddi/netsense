package com.verizon.netsense.service

import java.util.UUID

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.testkit.scaladsl.TestSink
import akka.testkit.TestKit
import com.verizon.netsense.config.LSSConfigLoader
import com.verizon.netsense.connector.KafkaConnection
import com.verizon.netsense.constants.Constants
import com.verizon.netsense.database.{EmbeddedDb, MicroServicesDb, PhantomService, ProductionDb}
import com.verizon.netsense.helper.{BaseSpec, LightStateHelper, Neo4jHelper, Neo4jService}
import com.verizon.netsense.model._
import com.verizon.netsense.utils.Logging
import org.joda.time.DateTime

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext

/**
  * Created by maidapr on 2/25/18.
  */
class LightStateServiceSpec extends TestKit(ActorSystem("LightState-Test-System"))
    with BaseSpec
    with Logging {

  val kafkaConnection = new KafkaConnection(system)
  kafkaConnection.configExternalConsumerSettings(LSSConfigLoader.kafkaGroupId)

  implicit val ec = ExecutionContext.Implicits.global

  implicit val mat = ActorMaterializer()(system)

  implicit val dbLayer = new DbLayer(new PhantomService {
    override def database: MicroServicesDb = EmbeddedDb
  }, new Neo4jService(new Neo4jHelper) {})


  val lightStateHelperObj = new LightStateHelper(dbLayer, kafkaConnection)

  "LightStateService" should "detect the lfs storing failures " in {
    def generateLightObj(nodeId: String): Light = Light(nodeId, Some(0), harvest_trigger = false,
      isscheduled = true,  "scheduled", Some(0), None, None, Some(DateTime.now()))

    val nodeId1: String = "JSV4_1"
    val lightStatusObj1 = LightStatusObj(Some(Constants.LIGHTING_SCHEDULE_EVENT_STR), nodeids = Some(Vector(nodeId1)),
      timeout = Some(2), isScheduled = true,
      light = Some(generateLightObj(nodeId1)), lightControlRequestHeaders =
        Some(LightControlRequestHeaders(success = false, error = Some("error1"))))

    val nodeId2: String = "JSV4_2"
    val lightStatusObj2 = LightStatusObj(Some(Constants.LIGHTING_SCHEDULE_EVENT_STR), nodeids = Some(Vector(nodeId2)),
      timeout = Some(2), isScheduled = true,
      light = Some(generateLightObj(nodeId2)), lightControlRequestHeaders =
        Some(LightControlRequestHeaders(success = false, error = Some("error2"))))

    val nodeId3: String = "JSV4_3"
    val lightStatusObj3 = LightStatusObj(Some(Constants.LIGHTING_SCHEDULE_EVENT_STR), nodeids = Some(Vector(nodeId3)),
      timeout = Some(2), isScheduled = true,
      light = Some(generateLightObj(nodeId3)), lightControlRequestHeaders =
        Some(LightControlRequestHeaders(success = true, error = Some("error3"))))

    val combinedResults = Vector(lightStatusObj1, lightStatusObj2, lightStatusObj3)
    val error: Vector[LightStatusObj] = lightStateHelperObj.responseContainsError(combinedResults)

    assert(error.size == 3)

  }


}
