package com.vz.nsp.parking.userdataservice.helper

import java.util.{Calendar, UUID}

import com.vz.nsp.parking.db.{PhantomService, ProductionDatabase}
import com.vz.nsp.parking.dblayer.DbLayer
import com.vz.nsp.parking.model._
import com.vz.nsp.parking.userdataservice.db.UserDataConnector
import com.vz.nsp.parking.userdataservice.util.BaseSpec
import org.scalatest.time.{Seconds, Span}
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}


class StreamHelperSpec extends BaseSpec {

  val dbLayer = spy(new DbLayer(new PhantomService with ProductionDatabase {}))
  val appId = UUID.randomUUID().toString
  val userId = UUID.randomUUID().toString
  val userDataId = UUID.randomUUID().toString
  val userDataId1 = UUID.randomUUID().toString
  val createdOn = Calendar.getInstance().getTimeInMillis
  val userData = UserData(appId, userId, userDataId, "datavalue", createdOn, 0, false)
  val orgId = UUID.randomUUID().toString
  val siteId = UUID.randomUUID().toString
  val reqResGenerator = spy(new ReqResGenerator())
  val messageId1 = UUID.randomUUID().toString
  val userDataHelper = spy(new UserDataHelper(dbLayer, reqResGenerator))
  val streamHelper = spy(new StreamHelper(userDataHelper, reqResGenerator))
  val instanceId = "1"
  val resTopic = "responseTopic"
  val requestId = UUID.randomUUID().toString
  val messageId = UUID.randomUUID().toString

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
  }

  override def afterAll(): Unit =
    DatabaseService.cleanup()

  val appUserDataProps = AppUserDataProps(Some(userDataId), userId, appId, Some("datavalue"))

  val appUserDataProps1 = AppUserDataProps(Some(userDataId1), userId, appId, Some("datavalue1"))

  val appRequest = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
    "getAppUserData", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId),
    ConfigProps(None, None, None, None, None, None, None, None, None, None, None,
      None, None, None), appUserDataProps))

  val appRequest1 = AppRequest(messageId1, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
    "getAppUserData", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId),
    ConfigProps(None, None, None, None, None, None, None, None, None, None, None,
      None, None, None), appUserDataProps))

  val appRequestMsgNull = AppRequest(null, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
    "policyTagsAssociation", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId),
    ConfigProps(None, None, None, None, None, None, None, None, None, None, None,
      None, None, None), appUserDataProps))

  val appRequestRspTopicNull = AppRequest(messageId, null, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
    "policyTagsAssociation", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId),
    ConfigProps(None, None, None, None, None, None, None, None, None, None, None,
      None, None, None), appUserDataProps))

  val appRequestRspUserData = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
    "createAppUserData", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId),
    ConfigProps(None, None, None, None, None, None, None, None, None, None, None,
      None, None, None), appUserDataProps))

  val appRequestIrrelevent = AppRequest(messageId, null, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
    "policyTagsAssociation", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId),
    ConfigProps(None, None, None, None, None, None, None, None, None, None, None,
      None, None, None), appUserDataProps))

  val appRequestIrreleventType = AppRequest(messageId, null, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
    "policyTagsAssociation", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId),
    ConfigProps(None, None, None, None, None, None, None, None, None, None, None,
      None, None, None), null))

  val appRequestReqObjNull   = AppRequest(messageId1, resTopic, null)

  val timestamp    = "9867654246885"
  val failRespBody = FailureResponseBody(requestId, timestamp, true, "Has it Failed!!!", 400)
  val appFailResp  = AppFailureResponse(messageId, failRespBody)

  private implicit val ec = scala.concurrent.ExecutionContext.Implicits.global



  /* Method: processRequest */
  "processRequest" should "Return Failure due to \"Message ID NULL\"" in {

    val chain = for {
      appResponse <- streamHelper.processRequest(appRequestMsgNull)
    } yield appResponse


    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.isEmpty)
    }
  }

  "processRequest" should "Return Failure due to \"Response Topic NULL\"" in {

    val chain = for {
      appResponse <- streamHelper.processRequest(appRequestRspTopicNull)
    } yield appResponse


    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId1).==(false))
    }
  }

  "processRequest" should "Return Failure due to \"Request Object NULL\"" in {

    val chain = for {
      appResponse <- streamHelper.processRequest(appRequestReqObjNull)
    } yield appResponse


    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId1).==(false))
    }
  }

  "processRequest" should "Return Failure Case" in {

    doReturn(Future(appFailResp)).when(streamHelper).matchCaselTypeAndProcessRequest(appRequest)

    val chain = for {
      appResponse <- streamHelper.processRequest(appRequest)
    } yield appResponse


    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId))
    }
  }

  "processRequest" should "Return Success Case" in {

    doReturn(Future(Some(userData))).when(dbLayer).getUserData(appId, userId,userDataId)

    val chain = for {
      appResponse <- streamHelper.processRequest(appRequest1)
    } yield appResponse



    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[AppRequest]].messageid.equals(messageId1))
    }
  }

 /*  Method: matchCaselTypeAndProcessRequest */
  "matchCaselTypeAndProcessRequest" should "Return Success-1" in {

    val messageId2 = UUID.randomUUID().toString
    doReturn(Future(List(userData))).when(dbLayer).storeUserData(userData)

    val chain = for {
      appResponse <- streamHelper.matchCaselTypeAndProcessRequest(appRequestRspUserData)
    }yield appResponse

    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[AppRequest]].messageid.equals(messageId))
    }
  }


  "matchCaselTypeAndProcessRequest" should "Return Failure" in {

    doReturn(Future(List(userData))).when(dbLayer).storeUserData((userData))

    val chain = for {
      appResponse <- streamHelper.matchCaselTypeAndProcessRequest(appRequestIrreleventType)
    } yield appResponse

    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId))
    }
  }

  "matchCaselTypeAndProcessRequest" should "Return Failure appRequestReqObjNull" in {

    doReturn(Future(List(userData))).when(dbLayer).storeUserData((userData))

    val appRequestIrreleventType = AppRequest(messageId, null, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
      "validationfailed", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId),
      ConfigProps(None, None, None, None, None, None, None, None, None, None, None,
        None, None, None), null))

    val chain = for {
      appResponse <- streamHelper.matchCaselTypeAndProcessRequest(appRequestIrreleventType)
    } yield appResponse

    whenReady(chain) { res => {
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId))
    }
    }
  }


  "matchCaselTypeAndProcessRequest" should "Return Failure appRequestReqObjNull - Exception" in {

    doReturn(Future(List(userData))).when(dbLayer).storeUserData((userData))

    val appRequestIrreleventType = AppRequest(messageId, null, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
      "createAppUserData", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId),
      ConfigProps(None, None, None, None, None, None, None, None, None, None, None,
        None, None, None), AppUserDataProps(null, null, null, null)))

    val chain = for {
      appResponse <- streamHelper.matchCaselTypeAndProcessRequest(appRequestIrreleventType)
    } yield appResponse

    whenReady(chain) { res => {
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId))
    }
    }
  }




}
