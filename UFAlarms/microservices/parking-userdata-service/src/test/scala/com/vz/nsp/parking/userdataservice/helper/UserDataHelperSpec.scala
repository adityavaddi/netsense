package com.vz.nsp.parking.userdataservice.helper

import java.util.{Calendar, UUID}

import com.vz.nsp.parking.db.{PhantomService, ProductionDatabase}
import com.vz.nsp.parking.dblayer.DbLayer
import com.vz.nsp.parking.model.{AppRequest, _}
import com.vz.nsp.parking.userdataservice.db.UserDataConnector
import com.vz.nsp.parking.userdataservice.util.BaseSpec
import org.scalatest.time.{Seconds, Span}
import scala.concurrent.duration._


import scala.concurrent.{Await, Future}

class UserDataHelperSpec extends BaseSpec {

  val dbLayer = spy(new DbLayer(new PhantomService with ProductionDatabase {}))
  val reqResGenerator = spy(new ReqResGenerator())
  val userDataHelper = spy(new UserDataHelper(dbLayer, reqResGenerator))
  val appId = UUID.randomUUID().toString
  val userId = UUID.randomUUID().toString
  val userDataId = UUID.randomUUID().toString
  val userDataId1 = UUID.randomUUID().toString
  val createdOn = Calendar.getInstance().getTimeInMillis
  val userData = UserData(appId, userId, userDataId, "datavalue", createdOn, 0, false)

  val messageId = UUID.randomUUID().toString


  private implicit val ec = scala.concurrent.ExecutionContext.Implicits.global

  val orgId = UUID.randomUUID().toString
  val siteId = UUID.randomUUID().toString
  val instanceId = "1"
  val resTopic = "responseTopic"
  val requestId = UUID.randomUUID().toString

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


  /* Method: getUserDataProcess */
  "getUserDataProcess" should "return Success" in {

    val messageId = UUID.randomUUID().toString

    val appId = "appid"
    val userId = "userid"
    val userDataId = "userdataid"
    val appUserDataProps = AppUserDataProps(Some(userDataId), userId, appId, Some("datavalue"))

    val appRequest = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
      "policyTagsAssociation", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId),
      ConfigProps(None, None, None, None, None, None, None, None, None, None, None,
        None, None, None), appUserDataProps))

    val userData = UserData(appId, userId, userDataId, "datavalue", 123456789L, 345678987L, true)

    doReturn(Future(Some(userData))).when(dbLayer).getUserData(appId, userId, userDataId)

    val chain = for {
      appResponse <- userDataHelper.getUserDataProcess(appRequest)
    } yield appResponse


    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[AppRequest]].messageid.equals(messageId))
    }
  }


  "getUserDataProcess" should "return Failure require appUserDataProps" in {

    val messageId = UUID.randomUUID().toString

    val appId = "appid"
    val userId = "userid"
    val userDataId = "userdataid"
    val appUserDataProps = AppUserDataProps(Some(userDataId), userId, appId, Some("datavalue"))

    val appRequest = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
      "policyTagsAssociation", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId),
      ConfigProps(None, None, None, None, None, None, None, None, None, None, None,
        None, None, None), null))

    val chain = for {
      appResponse <- userDataHelper.getUserDataProcess(appRequest)
    } yield appResponse


    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId))
      assert(res.asInstanceOf[AppFailureResponse].response.error.contains("Required appuserdataprops missing in request "))
    }
  }


  "getUserDataProcess" should "return Failure require userId" in {

    val messageId = UUID.randomUUID().toString

    val appId = "appid"
    val userId = "userid"
    val userDataId = "userdataid"
    val appUserDataProps = AppUserDataProps(Some(userDataId), null, appId, Some("datavalue"))

    val appRequest = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
      "policyTagsAssociation", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId),
      ConfigProps(None, None, None, None, None, None, None, None, None, None, None,
        None, None, None), appUserDataProps))

    val chain = for {
      appResponse <- userDataHelper.getUserDataProcess(appRequest)
    } yield appResponse


    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId))
      assert(res.asInstanceOf[AppFailureResponse].response.error.contains("Required userId missing in request"))
    }
  }

  "getUserDataProcess" should "return Failure require appId" in {

    val messageId = UUID.randomUUID().toString

    val appId = "appid"
    val userId = "userid"
    val userDataId = "userdataid"
    val appUserDataProps = AppUserDataProps(Some(userDataId), userId, null, Some("datavalue"))

    val appRequest = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
      "policyTagsAssociation", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId),
      ConfigProps(None, None, None, None, None, None, None, None, None, None, None,
        None, None, None), appUserDataProps))

    val chain = for {
      appResponse <- userDataHelper.getUserDataProcess(appRequest)
    } yield appResponse


    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId))
      assert(res.asInstanceOf[AppFailureResponse].response.error.contains("Required appId missing in request"))
    }
  }


  "getUserDataProcess" should "return Failure require userDataId" in {

    val messageId = UUID.randomUUID().toString

    val appId = "appid"
    val userId = "userid"
    val userDataId = "userdataid"
    val appUserDataProps = AppUserDataProps(None, userId, appId, Some("datavalue"))

    val appRequest = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
      "policyTagsAssociation", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId),
      ConfigProps(None, None, None, None, None, None, None, None, None, None, None,
        None, None, None), appUserDataProps))

    val chain = for {
      appResponse <- userDataHelper.getUserDataProcess(appRequest)
    } yield appResponse


    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId))
      assert(res.asInstanceOf[AppFailureResponse].response.error.contains("Required userDataId missing in request"))
    }
  }

  /* Method: getAllUserDataProcess */
  "getAllUserDataProcess" should "return Success" in {
    doReturn(Future(List(userData))).when(dbLayer).getAllUserData(appId, userId)

    val appUserDataProps = AppUserDataProps(None, userId, appId, Some("datavalue"))

    val appRequest = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
      "getAllAppUserData", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId),
      ConfigProps(None, None, None, None, None, None, None, None, None, None, None,
        None, None, None), appUserDataProps))


    val chain = for {
      appResponse <- userDataHelper.getAllUserDataProcess(appRequest)
    } yield appResponse

    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[UserData]].response.success.equals(true))
    }
  }



  "getAllUserDataProcess" should "return Failure require appUserDataProps" in {

    val messageId = UUID.randomUUID().toString

    val appId = "appid"
    val userId = "userid"
    val appUserDataProps = AppUserDataProps(Some(userDataId), userId, appId, Some("datavalue"))

    val appRequest = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
      "getAllAppUserData", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId),
      ConfigProps(None, None, None, None, None, None, None, None, None, None, None,
        None, None, None), null))

    val chain = for {
      appResponse <- userDataHelper.getAllUserDataProcess(appRequest)
    } yield appResponse

    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId))
      assert(res.asInstanceOf[AppFailureResponse].response.error.contains("Required appuserdataprops missing in request "))
    }
  }


  "getAllUserDataProcess" should "return Failure require userId" in {

    val messageId = UUID.randomUUID().toString

    val appId = "appid"
    val userId = "userid"
    val appUserDataProps = AppUserDataProps(Some(userDataId), null, appId, Some("datavalue"))

    val appRequest = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
      "getAllAppUserData", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId),
      ConfigProps(None, None, None, None, None, None, None, None, None, None, None,
        None, None, None), appUserDataProps))

    val chain = for {
      appResponse <- userDataHelper.getAllUserDataProcess(appRequest)
    } yield appResponse


    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId))
      assert(res.asInstanceOf[AppFailureResponse].response.error.contains("Required userId missing in request"))
    }
  }


  "getAllUserDataProcess" should "return Failure require appId" in {

    val messageId = UUID.randomUUID().toString

    val appId = "appid"
    val userId = "userid"
    val appUserDataProps = AppUserDataProps(Some(userDataId), userId, null, Some("datavalue"))

    val appRequest = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
      "getAllAppUserData", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId),
      ConfigProps(None, None, None, None, None, None, None, None, None, None, None,
        None, None, None), appUserDataProps))

    val chain = for {
      appResponse <- userDataHelper.getAllUserDataProcess(appRequest)
    } yield appResponse


    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId))
      assert(res.asInstanceOf[AppFailureResponse].response.error.contains("Required appId missing in request"))
    }
  }


  /* Method: deleteUserDataProcess */
  "deleteUserDataProcess" should "return Success " in {

    val appId = "appid"
    val userId = "userid"
    val userDataId = "userdataid"
    val appUserDataProps = AppUserDataProps(Some(userDataId), userId, appId, Some("datavalue"))

    doReturn(Future(Some(userData))).when(dbLayer).getUserData(appId, userId, userDataId)

    doReturn(Future(SuccessMessage(true))).when(dbLayer).deleteByUserDataId(appId, userId, userDataId)

    val appRequest = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
      "policyTagsAssociation", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId),
      ConfigProps(None, None, None, None, None, None, None, None, None, None, None,
        None, None, None), appUserDataProps))

    val chain = for {
      appResponse <- userDataHelper.deleteUserDataProcess(appRequest)
    } yield appResponse

    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[AppRequest]].messageid.equals(messageId))
    }
  }


  "deleteUserDataProcess" should "return Failure require appUserDataProps" in {

    val appId = "appid"
    val userId = "userid"
    val userDataId = "userdataid"
    val appUserDataProps = AppUserDataProps(None, userId, appId, Some("datavalue"))

    doReturn(Future(Some(userData))).when(dbLayer).getUserData(appId, userId, userDataId)

    doReturn(Future(SuccessMessage(true))).when(dbLayer).deleteByUserDataId(appId, userId, userDataId)

    val appRequest = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
      "policyTagsAssociation", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId),
      ConfigProps(None, None, None, None, None, None, None, None, None, None, None,
        None, None, None), null))

    val chain = for {
      appResponse <- userDataHelper.deleteUserDataProcess(appRequest)
    } yield appResponse


    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppFailureResponse].response.error.contains("Required appuserdataprops missing in request "))
    }
  }


  "deleteUserDataProcess" should "return Failure require userDataId" in {

    val appId = "appid"
    val userId = "userid"
    val userDataId = "userdataid"
    val appUserDataProps = AppUserDataProps(None, userId, appId, Some("datavalue"))

    doReturn(Future(Some(userData))).when(dbLayer).getUserData(appId, userId, userDataId)

    doReturn(Future(SuccessMessage(true))).when(dbLayer).deleteByUserDataId(appId, userId, userDataId)

    val appRequest = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
      "policyTagsAssociation", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId),
      ConfigProps(None, None, None, None, None, None, None, None, None, None, None,
        None, None, None), appUserDataProps))

    val chain = for {
      appResponse <- userDataHelper.deleteUserDataProcess(appRequest)
    } yield appResponse


    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppFailureResponse].response.error.contains("Required userDataId missing in request "))
    }
  }


  /* Method: postUserDataProcess */
  "postUserDataProcess" should "return Success" in {

    doReturn(Future(SuccessMessage(true))).when(dbLayer).storeUserData(userData)
    val appUserDataProps = AppUserDataProps(Some(userDataId), userId, appId, Some("datavalue"))

    val appRequest = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
      "policyTagsAssociation", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId),
      ConfigProps(None, None, None, None, None, None, None, None, None, None, None,
        None, None, None), appUserDataProps))

    val chain = for {
      appResponse <- userDataHelper.postUserDataProcess(appRequest)
    } yield appResponse


    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[AppRequest]].messageid.equals(messageId))
    }
  }

  "postUserDataProcess" should "return failure require appUserDataProps" in {

    doReturn(Future(SuccessMessage(true))).when(dbLayer).storeUserData(userData)
    val appUserDataProps = AppUserDataProps(Some(userDataId), userId, appId, Some("datavalue"))

    val appRequest = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
      "policyTagsAssociation", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId),
      ConfigProps(None, None, None, None, None, None, None, None, None, None, None,
        None, None, None), null))

    val chain = for {
      appResponse <- userDataHelper.postUserDataProcess(appRequest)
    } yield appResponse


    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppFailureResponse].response.error.contains("Required appuserdataprops missing in request "))
    }
  }


  "postUserDataProcess" should "return failure require appId" in {

    doReturn(Future(SuccessMessage(true))).when(dbLayer).storeUserData(userData)
    val appUserDataProps = AppUserDataProps(Some(userDataId), userId, null, Some("datavalue"))

    val appRequest = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
      "policyTagsAssociation", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId),
      ConfigProps(None, None, None, None, None, None, None, None, None, None, None,
        None, None, None), appUserDataProps))

    val chain = for {
      appResponse <- userDataHelper.postUserDataProcess(appRequest)
    } yield appResponse


    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppFailureResponse].response.error.contains("Required appId missing in request "))
    }
  }


  "postUserDataProcess" should "return failure require userId" in {

    doReturn(Future(SuccessMessage(true))).when(dbLayer).storeUserData(userData)
    val appUserDataProps = AppUserDataProps(Some(userDataId), null, appId, Some("datavalue"))

    val appRequest = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
      "policyTagsAssociation", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId),
      ConfigProps(None, None, None, None, None, None, None, None, None, None, None,
        None, None, None), appUserDataProps))

    val chain = for {
      appResponse <- userDataHelper.postUserDataProcess(appRequest)
    } yield appResponse


    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppFailureResponse].response.error.contains("Required userId missing in request "))
    }
  }

  "postUserDataProcess" should "return failure require dataValue" in {

    doReturn(Future(SuccessMessage(true))).when(dbLayer).storeUserData(userData)
    val appUserDataProps = AppUserDataProps(Some(userDataId), userId, appId, None)

    val appRequest = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
      "policyTagsAssociation", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId),
      ConfigProps(None, None, None, None, None, None, None, None, None, None, None,
        None, None, None), appUserDataProps))

    val chain = for {
      appResponse <- userDataHelper.postUserDataProcess(appRequest)
    } yield appResponse


    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppFailureResponse].response.error.contains("Required data value/Template data is missing in request "))
    }
  }


  /* Method: updateUserDataProcess */
  "updateUserDataProcess" should "update userdata id" in {

    val appUserDataProps = AppUserDataProps(Some(userDataId), userId, appId, Some("datavalue"))

    val appRequest = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
      "policyTagsAssociation", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId),
      ConfigProps(None, None, None, None, None, None, None, None, None, None, None,
        None, None, None), appUserDataProps))

    val chain = for {
      appResponse <- userDataHelper.updateUserDataProcess(appRequest)
    } yield appResponse


    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId))
    }
  }


  "updateUserDataProcess" should "return failure require userId" in {

    val appUserDataProps = AppUserDataProps(Some(userDataId), null, appId, Some("datavalue"))

    val appRequest = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
      "policyTagsAssociation", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId),
      ConfigProps(None, None, None, None, None, None, None, None, None, None, None,
        None, None, None), appUserDataProps))

    val chain = for {
      appResponse <- userDataHelper.updateUserDataProcess(appRequest)
    } yield appResponse


    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppFailureResponse].response.error.contains("Required userId missing in request "))
    }
  }

  "updateUserDataProcess" should "return failure require appUserDataProps" in {

    val appUserDataProps = AppUserDataProps(Some(userDataId), userId, appId, Some("datavalue"))

    val appRequest = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
      "policyTagsAssociation", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId),
      ConfigProps(None, None, None, None, None, None, None, None, None, None, None,
        None, None, None), null))

    val chain = for {
      appResponse <- userDataHelper.updateUserDataProcess(appRequest)
    } yield appResponse


    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppFailureResponse].response.error.contains("Required appuserdataprops missing in request "))
    }
  }

  "updateUserDataProcess" should "return failure require appId" in {

    val appUserDataProps = AppUserDataProps(Some(userDataId), userId, null, Some("datavalue"))

    val appRequest = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
      "policyTagsAssociation", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId),
      ConfigProps(None, None, None, None, None, None, None, None, None, None, None,
        None, None, None), appUserDataProps))

    val chain = for {
      appResponse <- userDataHelper.updateUserDataProcess(appRequest)
    } yield appResponse


    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppFailureResponse].response.error.contains("Required appId missing in request "))
    }
  }

  "updateUserDataProcess" should "return failure require dataValue" in {

    val appUserDataProps = AppUserDataProps(Some(userDataId), userId, appId, None)

    val appRequest = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
      "policyTagsAssociation", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId),
      ConfigProps(None, None, None, None, None, None, None, None, None, None, None,
        None, None, None), appUserDataProps))

    val chain = for {
      appResponse <- userDataHelper.updateUserDataProcess(appRequest)
    } yield appResponse


    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppFailureResponse].response.error.contains("Required data value/Template data is missing in request "))
    }
  }

  "updateUserDataProcess" should "return failure require userDataId" in {

    val appUserDataProps = AppUserDataProps(None, userId, appId, Some("dataValue"))

    val appRequest = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
      "policyTagsAssociation", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId),
      ConfigProps(None, None, None, None, None, None, None, None, None, None, None,
        None, None, None), appUserDataProps))

    val chain = for {
      appResponse <- userDataHelper.updateUserDataProcess(appRequest)
    } yield appResponse


    whenReady(chain, timeout(Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppFailureResponse].response.error.contains("Required userDataId missing in request "))
    }
  }



}




