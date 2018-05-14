package com.verizon.netsense.businessalertservice.service

import com.verizon.netsense.businessalertservice.data.TestDataFeed._
import com.verizon.netsense.businessalertservice.db.{BAPhantomService, BusinessAlertsDbLayer, ProductionDatabase}
import com.verizon.netsense.businessalertservice.model._
import com.verizon.netsense.businessalertservice.util.{BARequestResponseGenerator, BaseSpec}

import scala.concurrent.Future

/**
 * Created by jittara on 02/28/18.
 */
class BusinessAlertServiceSpec extends BaseSpec {

  private implicit val ec = scala.concurrent.ExecutionContext.Implicits.global

  val reqRes = spy(new BARequestResponseGenerator)

  val spyDb     = spy(new BusinessAlertsDbLayer(new BAPhantomService with ProductionDatabase {}))
  val bAService = spy(new BusinessAlertService(spyDb, reqRes))

  "BusinessAlertService" should "Get Business Alert by Id Success Response" in {
    doReturn(Future(Some(businessAlert(businessAlertId, "Minor"))))
      .when(spyDb)
      .getBusinessAlertById(orgId, siteId, businessAlertId)
    val chain = for {
      appResponse <- bAService.getBusinessAlertByIdProcess(orgId, siteId, appRequest)
    } yield appResponse

    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[BusinessAlertResponse]].messageid.equals(messageId))
    }
  }

  "BusinessAlertService" should "Get Business Alert by Id Failure Response" in {
    doReturn(Future(None)).when(spyDb).getBusinessAlertById(orgId, siteId, businessAlertId)
    val chain = for {
      appResponse <- bAService.getBusinessAlertByIdProcess(orgId, siteId, appRequest)
    } yield appResponse

    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId))
    }

  }

  it should "Get Business Alert Sys by Id - Positive" in {
    doReturn(Future(Some(businessAlert(businessAlertId, "Minor"))))
      .when(spyDb)
      .getBusinessAlertByIdSys(orgId, siteId, businessAlertId)
    val chain = for {
      appResponse <- bAService.getBusinessAlertSys(orgId, siteId, generateAppRequestForSys(businessAlertId))
    } yield appResponse

    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[BusinessAlertSysResponse]].messageid.equals(messageId))
      assert(
        res
          .asInstanceOf[AppSuccessResponse[BusinessAlertSysResponse]]
          .response
          .result
          .businessAlertId
          .equals(businessAlertId)
      )
    }
  }

  it should "Get Business Alert Sys by Id  - Negative(Inactive Alert)" in {
    doReturn(Future(Some(businessAlert(businessAlertId, "Minor").copy(active = Some(false)))))
      .when(spyDb)
      .getBusinessAlertByIdSys(orgId, siteId, businessAlertId)
    val chain = for {
      appResponse <- bAService.getBusinessAlertSys(orgId, siteId, generateAppRequestForSys(businessAlertId))
    } yield appResponse

    whenReady(chain) { res =>
      res.asInstanceOf[AppFailureResponse].response.status.equals(404)
      res.asInstanceOf[AppFailureResponse].response.success.equals(false)
      res.asInstanceOf[AppFailureResponse].response.error.equals("Cannot access inactive/acknowledged business alert")
    }
  }

  it should "Get Business Alert Sys by Id  - Negative(Acknowledged alert)" in {
    doReturn(Future(Some(businessAlert(businessAlertId, "Minor").copy(severity = Some("Clear")))))
      .when(spyDb)
      .getBusinessAlertByIdSys(orgId, siteId, businessAlertId)
    val chain = for {
      appResponse <- bAService.getBusinessAlertSys(orgId, siteId, generateAppRequestForSys(businessAlertId))
    } yield appResponse

    whenReady(chain) { res =>
      res.asInstanceOf[AppFailureResponse].response.status.equals(404)
      res.asInstanceOf[AppFailureResponse].response.success.equals(false)
      res.asInstanceOf[AppFailureResponse].response.error.equals("Cannot access inactive/acknowledged business alert")
    }
  }

  "BusinessAlertService" should "Get All Business Alerts success response" in {
    doReturn(Future(List(businessAlert(businessAlertId, "Minor"), businessAlert(businessAlertId2, "Minor"))))
      .when(spyDb)
      .getAllBusinessAlerts(orgId, siteId)
    val chain = for {
      appResponse <- bAService.getAllBusinessAlertsProcess(orgId, siteId, appRequest)
    } yield appResponse

    whenReady(chain) { res =>
      assert(res.messageid.equals(messageId))
      res.response.result.size mustBe 2
    }

  }

  ignore should "Dismiss business alert success response" in {
    doReturn(Future(Some(businessAlert(businessAlertId, "Minor"))))
      .when(spyDb)
      .getBusinessAlertById(orgId, siteId, businessAlertId)
    doReturn(businessAlert(businessAlertId, "Clear"))
      .when(reqRes)
      .generateBusinessAlertHistory(businessAlert(businessAlertId, "Clear"), "Clear", "userId", 110L)
    doReturn(Future(true)).when(spyDb).storeBusinessAlertHistory(businessAlertHistory(businessAlertId, "Clear"))
    doReturn(Future(true))
      .when(spyDb)
      .dismissByBusinesssAlertId(orgId, siteId, businessAlertId, "Clear", "userId", 100L)

    val chain = for {
      appResponse <- bAService.dismissBusinessAlertProcess(orgId, siteId, appRequest)
    } yield appResponse

    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[SuccessMessage]].messageid.equals(messageId))
    }

  }

  "BusinessAlertService" should "Dismiss business alert failure response" in {
    doReturn(Future(None)).when(spyDb).getBusinessAlertById(orgId, siteId, businessAlertId)
    doReturn(Future(true)).when(spyDb).storeBusinessAlertHistory(businessAlertHistory(businessAlertId, "Minor"))
    doReturn(Future(true))
      .when(spyDb)
      .dismissByBusinesssAlertId(orgId, siteId, businessAlertId, "Clear", "userId", 100L)

    val chain = for {
      appResponse <- bAService.dismissBusinessAlertProcess(orgId, siteId, appRequest)
    } yield appResponse

    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId))
    }

  }

  "BusinessAlertService" should "Search business alert" in {
    doReturn(Future(List(businessAlert(businessAlertId, "Minor"), businessAlert(businessAlertId2, "Minor"))))
      .when(spyDb)
      .getAllBusinessAlerts(orgId, siteId)

    val chain = for {
      appResponse <- bAService.searchBusinessAlert(orgId, siteId, appRequest)
    } yield appResponse

    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[List[BusinessAlertResponse]]].messageid.equals(messageId))
      res.asInstanceOf[AppSuccessResponse[List[BusinessAlertResponse]]].response.result.size mustBe 2
    }

  }

  "BusinessAlertService" should "Search business alert empty list" in {
    doReturn(Future(List()))
      .when(spyDb)
      .getAllBusinessAlerts(orgId, siteId)

    val chain = for {
      appResponse <- bAService.searchBusinessAlert(orgId, siteId, appRequest)
    } yield appResponse

    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[List[BusinessAlertResponse]]].messageid.equals(messageId))
      res.asInstanceOf[AppSuccessResponse[List[BusinessAlertResponse]]].response.result.size mustBe 0
    }

  }

}
