package com.verizon.netsense.whatifservice.service

import com.verizon.netsense.whatifservice.db._
import com.verizon.netsense.whatifservice.model.WhatIfJobResponse
import com.verizon.netsense.whatifservice.model.casel.{AppFailureResponse, AppRequest, AppSuccessResponse}
import com.verizon.netsense.whatifservice.util.{BaseSpec, RequestResponseGenerator}
import com.verizon.netsense.whatifservice.data.TestDataFeed._
import com.verizon.netsense.whatifservice.model.ResponseMessage._
import org.mockito.Mockito

import scala.concurrent.Future

/**
 * Created by maleva on 4/19/18.
 */
class WhatIfServiceSpec extends BaseSpec {

  private implicit val ec = scala.concurrent.ExecutionContext.Implicits.global

  val reqRes = spy(new RequestResponseGenerator)

  val spyDb = spy(new WhatIfDbLayer(new PhantomService {
    override def database: WhatIfServicesDb = EmbeddedDb
  }))

  val sparkReqRes = spy(new SparkJobRESTService(reqRes, spyDb))

  val WhatIfService = spy(new WhatIfService(spyDb, reqRes, sparkReqRes))

  "WhatIfService" should "Get What If by Id Success Response" in {
    doReturn(Future(Some(whatIf)))
      .when(spyDb)
      .getJobById(orgId, siteId, jobId)
    val chain = for {
      appResponse <- WhatIfService.getWhatIfByIdProcess(orgId, siteId, appRequest("getWhatIfJob"))
    } yield appResponse

    whenReady(chain) { res =>
      assert(res._1.asInstanceOf[AppSuccessResponse[WhatIfJobResponse]].messageid.equals(messageId))
    }
  }

  "WhatIfService" should "Get What If by Id Failure Response" in {
    doReturn(Future(None)).when(spyDb).getJobById(orgId, siteId, jobId)
    val chain = for {
      appResponse <- WhatIfService.getWhatIfByIdProcess(orgId, siteId, appRequest("getWhatIfJob"))
    } yield appResponse
    whenReady(chain) { res =>
      assert(res._1.asInstanceOf[AppFailureResponse].messageid.equals(messageId))
    }

  }

  "WhatIfService" should "Get All What If jobs success response" in {
    doReturn(Future(List(whatIf, whatIf2)))
      .when(spyDb)
      .getAllJobs(orgId, siteId)
    val chain = for {
      appResponse <- WhatIfService.getAllWhatIfProcess(orgId, siteId, appRequest("getAllWhatIfJobs"))
    } yield appResponse

    whenReady(chain) { res =>
      assert(res._1.asInstanceOf[AppSuccessResponse[WhatIfJobResponse]].messageid.equals(messageId))
      res._1.response.result.size mustBe 2
    }

  }

  "PostWhatIf" should "Return Failure when name already exists" in {
    val appRequestWhatIf = appRequest("createWhatIfJob")
    val name             = appRequestWhatIf.request.whatifprops.get.whatIfJobRequest.get.name

    doReturn(Future(true)).when(spyDb).whatIfExistWithSameName(orgId, siteId, name, None)

    val chain1 = for {
      appResponse <- WhatIfService.postJobStatusProcess(orgId, siteId, appRequestWhatIf)
    } yield appResponse

    whenReady(chain1) { res =>
      assert(res._1.asInstanceOf[AppFailureResponse].messageid.equals(messageId))
      assert(res._1.asInstanceOf[AppFailureResponse].response.error.equals(WhatIf_Name_Exist.value + name))
    }
  }

  "PostWhatIf" should "Return Failure when invalid policies" in {
    val appRequestWhatIf = appRequest("createWhatIfJob")
    val parkingPolicies  = appRequestWhatIf.request.whatifprops.get.whatIfJobRequest.get.parkingPolicies

    doReturn(Future(false))
      .when(spyDb)
      .whatIfExistWithSameName(orgId, siteId, appRequestWhatIf.request.whatifprops.get.whatIfJobRequest.get.name, None)

    doReturn(Future((List(""), parkingPolicies)))
      .when(WhatIfService)
      .validatePolicies(appRequestWhatIf.request.whatifprops.get.whatIfJobRequest.get.parkingPolicies, orgId, siteId)

    val chain1 = for {
      appResponse <- WhatIfService.postJobStatusProcess(orgId, siteId, appRequestWhatIf)
    } yield appResponse

    whenReady(chain1) { res =>
      assert(res._1.asInstanceOf[AppFailureResponse].messageid.equals(messageId))
      assert(
        res._1
          .asInstanceOf[AppFailureResponse]
          .response
          .error
          .equals(
            Parking_Policies.value + (parkingPolicies mkString ",") + All_Invalid_Jobs.value + orgId
            + "," + Siteid.value + siteId
          )
      )
    }
  }

  "UpdateWhatIf" should "return Failure for invalid jobId" in {
    val appRequestWhatIf = appRequest("createWhatIfJob")

    doReturn(Future(None))
      .when(spyDb)
      .getJobById(orgId, siteId, jobId)

    val chain1 = for {
      appResponse <- WhatIfService.updateWhatIfProcess(orgId, siteId, appRequestWhatIf)
    } yield appResponse

    whenReady(chain1) { res =>
      assert(
        res._1
          .asInstanceOf[AppFailureResponse]
          .response
          .error
          .equals(Missing_WhatIfJobId.value + jobId + Does_Not_Exist_OrgSite.value + orgId + Siteid.value + siteId)
      )
    }
  }

  "UpdateWhatIf" should "return Failure for invalid policies" in {
    val appRequestWhatIf = appRequest("createWhatIfJob")

    doReturn(Future(true)).when(spyDb).whatIfExistWithSameName(orgId, siteId, whatIf2.name.getOrElse(""), None)

    doReturn(Future((List(""), parkingPolicies)))
      .when(WhatIfService)
      .validatePolicies(appRequestWhatIf.request.whatifprops.get.whatIfJobRequest.get.parkingPolicies, orgId, siteId)

    val chain1 = for {
      appResponse <- WhatIfService.updateDBAndGenerateResponse(appRequestWhatIf, whatIf2, whatIfRequest)
    } yield appResponse

    whenReady(chain1) { res =>
      assert(
        res._1
          .asInstanceOf[AppFailureResponse]
          .response
          .error
          .equals(
            Parking_Policies.value + (parkingPolicies mkString ",") + All_Invalid_Jobs.value + orgId
            + "," + Siteid.value + siteId
          )
      )
    }
  }

  "UpdateWhatIf" should "Return Failure for duplicate name" in {
    val appRequestWhatIf = appRequest("createWhatIfJob")

    doReturn(Future(true)).when(spyDb).whatIfExistWithSameName(whatIf.orgid, whatIf.siteid, "", None)

    val chain1 = for {
      appResponse <- WhatIfService.updateDBAndGenerateResponse(appRequestWhatIf, whatIf, whatIfRequest)
    } yield appResponse

    whenReady(chain1) { res =>
      assert(res._1.asInstanceOf[AppFailureResponse].messageid.equals(messageId))
      assert(res._1.asInstanceOf[AppFailureResponse].response.error.equals(WhatIf_Name_Exist.value + name))
    }
  }
}
