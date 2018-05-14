package com.vz.nsp.parking.tagservice.helper

import java.util.Calendar

import com.vz.nsp.parking.db.{PhantomService, ProductionDatabase}
import com.vz.nsp.parking.dblayer.DbLayer
import com.vz.nsp.parking.model.{AppRequest, _}
import com.vz.nsp.parking.tagservice.constants.TestDataFeed._
import com.vz.nsp.parking.tagservice.util.BaseSpec
import org.scalatest.time.{Seconds, Span}
import com.vz.nsp.parking.model.CaselType.productionApi
import scala.concurrent.Future

/**
  * Created by nagarna on 9/28/17.
  */

class StreamHelperSpec extends BaseSpec {

  private implicit val ec = scala.concurrent.ExecutionContext.Implicits.global

  val dbLayer: DbLayer = spy(new DbLayer(new PhantomService with ProductionDatabase {}))
  val reqResGenerator: ReqResGenerator = spy(new ReqResGenerator())
  val tagHelper: TagHelper = spy(new TagHelper(dbLayer, reqResGenerator))
  val streamHelper: StreamHelper = spy(new StreamHelper(tagHelper, reqResGenerator))


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

  "processRequest" should "Return Generic Failure Case - \"Irrelevant type found in the request\"" in {

    val chain = for {
      appResponse <- streamHelper.processRequest(appRequestInvalid)
    } yield appResponse


    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId))
    }
  }

  "processRequest" should "Return Failure Case" in {

    doReturn(Future(appFailResp)).when(streamHelper).matchCaselTypeAndProcessRequest(appRequest, orgId, siteId, appRequest.request.`type`)

    val chain = for {
      appResponse <- streamHelper.processRequest(appRequestStreamHelper)
    } yield appResponse


    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId))
    }
  }


  "processRequest" should "Return Success Case" in {

    doReturn(getTagResponse).when(dbLayer).getTag(orgId, siteId, tagid,productionApi.value)

    val chain = for {
      appResponse <- streamHelper.processRequest(appRequestGetPolicy)
    } yield appResponse


    whenReady(chain, timeout( Span(10, Seconds))) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[AppRequest]].messageid.equals(messageId1))
    }
  }


  "matchCaselTypeAndProcessRequest" should "Return Success-1" in {

    val tagid_forDelete = "tagid_delete"

    val appRequestTag = AppRequest(messageId2, resTopic, RequestBody( instanceId, requestId, Calendar.getInstance().toInstant.toString,
      "deletePolicyCategory", "NoneModel", "CAN_READ", Some( "user" ), OrgProps( orgId ), SiteProps( siteId ),
      ConfigProps( Some( policyId2 ), Some( tagid_forDelete ), None, None, None, None, None, None, None, None,
        None, None, None, None ), AppUserDataProps( None, "", "", None ) ))

    doReturn(getTagResponse).when(dbLayer).getTag(orgId, siteId, tagid_forDelete,productionApi.value)
    doReturn(Future(List())).when(dbLayer).getAllPoliciesByTagid(orgId,siteId,tagid_forDelete,productionApi.value)
    doReturn(Future(true)).when(dbLayer).deleteByTagId(tagid_forDelete,productionApi.value,orgId,siteId)

    val chain = for {
      appResponse <- streamHelper.matchCaselTypeAndProcessRequest(appRequestTag, orgId, siteId, appRequestTag.request.`type`)
    } yield appResponse

    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[AppRequest]].messageid.equals(messageId2))
    }
  }

}
