package com.vz.nsp.trigger.helper


import java.util.Calendar

import com.verizon.netsense.utils.Logging
import com.vz.nsp.trigger.data.TestData
import com.vz.nsp.trigger.model.db.DBTrigger
import com.vz.nsp.trigger.util.BaseSpec
import com.vz.nsp.trigger.db.{PhantomService, ProductionDatabase}
import com.vz.nsp.trigger.dblayer.DbLayer
import com.vz.nsp.trigger.model.casel._
import com.vz.nsp.triggerservice.helper.ReqResGenerator
import org.scalatest.time.{Seconds, Span}
import com.vz.nsp.trigger.model.app.{TriggerRequest, TriggerResource, TriggerResponse}
import com.vz.nsp.trigger.service.{StreamService, TriggerProcessor}
import com.vz.nsp.util.ObjectMapperUtil.toJson

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

/**
  * Created by nagarna on 9/28/17.
  */

class StreamServiceSpec extends BaseSpec with TestData  {

  private implicit val ec = scala.concurrent.ExecutionContext.Implicits.global

  val dbLayer = spy(new DbLayer(new PhantomService with ProductionDatabase {}))
  val reqResGenerator = spy(new ReqResGenerator())
  val triggerHelper = spy(new TriggerProcessor(dbLayer, reqResGenerator))
  val streamHelper = spy(new StreamService(triggerHelper, reqResGenerator))


  "matchCaselTypeAndProcessRequest" should "Return Success-1" in {

    doReturn(Future(Some(dbTrigger))).when(dbLayer).getTriggerByTriggerId(orgId, siteId, "triggerId")
    doReturn(Future(reqResGenerator.successResponseWithoutFuture(appRequestTriggerByID, userid))).when(triggerHelper).getTriggerProcess(orgId, siteId, appRequestTriggerByID)

    val chain = for {
      appResponse <- streamHelper.matchCaselTypeAndProcessRequest(appRequestTriggerByID, orgId, siteId, appRequestTriggerByID.request.`type`)
    } yield appResponse

    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[AppRequest]].messageid.equals(messageId))
    }
  }



  "getAllTriggerCategory" should "Return Success-1" in {

          val appRequestTrigger = AppRequest(messageId, resTopic, RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
            "getAllTriggers", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId), None))

            val dbTriggers = List(dbTrigger,dbTrigger2)


    doReturn(Future(List(dbTriggers))).when(dbLayer).getAllTriggers(orgId, siteId)
    doReturn(Future(reqResGenerator.successResponseWithoutFuture(appRequestTrigger, userid))).when(triggerHelper).getAllTriggersProcess(orgId, siteId, appRequestTrigger)

          val chain1 = for {
            appResponse <- streamHelper.matchCaselTypeAndProcessRequest(appRequestTrigger, orgId, siteId, appRequestTrigger.request.`type`)
          } yield appResponse
          whenReady(chain1) { res =>
            assert(res.asInstanceOf[AppSuccessResponse[AppRequest]].messageid.equals(messageId))
          }
  }


  "PostTriggerCategory" should "Return Success-1" in {


    val appRequestTrigger = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
      "createTrigger", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId), Some(triggerProps)))

    doReturn(Future(reqResGenerator.successResponseWithoutFuture(appRequestTrigger, userid))).when(triggerHelper).postTriggerProcess(orgId, siteId, appRequestTrigger)

    val chain1 = for {
      appResponse <- streamHelper.matchCaselTypeAndProcessRequest(appRequestTrigger, orgId, siteId, appRequestTrigger.request.`type`)
    }yield appResponse

    whenReady(chain1) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[AppRequest]].messageid.equals(messageId))
    }
  }

  "UpdateTriggerCategory" should "Return Success-1" in {


    val appRequestTrigger = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
      "updateTrigger", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId), Some(triggerProps)))

    doReturn(Future(Some(dbTrigger))).when(dbLayer).getTriggerByTriggerId(orgId,siteId,"triggerId")
    doReturn(Future(reqResGenerator.successResponseWithoutFuture(appRequestTrigger, dbTrigger))).when(triggerHelper).updateDBAndGenerateResponse(appRequestTrigger, dbTrigger, triggerReq, "triggerId",orgId,siteId)


    val chain = for {
      appResponse <- streamHelper.matchCaselTypeAndProcessRequest(appRequestTrigger, orgId, siteId, appRequestTrigger.request.`type`)
    }yield appResponse
    whenReady(chain, timeout(Span(200, Seconds))) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[AppRequest]].messageid.equals(messageId))
    }
  }




  "DeleteTriggerCategory" should "Return Success-1" in {

    val appRequestTrigger = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
      "deleteTrigger", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId), Some(triggerProps)))

    doReturn(Future(Some(dbTrigger))).when(dbLayer).getTriggerByTriggerId("triggerId", siteId, orgId)

    val userid = appRequestTrigger.request.user.getOrElse("")
    doReturn(Future(reqResGenerator.successResponseWithoutFuture(appRequestTrigger, userid))).when(triggerHelper).deleteTriggerProcess(orgId, siteId, appRequestTrigger)


    val chain1 = for {
      appResponse <- streamHelper.matchCaselTypeAndProcessRequest(appRequestTrigger, orgId, siteId, appRequestTrigger.request.`type`)
    } yield appResponse


    whenReady(chain1) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[AppRequest]].messageid.equals(messageId))
    }
  }


    "filterTriggerCategory" should "Return Success-1" in {

      val triggerProps = TriggerProps(None,None,Some("severity eq severity"))

      val appRequestTrigger = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
        "filterTrigger", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId), Some(triggerProps)))

      doReturn(Future(Some(dbTrigger))).when(dbLayer).getAllTriggers(siteId, orgId)
      doReturn(Future(reqResGenerator.successResponseWithoutFuture(appRequestTrigger, userid))).when(triggerHelper).searchTrigger(orgId, siteId, appRequestTrigger)

     val x=  toJson(resourceListProps)
      println(x)

      val chain1 = for {
        appResponse <- streamHelper.matchCaselTypeAndProcessRequest(appRequestTrigger, orgId, siteId, appRequestTrigger.request.`type`)
      } yield appResponse


      whenReady(chain1) { res =>
        assert(res.asInstanceOf[AppSuccessResponse[AppRequest]].messageid.equals(messageId))
      }

    }

    it should "Return proper error message when incorrect search option is given " in {
      val triggerProps = TriggerProps(None,None,Some("resourceId eq "))

      val appRequestTrigger = AppRequest(messageId, resTopic, new RequestBody(instanceId, requestId, Calendar.getInstance().toInstant.toString,
        "filterTrigger", "NoneModel", "CAN_READ", Some("user"), OrgProps(orgId), SiteProps(siteId), Some(triggerProps)))

      val result = Await.result(triggerHelper.searchTrigger(orgId, siteId, appRequestTrigger), 3 second)
      assert(result.isInstanceOf[AppFailureResponse])
      assert(result.asInstanceOf[AppFailureResponse].response.error.equals("Search can be performed on any one of these severity, triggerCategory, triggerName"))
    }


}




