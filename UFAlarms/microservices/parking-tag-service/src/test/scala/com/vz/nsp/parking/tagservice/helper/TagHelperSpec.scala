package com.vz.nsp.parking.tagservice.helper

import com.vz.nsp.parking.db.{PhantomService, ProductionDatabase}
import com.vz.nsp.parking.dblayer.DbLayer
import com.vz.nsp.parking.model.{Tag, _}
import com.vz.nsp.parking.tagservice.constants.TestDataFeed._
import com.vz.nsp.parking.tagservice.util.BaseSpec
import org.scalatest.time.{Seconds, Span}
import com.vz.nsp.parking.model.CaselType.{productionApi,syntheticApi}

import scala.concurrent.Future

/**
 * Created by thangth on 8/14/17.
 */
class TagHelperSpec extends BaseSpec {

  private implicit val ec = scala.concurrent.ExecutionContext.Implicits.global

  val reqRes    = spy(new ReqResGenerator)
  val spyDb     = spy(new DbLayer(new PhantomService with ProductionDatabase {}))
  val tagHelper = spy(new TagHelper(spyDb, reqRes))

  "Get Tag by Id Success Response" should "Get Tag by Id Success Response" in {
    doReturn(getTagResponse).when(spyDb).getTag(orgId, siteId, tagId, productionApi.value)
    val chain = for {
      appResponse <- tagHelper.getParkingTagProcess(orgId, siteId, productionApi.value, appRequestGetTag)
    } yield appResponse

    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[Tag]].messageid.equals(messageId))
    }
  }

  "Get Tag by Id Failure Response" should "Get Tag by Id Failure Response" in {
    doReturn(getTagByIdfailure).when(spyDb).getTag(orgId, siteId, tagId, productionApi.value)
    val chain = for {
      appResponse <- tagHelper.getParkingTagProcess(orgId, siteId, productionApi.value, appRequestGetTag)
    } yield appResponse

    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId))
    }

  }

  "Get All Tags" should "Get All Tag" in {
    doReturn(tag1).when(spyDb).getAllTag(orgId, siteId, productionApi.value)
    val chain = for {
      appResponse <- tagHelper.getAllParkingTagProcess(orgId, siteId, productionApi.value, getAllTagAppRequest)
    } yield appResponse

    whenReady(chain) { res =>
      assert(res.messageid.equals(messageId))
      res.response.result.size mustBe 2
    }

  }

  "delete Tag Failure" should "return getTagByIdfailure since Policy Associated with Tag " in {
    doReturn(listOfTags).when(spyDb).getTag(orgId, siteId, tagId, productionApi.value)
    doReturn(futureSuccessMessage).when(spyDb).deleteByTagId(tagId, productionApi.value,orgId,siteId)
    doReturn(listOfPolicies).when(spyDb).getAllPoliciesByTagid(orgId,siteId,tagId,productionApi.value)
    val chain = for {
      appResponse <- tagHelper.deleteParkingTagProcess(orgId, siteId, productionApi.value, getAllTagAppRequest)
    } yield appResponse

    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId))
    }

  }

  "delete Tag Success" should "return success since Policy not Associated with Tag " in {
    doReturn(getTagResponse).when(spyDb).getTag(orgId, siteId, tagId, productionApi.value)
    doReturn(futureSuccessMessage).when(spyDb).deleteByTagId(tagId, productionApi.value,orgId,siteId)
    doReturn(emptyPoliciesList).when(spyDb).getAllPoliciesByTagid(orgId,siteId,tagId,productionApi.value)
    val chain = for {
      appResponse <- tagHelper.deleteParkingTagProcess(orgId, siteId, productionApi.value, getAllTagAppRequest)
    } yield appResponse

    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[Tag]].messageid.equals(messageId))
    }

  }

  "post Tag Success" should "Create Tag successful" in {
    doReturn(posTag).when(reqRes).generateTagObject(tagPostRequest, orgId, siteId)
    doReturn(futureSuccessMessage).when(spyDb).storeTag(posTag, productionApi.value)

    val chain = for {
      appResponse <- tagHelper.postParkingTagProcess(orgId, siteId, productionApi.value, appRequestCreateTag)
    } yield appResponse

    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[TagResponse]].response.result.tagId == tagId)
    }
  }

  "post Tag Failure" should "Create Tag Failure" in {
    doReturn(posTag).when(reqRes).generateTagObject(tagPostRequest, orgId, siteId)
    doReturn(futureFailureMessage).when(spyDb).storeTag(posTag, productionApi.value)

    val chain = for {
      appResponse <- tagHelper.postParkingTagProcess(orgId, siteId, productionApi.value, appRequestCreateTag)
    } yield appResponse

    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid == messageId)

    }
  }

  "post Tag Without TagRequest Failure" should "should result Tag Failure" in {
    val chain = for {
      appResponse <- tagHelper.postParkingTagProcess(orgId,
        siteId,
        productionApi.value,
        appRequestCreateTagWithoutTagRequest)
    } yield appResponse

    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid == messageId)

    }
  }

  "Update Tag Without TagId" should "should result Tag Update Failure" in {
    val chain = for {
      appResponse <- tagHelper.updateParkingTagProcess(orgId,
        siteId,
        productionApi.value,
        appRequestUpdateTagRequestWithoutTagId)
    } yield appResponse

    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid == messageId)

    }
  }

  "Update Tag Without Tag Body" should "should result Tag Update Failure" in {
    val chain = for {
      appResponse <- tagHelper.updateParkingTagProcess(orgId,
        siteId,
        productionApi.value,
        appRequestUpdateTagRequestWithoutTagBody)
    } yield appResponse

    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid == messageId)
      assert(res.asInstanceOf[AppFailureResponse].response.status == 404)

    }
  }

  "Update Tag In Right Way" should "update Tag successfully" in {
    doReturn(listOfTags).when(spyDb).getTag(orgId, siteId, tagId, productionApi.value)
    doReturn(updatedTag).when(reqRes).updateTagObject(updateTagRequest, tag)
    doReturn(Future.successful(true))
      .when(spyDb)
      .updateByTagId(tagId, updatedTag, productionApi.value)

    val chain = for {
      appResponse <- tagHelper.updateParkingTagProcess(orgId,
        siteId,
        productionApi.value,
        appRequestUpdateTagTagRequest)
    } yield appResponse

    whenReady(chain, timeout(Span(15, Seconds))) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[TagResponse]].messageid == messageId)
      assert(res.asInstanceOf[AppSuccessResponse[TagResponse]].response.success)

    }
  }

  "Update Tag Failure in db" should "fail to update Tag in db" in {
    doReturn(listOfTags).when(spyDb).getTag(orgId, siteId, tagId, productionApi.value)
    doReturn(updatedTag).when(reqRes).updateTagObject(updateTagRequest, posTag)
    doReturn(futureFailureMessage).when(spyDb).updateByTagId(tagId, updatedTag, productionApi.value)
    val chain = for {
      appResponse <- tagHelper.updateParkingTagProcess(orgId,
        siteId,
        productionApi.value,
        appRequestUpdateTagTagRequest)
    } yield appResponse

    whenReady(chain, timeout(Span(15, Seconds))) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid == messageId)
      assert(res.asInstanceOf[AppFailureResponse].response.status == 500)

    }
  }

  //////////////////////////////////
  "Get What-if Tag by Id Success Response" should "Get What-if by Id Success Response" in {
    doReturn(getTagResponse).when(spyDb).getTag(orgId, siteId, tagId, syntheticApi.value)
    val chain = for {
      appResponse <- tagHelper.getParkingTagProcess(orgId, siteId, syntheticApi.value, appRequestGetTag)
    } yield appResponse

    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[Tag]].messageid.equals(messageId))
    }
  }

  "Get What-if Tag by Id Failure Response" should "Get What-if Tag by Id Failure Response" in {
    doReturn(getTagByIdfailure).when(spyDb).getTag(orgId, siteId, tagId, syntheticApi.value)
    val chain = for {
      appResponse <- tagHelper.getParkingTagProcess(orgId, siteId, syntheticApi.value, appRequestGetTag)
    } yield appResponse

    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId))
    }

  }

  "Get What-if All Tags" should "Get All What-if Tag" in {
    doReturn(tag1).when(spyDb).getAllTag(orgId, siteId, syntheticApi.value)
    val chain = for {
      appResponse <- tagHelper.getAllParkingTagProcess(orgId, siteId, syntheticApi.value, getAllTagAppRequest)
    } yield appResponse

    whenReady(chain) { res =>
      assert(res.messageid.equals(messageId))
      res.response.result.size mustBe 2
    }

  }

  "delete What-if Tag Failure" should "return getTagByIdfailure since Policy Associated with Tag " in {
    doReturn(listOfTags).when(spyDb).getTag(orgId, siteId, tagId, syntheticApi.value)
    doReturn(futureSuccessMessage).when(spyDb).deleteByTagId(tagId, syntheticApi.value,orgId,siteId)
    doReturn(listOfPolicies).when(spyDb).getAllPoliciesByTagid(orgId,siteId,tagId,syntheticApi.value)
    val chain = for {
      appResponse <- tagHelper.deleteParkingTagProcess(orgId, siteId, syntheticApi.value, getAllTagAppRequest)
    } yield appResponse

    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid.equals(messageId))
    }

  }

  "delete What-if Tag Success" should "return success since Policy not Associated with Tag " in {
    doReturn(getTagResponse).when(spyDb).getTag(orgId, siteId, tagId, syntheticApi.value)
    doReturn(futureSuccessMessage).when(spyDb).deleteByTagId(tagId, syntheticApi.value,orgId,siteId)
    doReturn(emptyPoliciesList).when(spyDb).getAllPoliciesByTagid(orgId,siteId,tagId,syntheticApi.value)
    val chain = for {
      appResponse <- tagHelper.deleteParkingTagProcess(orgId, siteId, syntheticApi.value, getAllTagAppRequest)
    } yield appResponse

    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[Tag]].messageid.equals(messageId))
    }

  }

  "post What-if Tag Success" should "Create Tag successful" in {
    doReturn(posTag).when(reqRes).generateTagObject(tagPostRequest, orgId, siteId)
    doReturn(futureSuccessMessage).when(spyDb).storeTag(posTag, syntheticApi.value)

    val chain = for {
      appResponse <- tagHelper.postParkingTagProcess(orgId, siteId, syntheticApi.value, appRequestCreateTag)
    } yield appResponse

    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[TagResponse]].response.result.tagId == tagId)
    }
  }

  "post What-if Tag Failure" should "Create Tag Failure" in {
    doReturn(posTag).when(reqRes).generateTagObject(tagPostRequest, orgId, siteId)
    doReturn(futureFailureMessage).when(spyDb).storeTag(posTag, syntheticApi.value)

    val chain = for {
      appResponse <- tagHelper.postParkingTagProcess(orgId, siteId, syntheticApi.value, appRequestCreateTag)
    } yield appResponse

    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid == messageId)

    }
  }

  "post What-if Tag Without TagRequest Failure" should "should result Tag Failure" in {
    val chain = for {
      appResponse <- tagHelper.postParkingTagProcess(orgId,
        siteId,
        syntheticApi.value,
        appRequestCreateTagWithoutTagRequest)
    } yield appResponse

    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid == messageId)

    }
  }

  "Update What-if Tag Without TagId" should "should result Tag Update Failure" in {
    val chain = for {
      appResponse <- tagHelper.updateParkingTagProcess(orgId,
        siteId,
        syntheticApi.value,
        appRequestUpdateTagRequestWithoutTagId)
    } yield appResponse

    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid == messageId)

    }
  }

  "Update What-if Tag Without Tag Body" should "should result Tag Update Failure" in {
    val chain = for {
      appResponse <- tagHelper.updateParkingTagProcess(orgId,
        siteId,
        syntheticApi.value,
        appRequestUpdateTagRequestWithoutTagBody)
    } yield appResponse

    whenReady(chain) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid == messageId)
      assert(res.asInstanceOf[AppFailureResponse].response.status == 404)

    }
  }

  "Update What-if Tag In Right Way" should "update Tag successfully" in {
    doReturn(listOfTags).when(spyDb).getTag(orgId, siteId, tagId, syntheticApi.value)
    doReturn(updatedTag).when(reqRes).updateTagObject(updateTagRequest, tag)
    doReturn(Future.successful(true))
      .when(spyDb)
      .updateByTagId(tagId, updatedTag, syntheticApi.value)

    val chain = for {
      appResponse <- tagHelper.updateParkingTagProcess(orgId,
        siteId,
        syntheticApi.value,
        appRequestUpdateTagTagRequest)
    } yield appResponse

    whenReady(chain, timeout(Span(15, Seconds))) { res =>
      assert(res.asInstanceOf[AppSuccessResponse[TagResponse]].messageid == messageId)
      assert(res.asInstanceOf[AppSuccessResponse[TagResponse]].response.success)

    }
  }

  "Update What-if Tag Failure in db" should "fail to update Tag in db" in {
    doReturn(listOfTags).when(spyDb).getTag(orgId, siteId, tagId, syntheticApi.value)
    doReturn(updatedTag).when(reqRes).updateTagObject(updateTagRequest, posTag)
    doReturn(futureFailureMessage).when(spyDb).updateByTagId(tagId, updatedTag, syntheticApi.value)
    val chain = for {
      appResponse <- tagHelper.updateParkingTagProcess(orgId,
        siteId,
        syntheticApi.value,
        appRequestUpdateTagTagRequest)
    } yield appResponse

    whenReady(chain, timeout(Span(15, Seconds))) { res =>
      assert(res.asInstanceOf[AppFailureResponse].messageid == messageId)
      assert(res.asInstanceOf[AppFailureResponse].response.status == 500)

    }
  }

}
