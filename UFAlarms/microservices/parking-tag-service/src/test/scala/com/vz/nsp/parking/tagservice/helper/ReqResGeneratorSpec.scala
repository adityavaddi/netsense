package com.vz.nsp.parking.tagservice.helper

import java.util.UUID

import com.vz.nsp.parking.model.{AppRequest, Tag, _}
import com.vz.nsp.parking.tagservice.constants.TestDataFeed._
import com.vz.nsp.parking.tagservice.util.BaseSpec

/**
  * Created by nagarna on 9/28/17.
  */

class ReqResGeneratorSpec extends BaseSpec {

  val reqResGenerator = spy(new ReqResGenerator())

  private implicit val ec = scala.concurrent.ExecutionContext.Implicits.global


  "failurecaseResponse" should "Return Success Message" in {

    val output = reqResGenerator.failurecaseResponse(appRequest, 500, "Error: failurecaseResponse!!!")

    assert(output.isInstanceOf[SuccessMessage].!=(true))

  }

  "generateAppFailureResponse" should "Return Failure Response" in {

    val output = reqResGenerator.generateAppFailureResponse(appRequest, 500, "Error: generateAppFailureResponse!!!")

    assert(output.isInstanceOf[AppFailureResponse].!=(false))

  }

  "failurecaseResponseForIrrelevantRequestType" should "Return Success \"AppFailureResponse\"" in {

    val output = reqResGenerator.failurecaseResponseForIrrelevantRequestType(500, "Error: failurecaseResponseForIrrelevantRequestType!!!")

    assert(output.isInstanceOf[SuccessMessage].!=(true))

  }

  "successResponse" should "Return Success \"AppSuccessResponse\"" in {

    val output = reqResGenerator.successResponse(appRequest, resultBody)

    assert(output.isInstanceOf[SuccessMessage].!=(true))

  }

  "failurecaseResponseWithoutFuture" should "Return Success \"AppSuccessResponse\"" in {

    val output = reqResGenerator.failurecaseResponseWithoutFuture(appRequest, 500, "Error: failurecaseResponseWithoutFuture!!!")

    assert(output.isInstanceOf[AppFailureResponse].!=(false))

  }

  "successResponseWithoutFuture" should "Return Success Response" in {

    val output = reqResGenerator.successResponseWithoutFuture(appRequest, resultBody)

    assert(output.asInstanceOf[AppSuccessResponse[AppRequest]].messageid.equals(messageId))

  }

  "generateTagObject" should "Return Tag Object" in {

    val uid = UUID.randomUUID().toString
    val tagRequest = TagRequest(Some(uid), "name", Some("description"))

    val tagOutput = reqResGenerator.generateTagObject(tagRequest, orgId, siteId)

    assert(tagOutput.asInstanceOf[Tag].siteid.equals(siteId))

  }

  "updateTagObject" should "Return updated Tag Object" in {

    val uid1 = UUID.randomUUID().toString
    val uid2 = UUID.randomUUID().toString

    val tagRequest = TagRequest(Some(uid1), "name", Some("description"))
    val tag = Tag(uid2, "tag_name", "tag_description", orgId, siteId, 1506732699461L, 0, false)

    val tagOutput = reqResGenerator.updateTagObject(tagRequest, tag)

    assert(tagOutput.asInstanceOf[Tag].uid.equals(uid2))

  }

}
