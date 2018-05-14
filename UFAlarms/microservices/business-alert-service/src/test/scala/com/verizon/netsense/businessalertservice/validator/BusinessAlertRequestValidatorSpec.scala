package com.verizon.netsense.businessalertservice.validator

import com.verizon.netsense.businessalertservice.data.TestDataFeed._
import com.verizon.netsense.businessalertservice.db.EmbeddedDatabase
import com.verizon.netsense.businessalertservice.flow.KafkaConnector
import com.verizon.netsense.businessalertservice.model.{BusinessAlertProps, OrgProps}
import com.verizon.netsense.businessalertservice.util.BaseSpec

import scala.language.reflectiveCalls

/**
 * Created by jittara on 02/28/18.
  **/
class BusinessAlertRequestValidatorSpec extends BaseSpec with EmbeddedDatabase {

  val kafkaConnector                = new KafkaConnector()
  val businessAlertRequestValidator = new BusinessAlertRequestValidator(kafkaConnector)

  "IngestionServiceSpec" should "Check if time , value and sensorType are valid" in {
    businessAlertRequestValidator.validationPredicate(generateAppRequest("responseTopic", "getBusinessAlert")) mustBe true
  }

  "IngestionServiceSpec" should "Check if it is Error log is thrown if the  any of the " +
  "required feilds are missing in ingest service" in {
    businessAlertRequestValidator.validationPredicate(generateAppRequestSearch("responseTopic", "invalidType")) mustBe false
  }

  it should "alertSys - Positive" in {
    businessAlertRequestValidator.validationPredicate(generateAppRequestForSys(businessAlertId)) mustBe true
  }

  it should "alertSys - Negative(wrong user)" in {
    businessAlertRequestValidator.validationPredicate(generateAppRequestForSys(businessAlertId, userid = "notRoot")) mustBe false
  }

  it should "alertSys - Negative(Orgprops missing)" in {
    val appRequest = generateAppRequestForSys(businessAlertId)
    businessAlertRequestValidator.validationPredicate(
      appRequest.copy(request = appRequest.request.copy(orgprops = null))
    ) mustBe false
  }

  it should "alertSys - Negative(orgid missing)" in {
    val appRequest = generateAppRequestForSys(businessAlertId)
    businessAlertRequestValidator.validationPredicate(
      appRequest.copy(request = appRequest.request.copy(orgprops = OrgProps(null)))
    ) mustBe false
  }

  it should "alertSys - Negative(businessalertid missing)" in {
    val appRequest = generateAppRequestForSys(businessAlertId)
    businessAlertRequestValidator.validationPredicate(
      appRequest.copy(request = appRequest.request.copy(businessalertprops = Some(BusinessAlertProps(None, None))))
    ) mustBe false
  }

}
