package com.verizon.netsense.service

import com.verizon.netsense.config.TestSuiteConfig
import com.verizon.netsense.connector.CassandraConnector
import com.verizon.netsense.data.TestData
import com.verizon.netsense.exceptions.CustomExceptions.{EventMissingFieldsException, MessageUnMarshallingException}
import com.verizon.netsense.helper.{IngestServiceValidator, QueryServiceValidator}
import com.verizon.netsense.model.CoreNodeRawSensorSample
import com.vz.nsp.datasample.service.db.{DatabaseSpec, EmbeddedDatabase}

import scala.language.reflectiveCalls

/**
  * Created by nalamte on 9/1/17.
  **/

class IngestValidationFlowSpec extends TestData
                               with DatabaseSpec
                               with TestSuiteConfig
                               with EmbeddedDatabase
                               with CassandraConnector.testConnector.Connector{


  "IngestionServiceSpec" should "Check if time, value and sensorType are valid" in {
    IngestServiceValidator.validationIngestionPredicate(sensorSampleEventDecoded("SensorId", 12.12)) mustBe true
  }
  it should "Check Error log is thrown if the any of the required fields are missing in ingest service" in {
    an[EventMissingFieldsException] must be thrownBy IngestServiceValidator.validationIngestionPredicate(sensorSampleEventDecoded(null, 0))
  }
  it should "Check if Error log is thrown if the sensorId is missing in ingest service" in {
    an[EventMissingFieldsException] must be thrownBy IngestServiceValidator.validationIngestionPredicate(sensorSampleEventDecoded("", 0))
  }

  "CoreNodeSensorSample Validation Spec" should "Check if nodeId and sensor type are  valid" in {
    IngestServiceValidator.validateCoreNodeSensorSample(generateCoreNodeSensorSample("N07246181", "p", System.currentTimeMillis(), 87.9 )) mustBe true
  }
  it should "Check Error log is thrown if the any of the required fields are missing in ingest service for core node sensor sample" in {
    an[EventMissingFieldsException] must be thrownBy IngestServiceValidator.validateCoreNodeSensorSample(generateCoreNodeSensorSample("", "", System.currentTimeMillis(), 87.9 ))
  }
  it should "Check if Error log is thrown if the sensorId is missing in ingest service" in {
    an[EventMissingFieldsException] must be thrownBy IngestServiceValidator
      .validateCoreNodeSensorSample(generateCoreNodeSensorSample("", "", System.currentTimeMillis(), 90.0 ))
  }
}
