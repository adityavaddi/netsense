package com.verizon.netsense.util

import com.verizon.netsense.config.TestSuiteConfig
import com.verizon.netsense.constants.SensorSampleConstants
import com.verizon.netsense.data.TestData
import com.verizon.netsense.model._
import com.verizon.netsense.service.SensorHistoryQueryService

/**
 * Created by nalamte on 6/26/17.
 */
class PredicateCheckSpec extends BaseSpec with TestSuiteConfig with TestData {

  "QueryService" should "do predicate check for SensorSample" in {
    val nodeId                            = "NOTALL"
    val requestQuery: SensorQueryEnvelope = generateQueryWithHeaders(_nodeId = nodeId, sensorId)
    SensorHistoryQueryService.seggregateQueries(requestQuery) mustBe 0
  }

  it should "do predicate Check for Energy Saving Site" in {

    val nodeId                            = SensorSampleConstants.ALL
    val requestQuery: SensorQueryEnvelope = generateQueryWithHeaders(_nodeId = nodeId, _sensorId = energySensorId)
    SensorHistoryQueryService.seggregateQueries(requestQuery) mustBe 2
  }

  it should "do predicate Check for Energy Saving Node" in {
    val nodeId                            = "NOTALL"
    val requestQuery: SensorQueryEnvelope = generateQueryWithHeaders(_nodeId = nodeId, _sensorId = energySensorId)
    SensorHistoryQueryService.seggregateQueries(requestQuery) mustBe 1
  }
  
  it should "send respond with error for request out of scope" in {
    val nodeId                            = null
    val requestQuery: SensorQueryEnvelope = generateQueryWithHeaders(_nodeId = nodeId, _sensorId = energySensorId)
    SensorHistoryQueryService.seggregateQueries(requestQuery) mustBe 3
  }

  it should "send respond with error for request out of scope for non energy" in {
    val nodeId                            = null
    val requestQuery: SensorQueryEnvelope = generateQueryWithHeaders(_nodeId = nodeId, _sensorId = "FailMe!@#$%")
    SensorHistoryQueryService.seggregateQueries(requestQuery) mustBe 3
  }

}
