package com.vz.nsp.trigger.config


import com.typesafe.config.ConfigFactory

object ServiceConfig {
  lazy val config = ConfigFactory.load()

  lazy val service = config.getConfig("service")
  lazy val parallelism = service.getInt("parallelism")
  lazy val triggerValidation = config.getConfig("validationkeys")
  lazy val timePeriodSet = triggerValidation.getString("timePeriod").split(",").toList.map(_.trim)
  lazy val triggerCategorySet = triggerValidation.getString("triggerCategory").split(",").toList.map(_.trim)
  lazy val triggerSubcategorySet = triggerValidation.getString("triggerSubcategory").split(",").toList.map(_.trim)
  lazy val triggerVariableSet = triggerValidation.getString("triggerVariable").split(",").toList.map(_.trim)
  lazy val comparisonVariableSet = triggerValidation.getString("comparisonVariable").split(",").toList.map(_.trim)
  lazy val comparisonVariableOperatorSet = triggerValidation.getString("comparisonVariableOperator").split(",").toList.map(_.trim)
  lazy val comparisonOperatorSet = triggerValidation.getString("comparisonOperator").split(",").toList.map(_.trim)
  lazy val severitySet = triggerValidation.getString("severity").split(",").toList.map(_.trim)


  /**
    * kafkaRestartableSource
    */
  lazy val kafkaRestartableSource = config.getConfig("kafkaRestartableSource")
  lazy val minBackoffSource = kafkaRestartableSource.getInt("minBackoff")
  lazy val maxBackoffSource = kafkaRestartableSource.getInt("maxBackoff")
  lazy val randomFactorSource = kafkaRestartableSource.getDouble("randomFactor")

}
