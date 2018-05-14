package com.vz.nsp.parking.config

import com.typesafe.config.ConfigFactory

object ServiceConfig {
  lazy val config = ConfigFactory.load()

  lazy val service        = config.getConfig("service")
  lazy val parallelism    = service.getInt("parallelism")
  lazy val defaultTagName = service.getString("defaultTag")
  lazy val validationConfig = config.getConfig("days.months.occurs")
  lazy val daysConfig = validationConfig.getString("days")
  lazy val monthsConfig = validationConfig.getString("months")
  lazy val occursConfig = validationConfig.getString("occurs")
  lazy val reccurenceConfig = validationConfig.getString("reccurence")
  lazy val unitsConfig = validationConfig.getString("units")
  lazy val violationsConfig = validationConfig.getString("violations")
  lazy val daysSet: Set[String] = validationConfig.getString("days").split(",").toSet
  lazy val monthsSet: Set[String] = validationConfig.getString("months").split(",").toSet
  lazy val occursSet: Set[String] = validationConfig.getString("occurs").split(",").toSet
  lazy val reccurenceSet: Set[String] = validationConfig.getString("reccurence").split(",").toSet
  lazy val unitsSet: Set[String] = validationConfig.getString("units").split(",").toSet
  lazy val violationsSet: Set[String] = validationConfig.getString("violations").split(",").toSet
  lazy val typeOfVehicleConfig = validationConfig.getString("type-of-vehicle")
  lazy val formFactorConfig = validationConfig.getString("form-factor")
  lazy val businessUseConfig = validationConfig.getString("business-use")
  lazy val howMeteredConfig = validationConfig.getString("how-metered")
  lazy val activeConfig = validationConfig.getString("active")
  lazy val areaTypeConfig = validationConfig.getString("area-type")
  lazy val typeOfVehicleSet: Set[String] = validationConfig.getString("type-of-vehicle").split(",").toSet
  lazy val formFactorSet: Set[String] = validationConfig.getString("form-factor").split(",").toSet
  lazy val businessUseSet: Set[String] = validationConfig.getString("business-use").split(",").toSet
  lazy val howMeteredSet: Set[String] = validationConfig.getString("how-metered").split(",").toSet
  lazy val areaTypeSet: Set[String] = validationConfig.getString("area-type").split(",").toSet

  /**
    * kafkaRestartableSource
    */
  lazy val kafkaRestartableSource = config.getConfig("kafkaRestartableSource")
  lazy val minBackoffSource = kafkaRestartableSource.getInt("minBackoff")
  lazy val maxBackoffSource = kafkaRestartableSource.getInt("maxBackoff")
  lazy val randomFactorSource = kafkaRestartableSource.getDouble("randomFactor")

}
