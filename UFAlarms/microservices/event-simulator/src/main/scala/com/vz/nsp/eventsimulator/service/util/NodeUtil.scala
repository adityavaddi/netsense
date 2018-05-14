package com.vz.nsp.eventsimulator.service.util

import com.vz.nsp.eventsimulator.service.config.ConfigLoader

/**
 * Created by jittara on 4/20/17.
 * This object is to get the configuration Arrays
 */
object NodeUtil {

  /**
   *  Sensors Array
   */
  lazy val sensorsArray1 = ConfigLoader.sensorsArray.toArray

  /**
   *  nodes Ids Array
   */
  lazy val nodesArray = ConfigLoader.nodeIds

  /**
   *  names Array
   */
  lazy val namesArray = ConfigLoader.nodeNames.toArray

  /**
   *  ActionTypes Array
   */
  lazy val actionTypesArray = ConfigLoader.actionTypes.toArray

  /**
   *  AlarmSeveritys  Array
   */
  lazy val alarmSeverityArray = ConfigLoader.alarmSeverity.toArray

  /**
   *  Alaram Types Array
   */
  lazy val alarmaTypesArray = ConfigLoader.alarmType.toArray

  /**
   *  login Auth Types Array
   */
  lazy val loginAuthArray = ConfigLoader.alarmType.toArray
}
