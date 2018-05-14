package com.verizon.netsense.helper

import java.util.UUID

import com.verizon.netsense.constants.SensorSampleConstants
import com.verizon.netsense.model.{CoreNodeRawSensorSample, SensorPayload, SensorSampleEvent}

/**
  * Created by nalamte on 2/27/18.
  */
object SensorSampleMsgPackConverter {

  /**
    * This function constructs RestPack model of Sensor Sample from MsgPack Sensor Sample
    * received from Core Node devices
    * Assigns Random UUID to each core node Sensor Sample as they don't have a identifier
    * to each event.
    */
  lazy val msgPackToRestPackConverter: (CoreNodeRawSensorSample) => (SensorSampleEvent) = (a) => {
    SensorSampleEvent(UUID.randomUUID().toString,
      a.nodeid,
      SensorSampleConstants.UNSOL,
      a.sensor,
      s"v1/${a.nodeid}/out/UNSOL/sensor/${a.sensor}",
      SensorPayload(a.nodeid, a.units, a.sensor, a.value, a.time))
  }


}
