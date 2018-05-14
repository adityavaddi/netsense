package com.verizon.netsense.helper

import com.verizon.netsense.exceptions.CustomExceptions.EventMissingFieldsException
import com.verizon.netsense.model._
import com.verizon.netsense.utils.Logging

/**
  * Created by nalamte on 8/24/17
  */
object IngestServiceValidator extends Logging with Common {

  object UnappliedIngestionEnvelope {
    def unapply(x: SensorSampleEvent): Some[(String, Long, BigDecimal ,String)] =
      Some(x.sid,
        x.l.t,
        x.l.v,
        x.l.s)
  }

  object UnappliedCoreNodeSensorSample {
    def unapply(x: CoreNodeRawSensorSample): Some[(String, String, Long, BigDecimal)] =
      Some(x.nodeid, x.sensor, x.time, x.value)
  }

  val validateCoreNodeSensorSample: (CoreNodeRawSensorSample) => Boolean = {
    case UnappliedCoreNodeSensorSample(nodeid, sensor, time, value)
      if nodeid != null && !nodeid.isEmpty
        && !time.equals(0) && value != null
        && sensor != null && !sensor.isEmpty => true
    case event@UnappliedCoreNodeSensorSample(nodeid, sensor, time, value)
      if nodeid == null || time.equals(0) || value == null || sensor == null || sensor.isEmpty =>
      throw new EventMissingFieldsException("One or more fields missing from sensor sample" + {event.toJSON}){
        override val underlyingMessage = ""
      }
    case _ => false
  }

  val validationIngestionPredicate: (SensorSampleEvent) => Boolean = {
    case UnappliedIngestionEnvelope(nodeid, time, value, sensortype)
      if nodeid != null && !nodeid.isEmpty && !time.equals(0) && value != null && sensortype != null && !sensortype.isEmpty => true
    case s@UnappliedIngestionEnvelope(nodeid, time, value, sensortype)
      if nodeid == null || time.equals(0) || value == null || sensortype == null || sensortype.isEmpty =>
      throw new EventMissingFieldsException("One or more fields missing from sensor sample" + {s.toJSON}){
        override val underlyingMessage = ""
      }
    case _ => false
  }
}

