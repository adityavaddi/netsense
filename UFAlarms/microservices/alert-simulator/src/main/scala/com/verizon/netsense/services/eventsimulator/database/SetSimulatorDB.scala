package com.verizon.netsense.services.eventsimulator.database

import com.outworkers.phantom.connectors.CassandraConnection
import com.outworkers.phantom.dsl._
import com.verizon.netsense.connector.CassandraConnector

class SimulatorDB(val connection: CassandraConnection) extends Database[SimulatorDB](connection) {
  object DeviceAlerts  extends Alerts with Connector
  object DeviceAlarms  extends Device_Alarms with Connector
  object Notifications extends Notifications with Connector
  object OrgHierarchy  extends Orghierarchy_By_Nodeid with Connector
  object SensorSamples extends Device_Sensor_samples with Connector
}

object SimulatorDB extends SimulatorDB(CassandraConnector.connector)
