package com.verizon.netsense.services.eventsimulator.database

import com.outworkers.phantom.dsl._
import com.outworkers.phantom.keys.PartitionKey
import com.verizon.netsense.services.eventsimulator.model.SensorSample

import scala.concurrent.Future

abstract class Device_Sensor_samples extends CassandraTable[Device_Sensor_samples, SensorSample] with RootConnector {

  def truncateTable(): Future[ResultSet] =
    truncate().future()

  object nodeid extends StringColumn(this) with PartitionKey

  object sensor extends StringColumn(this) with ClusteringOrder with Ascending

  object time extends LongColumn(this) with ClusteringOrder with Ascending

  object date extends TimeUUIDColumn(this)

  object value extends DoubleColumn(this)
}
