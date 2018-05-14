package com.verizon.netsense.db

import com.outworkers.phantom.dsl._
import com.outworkers.phantom.keys.PartitionKey
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.model._
import com.verizon.netsense.utils.TimeConverter
import nl.grons.metrics.scala.Timer
import org.apache.cassandra.utils.UUIDGen

import scala.concurrent.Future

/**
  * Created by maidapr on 4/7/17.
  */
class DeviceEventTable extends CassandraTable[ConcreteDeviceEvent, SensorPayload] {

  override def tableName: String = "device_sensor_samples"

  object nodeid extends StringColumn(this) with PartitionKey

  object sensor extends StringColumn(this) with ClusteringOrder with Ascending

  object time extends LongColumn(this) with ClusteringOrder with Ascending

  object date extends TimeUUIDColumn(this)

  object value extends DoubleColumn(this)

  override def fromRow(r: Row): SensorPayload = SensorPayload(n = nodeid(r), s = sensor(r), v = value(r), t = time(r))

}

abstract class ConcreteDeviceEvent extends DeviceEventTable with RootConnector with TimeConverter with Instrumented {

  private[this] val getsensorHistoryFromTotimer: Timer = metrics.timer("cassandra-getsensorHistoryFromTo-timer")
  private[this] val getsensorHistoryFromtimer: Timer = metrics.timer("cassandra-getsensorHistoryFrom-timer")
  private[this] val getsensorDriverLevelFromTotimer: Timer = metrics.timer("cassandra-getsensorDriverLevelFromTo-timer")
  private[this] val running: Timer = metrics.timer("cassandra-async-persist-timer")

  def store(sensorEvent: SensorSampleEvent): Future[ResultSet] = {
    running.time (
    insert
      .value(_.nodeid, sensorEvent.sid)
      .value(_.date, UUIDGen.getTimeUUID(System.currentTimeMillis()))
      .value(_.sensor, sensorEvent.l.s)
      .value(_.time, sensorEvent.l.t)
      .value(_.value, sensorEvent.l.v.toDouble)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .future()
    )
  }

  def getHistoricalDataForSensor(requestQuery: SensorQueryEnvelope): Future[List[(Long, Double)]] = {

    val date = requestQuery.request.`type` match {
      case requestQuery.request.`type` if requestQuery.request.`type`.startsWith("getSensorHistoryFromTo") =>
        requestQuery.request.extprops.date1
      case requestQuery.request.`type` if requestQuery.request.`type`.startsWith("getSensorHistory") =>
        requestQuery.request.extprops.date
      case _ => requestQuery.request.extprops.date1
    }

    val baseQuery = select(_.time, _.value)
      .where(_.nodeid eqs requestQuery.request.nodeprops.nodeid)
      .and(_.sensor eqs requestQuery.request.extprops.sensorid)
      .and(_.time gte convertISOToMicros(date))
      .orderBy(_.sensor desc, _.time desc)

    val finalQuery = requestQuery.request.extprops.date2 match {
      case Some(date2) =>
        getsensorHistoryFromTotimer
          .time(baseQuery.and(_.time lte convertISOToMicros(date2))
          .limit(requestQuery.request.extprops.limit)
          .consistencyLevel_=(ConsistencyLevel.ONE))

      case None =>
        getsensorHistoryFromtimer
          .time(baseQuery
            .limit(requestQuery.request.extprops.limit)
            .consistencyLevel_=(ConsistencyLevel.ONE))
    }

    finalQuery.fetch()
  }

  def getSensorHistoryData(nodeId: String, sensorId: String, fromTime: Long, toTime: Long, limit: Int)
  : Future[List[(Long, Double)]] = {
    val query =
      getsensorDriverLevelFromTotimer.time(
      select(_.time, _.value)
      .where(_.nodeid eqs nodeId)
      .and(_.sensor eqs sensorId)
      .and(_.time gte fromTime)
      .and(_.time lte toTime)
      .orderBy(_.sensor desc, _.time desc)
      .limit(limit)
      .consistencyLevel_=(ConsistencyLevel.ONE))
    query.fetch()
  }
}

class EnergySavingsNodeTable extends CassandraTable[ConcreteEnergySavingsNode, EnergySavingsNodeModel] {

  override def tableName: String = "aggregation_energy_savings_node"

  object siteid extends StringColumn(this) with PartitionKey

  object nodeid extends StringColumn(this) with PartitionKey

  object aggregation_type extends StringColumn(this) with PartitionKey

  object starttime extends LongColumn(this) with ClusteringOrder with Ascending

  object actual_energy_consumption extends DoubleColumn(this)

  object led_energy_consumption extends DoubleColumn(this)

  object legacy_energy_consumption extends DoubleColumn(this)

  object savings_legacy_vs_actual extends DoubleColumn(this)

  object savings_legacy_vs_led extends DoubleColumn(this)

  object startdt extends StringColumn(this)

}

abstract class ConcreteEnergySavingsNode extends EnergySavingsNodeTable with RootConnector with TimeConverter with Instrumented {


  private[this] val getsensorEnergyNodeFromTotimer: Timer = metrics.timer("cassandra-getsensorEnergyNodeFromTotimer-timer")
  private[this] val getsensorEnergyNodeFromtimer: Timer = metrics.timer("cassandra-getsensorEnergyNodeFromtimer-timer")


  def getEnergySavingsNode(
                            requestQuery: SensorQueryEnvelope
                          ): Future[List[(String, String, String, Double, Double, Double, Double, Double)]] = {

    val date = requestQuery.request.`type` match {
      case requestQuery.request.`type` if requestQuery.request.`type`.startsWith("getSensorHistoryFromTo") =>
        requestQuery.request.extprops.date1
      case requestQuery.request.`type` if requestQuery.request.`type`.startsWith("getSensorHistory") =>
        requestQuery.request.extprops.date
      case _ => requestQuery.request.extprops.date1
    }

    val baseQuery = select(
      _.siteid,
      _.nodeid,
      _.startdt,
      _.legacy_energy_consumption,
      _.led_energy_consumption,
      _.actual_energy_consumption,
      _.savings_legacy_vs_led,
      _.savings_legacy_vs_actual
    ).where(_.siteid eqs requestQuery.request.siteprops.siteid)
      .and(_.nodeid eqs requestQuery.request.nodeprops.nodeid)
      .and(_.aggregation_type eqs requestQuery.request.extprops.period.getOrElse("15min"))
      .and(_.starttime gte convertISOToMicros(date))
      .orderBy(_.starttime desc)

    val finalQuery = requestQuery.request.extprops.date2 match {
      case Some(isDefined) =>
        getsensorEnergyNodeFromTotimer.time(
          baseQuery
            .and(_.starttime lte convertISOToMicros(requestQuery.request.extprops.date2.get))
            .limit(requestQuery.request.extprops.limit)
            .consistencyLevel_=(ConsistencyLevel.ONE))
      case None =>
        getsensorEnergyNodeFromtimer.time(
          baseQuery
            .limit(requestQuery.request.extprops.limit)
            .consistencyLevel_=(ConsistencyLevel.ONE))
    }

    finalQuery.fetch()
  }
}

class EnergySavingsSiteTable extends CassandraTable[ConcreteEnergySavingsSite, EnergySavingsSiteModel] {

  override def tableName: String = "aggregation_energy_savings_site"

  object siteid extends StringColumn(this) with PartitionKey

  object aggregation_type extends StringColumn(this) with PartitionKey

  object starttime extends LongColumn(this) with ClusteringOrder with Ascending

  object actual_energy_consumption extends DoubleColumn(this)

  object led_energy_consumption extends DoubleColumn(this)

  object legacy_energy_consumption extends DoubleColumn(this)

  object savings_legacy_vs_actual extends DoubleColumn(this)

  object savings_legacy_vs_led extends DoubleColumn(this)

  object startdt extends StringColumn(this)

}

abstract class ConcreteEnergySavingsSite extends EnergySavingsSiteTable with RootConnector with TimeConverter with Instrumented {


  private[this] val getsensorEnergySiteFromTotimer: Timer = metrics.timer("cassandra-getsensorEnergySiteFromTotimer-timer")
  private[this] val getsensorEnergySiteFromtimer: Timer = metrics.timer("cassandra-getsensorEnergySiteFromtimer-timer")


  def getEnergySavingsSite(
                            requestQuery: SensorQueryEnvelope
                          ): Future[List[(String, String, Double, Double, Double, Double, Double)]] = {

    val date = requestQuery.request.`type` match {
      case requestQuery.request.`type` if requestQuery.request.`type`.startsWith("getSensorHistoryFromTo") =>
        requestQuery.request.extprops.date1
      case requestQuery.request.`type` if requestQuery.request.`type`.startsWith("getSensorHistory") =>
        requestQuery.request.extprops.date
      case _ => requestQuery.request.extprops.date1
    }

    val baseQuery = select(
      _.siteid,
      _.startdt,
      _.legacy_energy_consumption,
      _.led_energy_consumption,
      _.actual_energy_consumption,
      _.savings_legacy_vs_led,
      _.savings_legacy_vs_actual
    ).where(_.siteid eqs requestQuery.request.siteprops.siteid)
      .and(_.aggregation_type eqs requestQuery.request.extprops.period.getOrElse("15min"))
      .and(_.starttime gte convertISOToMicros(date))
      .orderBy(_.starttime desc)

    val finalQuery = requestQuery.request.extprops.date2 match {
      case Some(isDefined) =>
        getsensorEnergySiteFromTotimer.time(
          baseQuery
            .and(_.starttime lte convertISOToMicros(requestQuery.request.extprops.date2.get))
            .limit(requestQuery.request.extprops.limit)
            .consistencyLevel_=(ConsistencyLevel.ONE))
      case None =>
        getsensorEnergySiteFromtimer.time(
          baseQuery
            .limit(requestQuery.request.extprops.limit)
            .consistencyLevel_=(ConsistencyLevel.ONE))
    }

    finalQuery.fetch()
  }
}

class LightTable extends CassandraTable[ConcreteLightTable, Light] {

  override def tableName: String = "light"

  object nodeid           extends StringColumn(this) with PartitionKey
  object driver           extends OptionalIntColumn(this)
  object harvest_trigger  extends OptionalBooleanColumn(this)
  object isscheduled      extends OptionalBooleanColumn(this)
  object policy           extends OptionalStringColumn(this)
  object priority         extends OptionalIntColumn(this)
  object startdt          extends OptionalDateTimeColumn(this)

  override def fromRow(r: Row): Light =
    Light(nodeid(r), driver(r), harvest_trigger(r), isscheduled(r), policy(r), priority(r), startdt(r))

}

abstract class ConcreteLightTable extends LightTable with RootConnector with Instrumented {

  private[this] val getLightModeUpdatetimer: Timer = metrics.timer("cassandra-get-light-timer")
  private[this] val storeLightModeUpdatetimer: Timer = metrics.timer("cassandra-store-light-timer")

  def getLatestLightModeByNodeId(nodeId: String): Future[Option[Light]] =
    getLightModeUpdatetimer.time(
      select
        .where(_.nodeid eqs nodeId)
        .consistencyLevel_=(ConsistencyLevel.ONE)
        .one())

  def storeLatestLightModeByNodeId(light: Light): Future[ResultSet] =
    storeLightModeUpdatetimer.time(insert()
      .value(_.nodeid, light.nodeid)
      .value(_.driver, light.driver)
      .value(_.harvest_trigger, light.harvest_trigger)
      .value(_.isscheduled, light.isscheduled)
      .value(_.policy, light.policy)
      .value(_.priority, light.priority)
      .value(_.startdt, light.startdt)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .future())

}

class NodeStatusModel extends CassandraTable[ConcreteNodeStatusModel, NodeStatus] {

  override def tableName: String = "node_status"

  object nodeid   extends StringColumn(this) with PartitionKey

  object orgid    extends OptionalStringColumn(this)
  object siteid   extends OptionalStringColumn(this)
  object net_stat extends OptionalIntColumn(this)
  object lig_stat extends OptionalStringColumn(this)
  object sen_stat extends OptionalStringColumn(this)

  override def fromRow(r: Row): NodeStatus =
    NodeStatus("NodeStatus", nodeid(r), orgid(r), siteid(r), net_stat(r), lig_stat(r), sen_stat(r))
}

abstract class ConcreteNodeStatusModel extends NodeStatusModel with RootConnector with Instrumented {

  private[this] val cassandraGetNodeStatusTimer: Timer = metrics.timer("cassandra-get-nodestatus-timer")
  private[this] val cassandraStoreNodeStatusTimer: Timer = metrics.timer("cassandra-store-nodestatus-timer")

  def getNodeStatusbyNodeId(nodeId: String): Future[Option[NodeStatus]] = {
    cassandraGetNodeStatusTimer.time (
     select
        .where(_.nodeid eqs nodeId)
        .consistencyLevel_=(ConsistencyLevel.ONE)
        .one()
    )
  }

  def storeNodeStatus(nodeStatus: NodeStatus): Future[ResultSet] = {
    cassandraStoreNodeStatusTimer.time (
      insert
        .value(_.nodeid, nodeStatus.nodeId)
        .value(_.orgid, nodeStatus.orgId)
        .value(_.siteid, nodeStatus.siteId)
        .value(_.net_stat, nodeStatus.netStat)
        .value(_.lig_stat, nodeStatus.ligStat)
        .value(_.sen_stat, nodeStatus.senStat)
        .consistencyLevel_=(ConsistencyLevel.ONE)
        .future()
    )
  }

}
