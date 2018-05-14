package com.verizon.netsense.database

import com.outworkers.phantom.CassandraTable
import com.outworkers.phantom.dsl._
import com.outworkers.phantom.keys.PartitionKey
import com.verizon.netsense.model.Light

import scala.concurrent.Future

/**
  * Created by maidapr on 1/10/18.
  */
class LightTable extends CassandraTable[ConcreteLightTable, Light] {

  override def tableName: String = "light"

  object nodeid extends StringColumn(this) with PartitionKey

  object driver extends OptionalIntColumn(this)

  object harvest_trigger extends BooleanColumn(this)

  object isscheduled extends BooleanColumn(this)

  object policy extends StringColumn(this)

  object priority extends OptionalIntColumn(this)

  object schedule_id extends OptionalStringColumn(this)

  object schedule_time extends OptionalLongColumn(this)

  object startdt extends OptionalDateTimeColumn(this)

  override def fromRow(r: Row): Light = Light(nodeid(r), driver(r), harvest_trigger(r), isscheduled(r), policy(r), priority(r), schedule_id(r), schedule_time(r), startdt(r))

}

abstract class ConcreteLightTable extends LightTable with RootConnector {

  def getLatestLightModeByNodeId(nodeId: String): Future[Option[Light]] =
    select
    .where(_.nodeid eqs nodeId)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .one()

  def storeLatestLightModeByNodeId(light: Light): Future[ResultSet] =
    insert()
        .value(_.nodeid, light.nodeid)
        .value(_.driver, light.driver)
        .value(_.harvest_trigger, light.harvest_trigger)
        .value(_.isscheduled, light.isscheduled)
        .value(_.policy, light.policy)
        .value(_.priority, light.priority)
        .value(_.schedule_id, light.schedule_id)
        .value(_.schedule_time, light.schedule_time)
        .value(_.startdt, light.startdt)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .future()

}
