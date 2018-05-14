package com.vz.nsp.datasample.service.db

/**
 * Created by nalamte on 6/20/17.
 */
import com.outworkers.phantom.CassandraTable
import com.outworkers.phantom.dsl._
import com.outworkers.phantom.keys.PartitionKey
import com.verizon.netsense.model.{EnergySavingsNodeModel, EnergySavingsSiteModel, SensorQueryEnvelope}
import com.verizon.netsense.utils.TimeConverter

import scala.concurrent.Future

class EnergySavingsNodeTableTest extends CassandraTable[ConcreteEnergySavingsNodeTest, EnergySavingsNodeModel] {

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

  object starthr extends StringColumn(this)

  object startday extends StringColumn(this)

  object enddt extends StringColumn(this)

  object endtime extends LongColumn(this)

  object ts extends LongColumn(this)

}

abstract class ConcreteEnergySavingsNodeTest extends EnergySavingsNodeTableTest with RootConnector with TimeConverter {

  def storeEnergySavingNode(energySavingsNodeModel: EnergySavingsNodeModel): Future[ResultSet] =
    insert
      .value(_.siteid, energySavingsNodeModel.siteid)
      .value(_.starthr, energySavingsNodeModel.starthr)
      .value(_.enddt, energySavingsNodeModel.enddt)
      .value(_.endtime, energySavingsNodeModel.endtime)
      .value(_.startday, energySavingsNodeModel.startday)
      .value(_.nodeid, energySavingsNodeModel.nodeid)
      .value(_.aggregation_type, energySavingsNodeModel.aggregation_type)
      .value(_.starttime, energySavingsNodeModel.starttime)
      .value(_.startdt, energySavingsNodeModel.startdt)
      .value(_.legacy_energy_consumption, energySavingsNodeModel.legacy_energy_consumption)
      .value(_.led_energy_consumption, energySavingsNodeModel.led_energy_consumption)
      .value(_.actual_energy_consumption, energySavingsNodeModel.actual_energy_consumption)
      .value(_.savings_legacy_vs_led, energySavingsNodeModel.savings_legacy_vs_led)
      .value(_.savings_legacy_vs_actual, energySavingsNodeModel.savings_legacy_vs_actual)
      .value(_.ts, energySavingsNodeModel.ts)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .future()

  def getEnergySavingsNode(
      requestQuery: SensorQueryEnvelope
  ): Future[List[(String, String, String, Double, Double, Double, Double, Double)]] = {

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
      .and(_.starttime gte convertISOToMicros(requestQuery.request.extprops.date1))

    val finalQuery = requestQuery.request.extprops.date2 match {
      case Some(isDefined) =>
        baseQuery
          .and(_.starttime lte convertISOToMicros(requestQuery.request.extprops.date2.get))
          .limit(requestQuery.request.extprops.limit)
          .allowFiltering
      case None =>
        baseQuery
          .limit(requestQuery.request.extprops.limit)
          .allowFiltering
    }

    finalQuery.fetch()
  }
}

class EnergySavingsSiteTableTest extends CassandraTable[ConcreteEnergySavingsSiteTest, EnergySavingsSiteModel] {

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

abstract class ConcreteEnergySavingsSiteTest extends EnergySavingsSiteTableTest with RootConnector with TimeConverter {

  def storeEnergySavingSite(energySavingsSiteModel: EnergySavingsSiteModel): Future[ResultSet] =
    insert
      .value(_.siteid, energySavingsSiteModel.siteid)
      .value(_.aggregation_type, energySavingsSiteModel.aggregation_type)
      .value(_.starttime, energySavingsSiteModel.starttime)
      .value(_.startdt, energySavingsSiteModel.startdt)
      .value(_.actual_energy_consumption, energySavingsSiteModel.actual_energy_consumption)
      .value(_.led_energy_consumption, energySavingsSiteModel.led_energy_consumption)
      .value(_.legacy_energy_consumption, energySavingsSiteModel.legacy_energy_consumption)
      .value(_.savings_legacy_vs_actual, energySavingsSiteModel.savings_legacy_vs_actual)
      .value(_.savings_legacy_vs_led, energySavingsSiteModel.savings_legacy_vs_led)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .future()
  def getEnergySavingsSite(
      requestQuery: SensorQueryEnvelope
  ): Future[List[(String, String, Double, Double, Double, Double, Double)]] = {

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
      .and(_.starttime gte convertISOToMicros(requestQuery.request.extprops.date1))

    val finalQuery = requestQuery.request.extprops.date2 match {
      case Some(isDefined) =>
        baseQuery
          .and(_.starttime lte convertISOToMicros(requestQuery.request.extprops.date2.get))
          .limit(requestQuery.request.extprops.limit)
          .allowFiltering
      case None =>
        baseQuery
          .limit(requestQuery.request.extprops.limit)
          .allowFiltering
    }

    finalQuery.fetch()
  }
}
