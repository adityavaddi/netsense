package com.verizon.netsense.businessalertservice.db

import com.outworkers.phantom.dsl.{ConsistencyLevel, _}
import com.outworkers.phantom.keys.PartitionKey
import com.verizon.netsense.businessalertservice.config.CassandraConfig
import com.verizon.netsense.businessalertservice.model.BusinessAlertHistory

import scala.concurrent.Future

/**
 * Business alerts history were written by Sparkjob
 * All fields are made optional to make this service fault-tolerant towards null pointer exceptions from DB
 */
abstract class BusinessAlertHistoryTable extends Table[BusinessAlertHistoryTable, BusinessAlertHistory] {

  override def tableName: String = CassandraConfig.businessAlertHistoryTable

  object orgid extends StringColumn with PartitionKey

  object siteid extends StringColumn with PartitionKey

  object triggerid extends OptionalStringColumn

  object resourceid extends OptionalStringColumn

  object businessalertid extends OptionalStringColumn

  object triggername extends OptionalStringColumn

  object triggercategory extends OptionalStringColumn

  object triggersubcategory extends OptionalStringColumn

  object resourcename extends OptionalStringColumn

  object active extends OptionalBooleanColumn

  object createdon extends LongColumn with ClusteringOrder

  object message extends OptionalStringColumn

  object severity extends OptionalStringColumn

  object lastclearedat extends OptionalLongColumn

  object lastclearedby extends OptionalStringColumn

  object triggeruserid extends OptionalStringColumn

}

abstract class ConcreteBusinessAlertHistory extends BusinessAlertHistoryTable with RootConnector {

  lazy val insertBusinessAlertQuery = insert
    .p_value(_.orgid, ?)
    .p_value(_.siteid, ?)
    .p_value(_.triggerid, ?)
    .p_value(_.resourceid, ?)
    .p_value(_.businessalertid, ?)
    .p_value(_.triggername, ?)
    .p_value(_.triggercategory, ?)
    .p_value(_.triggersubcategory, ?)
    .p_value(_.resourcename, ?)
    .p_value(_.active, ?)
    .p_value(_.createdon, ?)
    .p_value(_.message, ?)
    .p_value(_.severity, ?)
    .p_value(_.lastclearedat, ?)
    .p_value(_.lastclearedby, ?)
    .p_value(_.triggeruserid, ?)
    .consistencyLevel_=(ConsistencyLevel.ONE)
    .prepareAsync()

  def storeBusinessAlertHistory(businessAlert: BusinessAlertHistory): Future[ResultSet] =
    insertBusinessAlertQuery.flatMap(
      _.bind(
        businessAlert.orgId,
        businessAlert.siteId,
        businessAlert.triggerId,
        businessAlert.resourceId,
        businessAlert.businessAlertId,
        businessAlert.triggerName,
        businessAlert.triggerCategory,
        businessAlert.triggerSubcategory,
        businessAlert.resourceName,
        businessAlert.active,
        businessAlert.createdOn,
        businessAlert.message,
        businessAlert.severity,
        businessAlert.lastClearedAt,
        businessAlert.lastClearedBy,
        businessAlert.triggerUserId
      ).future()
    )

}
