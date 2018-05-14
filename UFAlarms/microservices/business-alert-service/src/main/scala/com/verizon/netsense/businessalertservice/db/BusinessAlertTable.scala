package com.verizon.netsense.businessalertservice.db

import com.outworkers.phantom.dsl.{ConsistencyLevel, _}
import com.outworkers.phantom.keys.PartitionKey
import com.verizon.netsense.businessalertservice.config.CassandraConfig
import com.verizon.netsense.businessalertservice.model.BusinessAlert
import scala.concurrent.Future

/**
 * Business alerts were written by Sparkjob
 * All fields are made optional to make this service fault-tolerant towards null pointer exceptions from DB
 */
abstract class BusinessAlertTable extends Table[BusinessAlertTable, BusinessAlert] {

  override def tableName: String = CassandraConfig.businessAlertTable

  object orgid extends StringColumn with PartitionKey

  object siteid extends StringColumn with PartitionKey

  object triggerid extends OptionalStringColumn

  object resourceid extends OptionalStringColumn

  object businessalertid extends StringColumn with ClusteringOrder

  object triggername extends OptionalStringColumn

  object triggercategory extends OptionalStringColumn

  object triggersubcategory extends OptionalStringColumn

  object resourcename extends OptionalStringColumn

  object active extends OptionalBooleanColumn

  object createdon extends OptionalLongColumn

  object lastupdated extends OptionalLongColumn

  object message extends OptionalStringColumn

  object severity extends OptionalStringColumn

  object lastclearedat extends OptionalLongColumn

  object lastclearedby extends OptionalStringColumn

  object triggeruserid extends OptionalStringColumn

}

abstract class ConcreteBusinessAlert extends BusinessAlertTable with RootConnector {

  def getBusinessAlertById(orgid: String, siteid: String, businessAlertId: String): Future[Option[BusinessAlert]] =
    select
      .where(_.orgid eqs orgid)
      .and(_.siteid eqs siteid)
      .and(_.businessalertid eqs businessAlertId)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .one()

  def getAllBusinessAlerts(orgid: String, siteid: String): Future[List[BusinessAlert]] =
    select
      .where(_.orgid eqs orgid)
      .and(_.siteid eqs siteid)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .fetch()

  def dismissByBusinessAlertId(orgid: String,
                               siteid: String,
                               businessAlertId: String,
                               severity: String,
                               userId: String,
                               lastClearedAt: Long): Future[ResultSet] =
    update
      .where(_.orgid eqs orgid)
      .and(_.siteid eqs siteid)
      .and(_.businessalertid eqs businessAlertId)
      .modify(_.severity setTo Some(severity))
      .and(_.lastclearedby setTo Some(userId))
      .and(_.lastclearedat setTo Some(lastClearedAt))
      .and(_.lastupdated setTo Some(lastClearedAt))
      .ifExists
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .future()

  lazy val insertBusinessAlert = insert
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
    .p_value(_.lastupdated, ?)
    .consistencyLevel_=(ConsistencyLevel.ONE)
    .prepareAsync()

  def storeBusinessAlert(businessAlert: BusinessAlert): Future[ResultSet] =
    insertBusinessAlert.flatMap(
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
        businessAlert.triggerUserId,
        businessAlert.lastUpdated
      ).future()
    )
}
