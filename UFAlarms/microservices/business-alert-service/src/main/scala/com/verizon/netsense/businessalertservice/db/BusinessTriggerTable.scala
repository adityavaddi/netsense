package com.verizon.netsense.businessalertservice.db

import com.outworkers.phantom.dsl._
import com.outworkers.phantom.keys.PartitionKey
import com.verizon.netsense.businessalertservice.config.CassandraConfig
import com.verizon.netsense.businessalertservice.model.DBTrigger
import com.verizon.netsense.utils.Logging

import scala.concurrent.Future

abstract class BusinessTriggerTable extends Table[BusinessTriggerTable, DBTrigger] {

  override def tableName: String = CassandraConfig.businessTriggerTable

  object triggerid extends StringColumn with PrimaryKey

  object triggername extends StringColumn

  object userid extends StringColumn

  object createdon extends LongColumn

  object lastupdated extends LongColumn

  object triggercategory extends StringColumn

  object triggersubcategory extends StringColumn

  object resourcelistjson extends StringColumn

  object siteid extends StringColumn with PartitionKey

  object orgid extends StringColumn with PartitionKey

  object timeperiod extends StringColumn

  object triggervariable extends StringColumn

  object comparisonvariableoperator extends StringColumn

  object comparisonvariable extends StringColumn

  object comparisonoperator extends StringColumn

  object comparisonvalue extends StringColumn

  object usermessage extends StringColumn

  object additionalusermessage extends StringColumn

  object severity extends StringColumn

  object isdeleted extends BooleanColumn

}

abstract class ConcreteBusinessTrigger extends BusinessTriggerTable with RootConnector with Logging {

  def getAllTriggersInOrgSite(orgid: String, siteid: String): Future[List[DBTrigger]] =
    select
      .where(_.orgid eqs orgid)
      .and(_.siteid eqs siteid)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .fetch()

  def getTriggerById(orgid: String, siteid: String, triggerid: String): Future[Option[DBTrigger]] =
    select
      .where(_.orgid eqs orgid)
      .and(_.siteid eqs siteid)
      .and(_.triggerid eqs triggerid)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .one()

  def storeTrigger(trigger: DBTrigger): Future[ResultSet] =
    insert
      .value(_.triggerid, trigger.triggerid)
      .value(_.triggername, trigger.triggername)
      .value(_.userid, trigger.userid)
      .value(_.createdon, trigger.createdon)
      .value(_.lastupdated, trigger.lastupdated)
      .value(_.triggercategory, trigger.triggercategory)
      .value(_.triggersubcategory, trigger.triggersubcategory)
      .value(_.resourcelistjson, trigger.resourcelistjson)
      .value(_.siteid, trigger.siteid)
      .value(_.orgid, trigger.orgid)
      .value(_.timeperiod, trigger.timeperiod)
      .value(_.triggervariable, trigger.triggervariable)
      .value(_.comparisonvariableoperator, trigger.comparisonvariableoperator)
      .value(_.comparisonvariable, trigger.comparisonvariable)
      .value(_.comparisonoperator, trigger.comparisonoperator)
      .value(_.comparisonvalue, trigger.comparisonvalue)
      .value(_.usermessage, trigger.usermessage)
      .value(_.additionalusermessage, trigger.additionalusermessage)
      .value(_.severity, trigger.severity)
      .value(_.isdeleted, trigger.isdeleted)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .future()

}
