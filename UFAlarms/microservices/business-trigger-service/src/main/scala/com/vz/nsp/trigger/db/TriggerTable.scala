

package com.vz.nsp.trigger.db

/**
  * Created by maleva on 2/13/18.
  */

import com.outworkers.phantom.dsl._
import com.outworkers.phantom.keys.PartitionKey
import com.vz.nsp.trigger.config.CassandraConfig
import com.vz.nsp.trigger.model._
import com.verizon.netsense.utils.Logging
import com.vz.nsp.trigger.model.db.DBTrigger

import scala.concurrent.Future

abstract class TriggerTable extends Table[TriggerTable, DBTrigger] {

  override def tableName: String = CassandraConfig.TriggerTableName

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

abstract class ConcreteTrigger extends TriggerTable with RootConnector with Logging {

  def store(trigger: DBTrigger): Future[ResultSet] =
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


  def getTriggerById(triggerid: String, siteid: String, orgid: String): Future[Option[DBTrigger]] =
    select.where(_.orgid eqs orgid)
      .and(_.siteid eqs siteid)
      .and(_.triggerid eqs triggerid)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .one()


  def getAllTriggerId(orgid: String, siteid: String): Future[List[DBTrigger]] =
    select
      .where(_.orgid eqs orgid)
      .and(_.siteid eqs siteid)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .fetch()

  def deleteByTriggerId(triggerid: String, siteid: String, orgid: String): Future[ResultSet] =
    update
      .where(_.orgid eqs orgid)
      .and(_.siteid eqs siteid)
      .and(_.triggerid eqs triggerid)
      .modify(_.isdeleted setTo true)
      .ifExists
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .future()

  def updateByTriggerId(triggerid: String, siteid: String, orgid: String, trigger: DBTrigger): Future[ResultSet] =
    update
      .where(_.triggerid eqs triggerid)
      .and(_.siteid eqs siteid)
      .and(_.orgid eqs orgid)
      .modify(_.triggername setTo trigger.triggername)
      .and(_.userid setTo trigger.userid)
      .and(_.createdon setTo trigger.createdon)
      .and(_.triggercategory setTo trigger.triggercategory)
      .and(_.triggersubcategory setTo trigger.triggersubcategory)
      .and(_.resourcelistjson setTo trigger.resourcelistjson)
      .and(_.timeperiod setTo trigger.timeperiod)
      .and(_.triggervariable setTo trigger.triggervariable)
      .and(_.comparisonvariableoperator setTo trigger.comparisonvariableoperator)
      .and(_.comparisonvariable setTo trigger.comparisonvariable)
      .and(_.comparisonoperator setTo trigger.comparisonoperator)
      .and(_.lastupdated setTo trigger.lastupdated)
      .and(_.comparisonvalue setTo trigger.comparisonvalue)
      .and(_.usermessage setTo trigger.usermessage)
      .and(_.additionalusermessage setTo trigger.additionalusermessage)
      .and(_.severity setTo trigger.severity)
      .and(_.isdeleted setTo trigger.isdeleted)

      .ifExists
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .future()
}












