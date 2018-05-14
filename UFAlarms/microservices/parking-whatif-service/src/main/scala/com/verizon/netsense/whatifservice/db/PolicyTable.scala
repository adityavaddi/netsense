package com.verizon.netsense.whatifservice.db

import com.outworkers.phantom.dsl._
import com.outworkers.phantom.keys.PartitionKey
import com.verizon.netsense.whatifservice.config.WhatIfConfigLoader
import com.verizon.netsense.whatifservice.model.{Policy, PolicyLevelViolation, PolicyRule}

import scala.concurrent.Future

/**
 * Created by maleva on 4/11/18.
 */
abstract class PolicyTable extends Table[PolicyTable, Policy] {
  override def tableName: String = WhatIfConfigLoader.whatIfParkingPolicyTableName

  object userid extends StringColumn with PartitionKey

  object version extends IntColumn with ClusteringOrder

  object policyid extends StringColumn with ClusteringOrder

  object siteid extends StringColumn with PartitionKey

  object orgid extends StringColumn with PartitionKey

  object tags extends SetColumn[String]

  object policyLevelViolations extends SetColumn[PolicyLevelViolation]

  object islibrarypolicy extends BooleanColumn

  object hashvalue extends StringColumn

  object createdon extends LongColumn

  object lastupdated extends LongColumn

  object isdeleted extends BooleanColumn

  object name extends StringColumn

  object description extends OptionalCol[String]

  object timezone extends StringColumn

  object state extends OptionalCol[String]

  object policyrule extends SetColumn[PolicyRule]

}

abstract class ConcretePolicy extends PolicyTable with RootConnector {

  def storePolicy(policy: Policy): Future[ResultSet] =
    insert
      .value(_.userid, policy.userid)
      .value(_.policyid, policy.policyid)
      .value(_.siteid, policy.siteid)
      .value(_.orgid, policy.orgid)
      .value(_.tags, policy.tags)
      .value(_.policyLevelViolations, policy.policylevelviolations)
      .value(_.islibrarypolicy, policy.islibrarypolicy)
      .value(_.hashvalue, policy.hashvalue)
      .value(_.createdon, policy.createdon)
      .value(_.lastupdated, policy.lastupdated)
      .value(_.isdeleted, policy.isdeleted)
      .value(_.version, policy.version)
      .value(_.name, policy.name)
      .value(_.description, policy.description)
      .value(_.timezone, policy.timezone)
      .value(_.state, policy.state)
      .value(_.policyrule, policy.policyrule)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .future()

  def getAllPolicies(orgid: String, siteid: String): Future[List[Policy]] =
    select
      .where(_.orgid eqs orgid)
      .and(_.siteid eqs siteid)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .allowFiltering()
      .fetch()
}
