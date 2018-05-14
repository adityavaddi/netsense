package com.vz.nsp.parking.db

import com.outworkers.phantom.dsl._
import com.outworkers.phantom.keys.PartitionKey
import com.vz.nsp.parking.config.CassandraConfig
import com.vz.nsp.parking.model.{Policy, PolicyLevelViolation, PolicyRule}

import scala.concurrent.Future

abstract class WhatIfPolicyTable extends Table[WhatIfPolicyTable, Policy] {
  override def tableName: String = CassandraConfig.whatIfPolicyTableName

  object policyid extends StringColumn with ClusteringOrder

  object version extends IntColumn with ClusteringOrder with Descending

  object userid extends StringColumn

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

abstract class ConcreteWhatIfPolicy extends WhatIfPolicyTable with RootConnector {

  def storeWhatIfPolicy(policy: Policy): Future[ResultSet] =
    insert
      .value(_.policyid, policy.uid)
      .value(_.userid, policy.policyAuthorizerid)
      .value(_.siteid, policy.siteid)
      .value(_.orgid, policy.orgid)
      .value(_.tags, policy.tags)
      .value(_.policyLevelViolations, policy.policyLevelViolations)
      .value(_.islibrarypolicy, policy.isLibraryPolicy)
      .value(_.hashvalue, policy.hashValue)
      .value(_.createdon, policy.createdOn)
      .value(_.lastupdated, policy.lastUpdated)
      .value(_.isdeleted, policy.isDeleted)
      .value(_.version, policy.version)
      .value(_.name, policy.name)
      .value(_.description, policy.description)
      .value(_.timezone, policy.timeZone)
      .value(_.state, policy.state)
      .value(_.policyrule, policy.policyRule)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .future()

  def getWhatIfPolicyByIdAndVersion(uid: String, version: Int, orgid: String, siteid: String): Future[Option[Policy]] =
    select
      .where(_.policyid eqs uid)
      .and(_.orgid eqs orgid)
      .and(_.siteid eqs siteid)
      .and(_.version eqs version)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .one()

  def getAllPolicies(orgid: String, siteid: String): Future[List[Policy]] =
    select
      .where(_.orgid eqs orgid)
      .and(_.siteid eqs siteid)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .fetch()

  def getAllLiveWhatIfPoliciesById(uid: String, orgid: String, siteid: String): Future[List[Policy]] =
    select
      .where(_.policyid eqs uid)
      .and(_.orgid eqs orgid)
      .and(_.siteid eqs siteid)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .fetch()

  def deleteByWhatIfPolicyId(uid: String, version: Int, orgId: String, siteId: String): Future[ResultSet] =
    update
      .where(_.policyid eqs uid)
      .and(_.version eqs version)
      .and(_.orgid eqs orgId)
      .and(_.siteid eqs siteId)
      .modify(_.isdeleted setTo true)
      .ifExists
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .future()

  def updateWhatIfPolicyById(uid: String, version: Int, policy: Policy): Future[ResultSet] =
    insert
      .value(_.policyid, uid)
      .value(_.userid, policy.policyAuthorizerid)
      .value(_.siteid, policy.siteid)
      .value(_.orgid, policy.orgid)
      .value(_.tags, policy.tags)
      .value(_.policyLevelViolations, policy.policyLevelViolations)
      .value(_.islibrarypolicy, policy.isLibraryPolicy)
      .value(_.hashvalue, policy.hashValue)
      .value(_.createdon, policy.createdOn)
      .value(_.lastupdated, policy.lastUpdated)
      .value(_.isdeleted, false)
      .value(_.version, version)
      .value(_.name, policy.name)
      .value(_.description, policy.description)
      .value(_.timezone, policy.timeZone)
      .value(_.state, policy.state)
      .value(_.policyrule, policy.policyRule)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .future()


}
