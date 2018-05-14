package com.vz.nsp.parking.db

import com.outworkers.phantom.dsl._
import com.outworkers.phantom.keys.PartitionKey
import com.vz.nsp.parking.config.CassandraConfig
import com.vz.nsp.parking.model.{Policy, PolicyLevelViolation, PolicyRule}

import scala.concurrent.Future

abstract class PolicyTable extends Table[PolicyTable, Policy] {
  override def tableName: String = CassandraConfig.parkingPolicyTableName

  object uid extends StringColumn with PartitionKey

  object version extends IntColumn with ClusteringOrder with Descending

  object policyauthorizerid extends StringColumn

  object siteid extends StringColumn

  object orgid extends StringColumn

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
      .value(_.uid, policy.uid)
      .value(_.policyauthorizerid, policy.policyAuthorizerid)
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

  def getPolicyByIdAndVerison(uid: String, version: Int, orgid: String, siteid: String): Future[Option[Policy]] =
    select
      .where(_.uid eqs uid)
      .and(_.orgid is orgid)
      .and(_.siteid is siteid)
      .and(_.version eqs version)
      .and(_.isdeleted is false)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .allowFiltering()
      .one()

  def getPolicyById(uid: String): Future[Option[Policy]] =
    select
      .where(_.uid eqs uid)
      .and(_.isdeleted is false)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .allowFiltering()
      .one()

  def getAllPolicies(orgid: String, siteid: String): Future[List[Policy]] =
    select
      .where(_.orgid is orgid)
      .and(_.siteid is siteid)
      .and(_.isdeleted is false)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .allowFiltering()
      .fetch()

  def getAllPoliciesById(uid: String): Future[List[Policy]] =
    select.where(_.uid eqs uid).consistencyLevel_=(ConsistencyLevel.ONE).allowFiltering().fetch()

  def getAllLivePoliciesById(uid: String, orgid: String, siteid: String): Future[List[Policy]] =
    select
      .where(_.uid eqs uid)
      .and(_.isdeleted is false)
      .and(_.orgid is orgid)
      .and(_.siteid is siteid)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .allowFiltering()
      .fetch()

  def deleteByPolicyId(uid: String, version: Int): Future[ResultSet] =
    update
      .where(_.uid eqs uid)
      .and(_.version eqs version)
      .modify(_.isdeleted setTo true)
      .ifExists
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .future()

  def getMaxVersionById(uid: String): Future[Int] = {
    val v: Future[Option[Option[Int]]] = select
      .function(t => max(t.version))
      .where(_.uid eqs uid)
      .and(_.isdeleted is false)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .allowFiltering()
      .one()
    v.map(x => {
        x.flatten
      })
      .map(v => v.getOrElse(0))
  }

  def updateByPolicyId(uid: String, version: Int, policy: Policy): Future[ResultSet] =
    insert
      .value(_.uid, uid)
      .value(_.policyauthorizerid, policy.policyAuthorizerid)
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

  def getPolicyByTag(tag: String): Future[List[Policy]] =
    select
      .where(_.tags contains tag)
      .and(_.isdeleted is false)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .allowFiltering()
      .fetch()

  def getPolicyByNameWithinOrg(name: String, orgid: String, siteid: String): Future[List[Policy]] =
    select
      .where(_.orgid is orgid)
      .and(_.siteid is siteid)
      .and(_.name is name)
      .and(_.isdeleted is false)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .allowFiltering()
      .fetch()

  def getPolicyByTagidWithinOrg(tagid: String, orgid: String, siteid: String): Future[List[Policy]] =
    select
      .where(_.orgid is orgid)
      .and(_.siteid is siteid)
      .and(_.tags contains tagid)
      .and(_.isdeleted is false)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .allowFiltering()
      .fetch()

  def getVersionHistoryById(uid: String, orgid: String, siteid: String): Future[List[Policy]] =
    select
      .where(_.uid eqs uid)
      .and(_.orgid is orgid)
      .and(_.siteid is siteid)
      .and(_.isdeleted is false)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .allowFiltering()
      .fetch()

  def updateStateOfPolicy(uid: String, version: Int, state: Option[String]): Future[ResultSet] =
    update
      .where(_.uid eqs uid)
      .and(_.version eqs version)
      .modify(_.state setTo state)
      .ifExists
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .future()

  def checkStateOfPolicy(uid: String, version: Int, state: Option[String]): Future[Option[Policy]] =
    select
      .where(_.uid eqs uid)
      .and(_.version eqs version)
      .and(_.state is state)
      .and(_.isdeleted is false)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .allowFiltering()
      .one()
}
