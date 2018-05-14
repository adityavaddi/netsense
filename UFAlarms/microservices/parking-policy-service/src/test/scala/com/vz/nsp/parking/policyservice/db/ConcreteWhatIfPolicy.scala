package com.vz.nsp.parking.policyservice.db

import com.outworkers.phantom.dsl._
import com.outworkers.phantom.keys.PartitionKey
import com.vz.nsp.parking.config.CassandraConfig
import com.vz.nsp.parking.model._
import com.vz.nsp.parking.policyservice.db.PolicyConnector._

import scala.concurrent.Future

abstract class ConcreteWhatIfPolicy extends Table[ConcreteWhatIfPolicy, Policy] {

  def truncateWhatIfPolicy: Future[ResultSet] = truncate().future()


  def WhatIfPolicyCustomTypes() = {
    customTypesPolicy
  }

  def createWhatIfPolicyTable(): Future[Seq[ResultSet]] = {
    create.ifNotExists().future()
  }

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
