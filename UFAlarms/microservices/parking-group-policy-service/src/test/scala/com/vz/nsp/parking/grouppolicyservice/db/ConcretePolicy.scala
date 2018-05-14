package com.vz.nsp.parking.grouppolicyservice.db

import com.outworkers.phantom.dsl._
import com.vz.nsp.parking.model._

abstract class ConcretePolicy extends Table[ConcretePolicy, Policy] {

  def truncatePolicy = truncate().future()

  def customTypesPolicy()=parkingServicesDbTest.customTypesPolicy()

  def createPolicyTable=create.ifNotExists().future()
  //def createPolicyTable=parkingServicesDbTest.indexesForPolicy.future()

  override def tableName: String = "parkingpolicy"

  object uid extends StringColumn with PartitionKey

  object version extends IntColumn with ClusteringOrder with Descending

  object policyauthorizerid extends StringColumn

  object siteid extends StringColumn with Index

  object orgid extends StringColumn with Index

  object tags extends SetColumn[String] with Index

  object policyLevelViolations extends SetColumn[PolicyLevelViolation]

  object islibrarypolicy extends BooleanColumn

  object hashvalue extends StringColumn

  object createdon extends LongColumn

  object lastupdated extends LongColumn

  object isdeleted extends BooleanColumn with Index

  object name extends StringColumn with Index

  object description extends OptionalCol[String]

  object timezone extends StringColumn

  object state extends OptionalCol[String]

  object policyrule extends SetColumn[PolicyRule]


}
