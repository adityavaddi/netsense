package com.vz.nsp.parking.tagservice.db

import com.outworkers.phantom.dsl._
import com.outworkers.phantom.keys.PartitionKey
import com.vz.nsp.parking.model._

import scala.concurrent.Future

abstract class ConcreteWhatIfTag extends Table[ConcreteWhatIfTag, Tag] {

  override def tableName: String = "parking_whatif_tag"

  object tagid extends StringColumn with ClusteringOrder

  object name extends StringColumn

  object description extends StringColumn

  object orgid extends StringColumn with PartitionKey

  object siteid extends StringColumn with PartitionKey

  object createdon extends LongColumn

  object lastupdated extends LongColumn

  object isdeleted extends BooleanColumn

  def truncateWhatIfTagTable(): Future[ResultSet] =
    truncate().future()

  def createWhatIfTagTable() = create.ifNotExists().future()

}
