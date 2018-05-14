package com.verizon.netsense.database

import com.outworkers.phantom.CassandraTable
import com.outworkers.phantom.connectors.{CassandraConnection, RootConnector}
import com.outworkers.phantom.dsl.{ClusteringOrder, Descending, ResultSet, _}
import com.verizon.netsense.entity.SicStatus

import scala.concurrent.Future

/**
  * Cassandra  Model for sic_status table.
  */

trait SicStatusModel extends CassandraTable[SicStatusModel, SicStatus] with RootConnector {

  override def tableName: String = "sic_status"

  object msgId      extends StringColumn(this) with PartitionKey  // Request UUID
  object msgSrc     extends StringColumn(this) with PartitionKey  // KAFKA, MQTT
  object msgType    extends StringColumn(this) with PartitionKey  // REQ, RESP, ACK
  object state      extends StringColumn(this)                    //  SENT, RCVD, FAILED
  object id         extends TimeUUIDColumn(this) with ClusteringOrder with Descending
  object ts  extends DateTimeColumn(this)                         // Timestamp when event was triggered/ msg received
  object reason     extends StringColumn(this)                    // Reason we are in this "state"

  def store (sicstatus: SicStatus): Future[ResultSet] =
    insert
      .value(_.msgId, sicstatus.msgId)
      .value(_.msgSrc, sicstatus.msgSrc)
      .value(_.msgType, sicstatus.msgType)
      .value(_.id, sicstatus.id)
      .value(_.ts, sicstatus.ts)
      .value(_.state, sicstatus.state)
      .value(_.reason, sicstatus.reason)
      .future()

  def getByType(msgId: String, msgSrc: String,  mType: String): Future[Option[SicStatus]] =
    select
      .where(_.msgId eqs msgId)
      .and(_.msgSrc eqs msgSrc)
      .and(_.msgType eqs mType)
      .consistencyLevel_=(ConsistencyLevel.ONE)
      .one()


  override def fromRow(r: Row): SicStatus =
    SicStatus(id(r), msgId(r), msgSrc(r), msgType(r), state(r), ts(r), reason(r))
}

case class SicStatusDB(override val connector: CassandraConnection) extends Database[SicStatusDB](connector) {
  object model extends SicStatusModel with Connector
}

