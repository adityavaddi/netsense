package com.vz.nsp.parking.userdataservice.db

import com.outworkers.phantom.connectors.ContactPoints
import com.outworkers.phantom.dsl.{Database, KeySpaceDef}
import com.verizon.netsense.utils.ConfigLoader

object Defaults {
  val cassandraHost = ConfigLoader.config.getString("cassandra.host")
  val cassandraKeyspace = ConfigLoader.config.getString("cassandra.keyspace")
  val keySpaceBuilder = ContactPoints(Seq(cassandraHost))
  val connection = keySpaceBuilder.keySpace(cassandraKeyspace)
}

class UserDataConnector(override val connector: KeySpaceDef) extends Database[UserDataConnector](connector) {

  object userdataTestTable extends ConcreteUserDataTestTable with connector.Connector

}


object UserDataConnector extends UserDataConnector(Defaults.connection)
