package com.verizon.netsense.config

import com.verizon.netsense.connector.Neo4jConnector

/**
 * Created by maidapr on 6/8/17.
 */
object Neo4jConnection {

  implicit lazy val neo4jDriver =
    Neo4jConnector.driver.getOrElse(throw new IllegalArgumentException("neo4j undefined"))

}
