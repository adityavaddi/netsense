package com.verizon.netsense.db.neo4j

import com.verizon.netsense.connector.Neo4jConnector
import org.neo4j.driver.v1.Record

import scala.util.Try


/**
  * Created by maidapr on 1/24/18.
  */
trait Neo4jProductionDb {

  def execute(cypher: String, params: java.util.HashMap[String, Object]): Try[List[Record]] = {
    Neo4jConnector.executeToRecordList(cypher, params)
  }




}
