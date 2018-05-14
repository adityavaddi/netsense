package com.verizon.netsense.helper

import com.verizon.netsense.connector.Neo4jConnector
import com.verizon.netsense.exceptions.CustomExceptions.NoScheduleFoundException
import org.neo4j.driver.v1.{Driver, Record}
import org.neo4j.driver.v1.types.Node

import scala.util.Try


/**
  * Created by maidapr on 1/24/18.
  */
trait Neo4jProductionDb {

  def execute(cypher: String, params: java.util.HashMap[String, Object]): Try[List[Record]] = {
    Neo4jConnector.executeToRecordList(cypher, params)
  }




}
