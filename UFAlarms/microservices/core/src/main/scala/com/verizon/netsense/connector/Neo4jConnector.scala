package com.verizon.netsense.connector

import java.util
import java.util.concurrent.TimeUnit

import com.typesafe.config.ConfigFactory
import org.neo4j.driver.v1.{AuthTokens, Config, Driver, GraphDatabase}

import scala.collection.JavaConversions._
import scala.util.{Failure, Success, Try}


/**
  * Created by brefsdal on 3/17/17.
  */
object Neo4jConnector{

  private val config = ConfigFactory.load()

  private val hosts = config.getString("neo4j.host").split(",").toSeq
  private val port = config.getInt("neo4j.port")

  val neoconfig: Config = Config.build().withConnectionLivenessCheckTimeout(1, TimeUnit.SECONDS).withConnectionTimeout(5, TimeUnit.SECONDS).withMaxIdleSessions(10).withRoutingFailureLimit(1).withRoutingRetryDelay(5, TimeUnit.SECONDS).toConfig()

  def connect(hosts: Seq[String], port: Int): Option[Driver] ={
    val context = if (hosts.length > 1) "+routing" else ""
    def attempt(host: String, port: Int): Try[Driver] = Try(GraphDatabase.driver(s"bolt$context://$host:$port", AuthTokens.basic("neo4j", "neo4j1"),neoconfig))

    hosts.foldLeft(List[Driver]()){ (acc, host) =>
      attempt(host, port) match {
        case Success(drvr) => if(acc.nonEmpty) acc else acc :+ drvr
        case Failure(e) => println(s"Failed to connect $e")
          acc
      }
    } match {
      case List(drvr) => Some(drvr)
      case _ => None
    }
  }

  lazy val driver: Option[Driver] = connect(hosts, port)

  def execute(cypher: String, props: util.HashMap[String, Object]): Try[Map[String, Any]] = driver match {
    case Some(graphDriver) => Try{
      val session = graphDriver.session
      try {
        session.run(cypher, props).foldLeft(Map[String, Any]()){ (acc, resultSet) =>
          acc ++ resultSet.asMap
        }
      } finally {
        session.close()
      }
    }
    case _ => println("Driver NOT FOUND!")
      Try(Map())
  }

  def executeToRecordList(cypher: String, props: util.HashMap[String, Object]) = driver match {
    case Some(graphDriver) =>
      Try {
        val session = graphDriver.session
        try {
          session.run(cypher, props).toList

        } finally {
          session.close()
        }
      }
    case _ =>
      println("Driver NOT FOUND!")
      Try(List())
  }

}
