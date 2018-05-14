package com.verizon.netsense.sse.specs.db

import java.nio.file.Files

import akka.persistence.cassandra.testkit.CassandraLauncher
import com.datastax.driver.core.Session
import com.outworkers.phantom.connectors.CassandraConnection
import com.verizon.netsense.sse.config.SSEConfigLoader._

/**
 * Created by sundach on 7/5/17.
 */
trait CassandraSpec {

  def startDB(): CassandraConnection = {
    val cassandraPort      = CassandraLauncher.randomPort
    val cassandraDirectory = Files.createTempDirectory("test").toFile
    val t0                 = System.nanoTime()
    CassandraLauncher.start(
      cassandraDirectory,
      CassandraLauncher.DefaultTestConfigResource,
      clean = true,
      port = 0
    )
    sseCassandraConnector
  }

  def stopDB() {
    CassandraLauncher.stop()
  }

  def createSchemaAndTable(session: Session) = {
    session.execute(
      "CREATE TABLE IF NOT EXISTS farallones.orghierarchy_by_nodeid  (\n  orgid varchar,\n  siteid varchar,\n  nodeid varchar,\n PRIMARY KEY(nodeid)\n)"
    )
    session.execute(
      "CREATE TABLE IF NOT EXISTS farallones.sse_filter  (\n  orgid varchar,\n  siteid varchar,\n  nodeid varchar,\n  " +
      "\n eventtype varchar,\n counter int, \n topicname varchar, \n PRIMARY KEY(orgid,siteid,nodeid,eventtype)\n) "
    )
    session.execute(
      "CREATE TABLE IF NOT EXISTS farallones.sse_filter_businessalerts  (\n  orgid varchar,\n  siteid varchar,\n  application varchar,\n  " +
      "\n triggerid varchar,\n counter int, \n topicname varchar, \n PRIMARY KEY(orgid,siteid,application,triggerid)\n) "
    )
  }

  def insertIntoDevices(session: Session) = {
    session.execute(
      "insert into farallones.orghierarchy_by_nodeid(nodeid,orgid,siteid) values ('JSV7_0','O0','S0') "
    )
    session.execute(
      "insert into farallones.orghierarchy_by_nodeid(nodeid,orgid,siteid) values ('JSV4_0','O0','S0') "
    )
    session.execute(
      "insert into farallones.orghierarchy_by_nodeid(nodeid,orgid,siteid) values ('JSV7_1', 'O1', 'S1') "
    )
    session.execute(
      "insert into farallones.orghierarchy_by_nodeid(nodeid,orgid,siteid) values ('JSV7_2', 'O2', 'S2') "
    )
    session.execute(
      "insert into farallones.orghierarchy_by_nodeid(nodeid,orgid,siteid) values ('JSV7_3', 'O3', 'S3') "
    )
    session.execute(
      "insert into farallones.orghierarchy_by_nodeid(nodeid,orgid,siteid) values ('JSV7_4', 'O4', 'S4') "
    )
    session.execute(
      "insert into farallones.orghierarchy_by_nodeid(nodeid,orgid,siteid) values ('JSV7_5', 'O5', 'S5') "
    )
    session.execute(
      "insert into farallones.orghierarchy_by_nodeid(nodeid,orgid,siteid) values ('JSV7_6', 'O6', 'S6') "
    )
    session.execute(
      "insert into farallones.orghierarchy_by_nodeid(nodeid,orgid,siteid) values ('JSV7_7', 'O7', 'S7') "
    )
    session.execute(
      "insert into farallones.orghierarchy_by_nodeid(nodeid,orgid,siteid) values ('JSV7_8', 'O8', 'S8') "
    )
    session.execute(
      "insert into farallones.orghierarchy_by_nodeid(nodeid,orgid,siteid) values ('JSV7_9', 'O9', 'S9') "
    )
    session.execute(
      "insert into farallones.orghierarchy_by_nodeid(nodeid,orgid,siteid) values ('JSV7_10', 'O10', 'S10') "
    )
  }

  def insertIntoFilter(session: Session) = {

    session.execute(
      "insert into farallones.sse_filter(orgid,siteid,nodeid,eventtype,counter,topicname) values ('O0','S0','JSV7_0','LoginReq',1,'/streamv1/O0/S0/JSV7_0/LoginReq') "
    )
    session.execute(
      "insert into farallones.sse_filter(orgid,siteid,nodeid,eventtype,counter,topicname) values ('O0','S0','JSV7_0','DeviceAlarm',1,'/streamv1/O0/S0/JSV7_0/DeviceAlarm') "
    )
    session.execute(
      "insert into farallones.sse_filter(orgid,siteid,nodeid,eventtype,counter,topicname) values ('O0','S0','JSV7_0','SensorSample',1,'/streamv1/O0/S0/JSV7_0/SensorSample') "
    )
    session.execute(
      "insert into farallones.sse_filter(orgid,siteid,nodeid,eventtype,counter,topicname) values ('O0','S0','JSV7_4','SensorSample',1,'/streamv1/O0/S0/JSV7_4/SensorSample') "
    )
    session.execute(
      "insert into farallones.sse_filter(orgid,siteid,nodeid,eventtype,counter,topicname) values ('O0','S0','JSV7_0','ConnectionStatus',1,'/streamv1/O0/S0/JSV7_0/ConnectionStatus') "
    )
    session.execute(
      "insert into farallones.sse_filter(orgid,siteid,nodeid,eventtype,counter,topicname) values ('O0','S0','JSV7_0','GpsSample',1,'/streamv1/O0/S0/JSV7_0/GpsSample') "
    )
    session.execute(
      "insert into farallones.sse_filter(orgid,siteid,nodeid,eventtype,counter,topicname) values ('O1','S1','JSV7_1','LoginReq',1,'/streamv1/O1/S1/JSV7_1/LoginReq') "
    )
    session.execute(
      "insert into farallones.sse_filter(orgid,siteid,nodeid,eventtype,counter,topicname) values ('O1','S1','JSV7_1','DeviceAlarm',1,'/streamv1/O1/S1/JSV7_1/DeviceAlarm') "
    )
    session.execute(
      "insert into farallones.sse_filter(orgid,siteid,nodeid,eventtype,counter,topicname) values ('O1','S1','JSV7_1','SensorSample',1,'/streamv1/O1/S1/JSV7_1/SensorSample') "
    )
    session.execute(
      "insert into farallones.sse_filter(orgid,siteid,nodeid,eventtype,counter,topicname) values ('O1','S1','JSV7_1','ConnectionStatus',1,'/streamv1/O1/S1/JSV7_1/ConnectionStatus') "
    )
    session.execute(
      "insert into farallones.sse_filter(orgid,siteid,nodeid,eventtype,counter,topicname) values ('O1','S1','JSV7_1','GpsSample',1,'/streamv1/O1/S1/JSV7_1/GpsSample') "
    )
    session.execute(
      "insert into farallones.sse_filter(orgid,siteid,nodeid,eventtype,counter,topicname) values ('+','+','+','DeviceAlarm',1,'/streamv1/+/+/+/DeviceAlarm') "
    )
    session.execute(
      "insert into farallones.sse_filter(orgid,siteid,nodeid,eventtype,counter,topicname) values ('O1','S1','+','SensorSample',1,'/streamv1/O1/S1/+/SensorSample') "
    )
    session.execute(
      "insert into farallones.sse_filter(orgid,siteid,nodeid,eventtype,counter,topicname) values ('O1','S1','+','ConnectionStatus',1,'/streamv1/O1/S1/+/ConnectionStatus') "
    )
    session.execute(
      "insert into farallones.sse_filter(orgid,siteid,nodeid,eventtype,counter,topicname) values ('+','+','+','GpsSample',1,'/streamv1/+/+/+/GpsSample') "
    )
    session.execute(
      "insert into farallones.sse_filter(orgid,siteid,nodeid,eventtype,counter,topicname) values ('O1','S1','+','LoginReq',1,'/streamv1/O1/S1/+/LoginReq') "
    )
    session.execute(
      "insert into farallones.sse_filter(orgid,siteid,nodeid,eventtype,counter,topicname) values ('O1','S1','JSV7_1','+',1,'/streamv1/O1/S1/JSV7_1/+')"
    )
    session.execute(
      "insert into farallones.sse_filter(orgid,siteid,nodeid,eventtype,counter,topicname) values ('O1','S1','+','+',1,'/streamv1/O1/S1/+/+') "
    )
    session
      .execute("select * from farallones.orghierarchy_by_nodeid")
      .forEach(x => println("orghierarchy_by_nodeid ::::" + x))
    session.execute("select * from farallones.sse_filter").forEach(x => println("sse_filter::::" + x))
    session
      .execute(
        "select orgid,siteid,nodeid from farallones.orghierarchy_by_nodeid where nodeid = 'JSV7_0' ALLOW FILTERING"
      )
      .forEach(x => println("GetByNode:::" + x))
  }

  def insertIntoBusinessAlertFilter(session: Session) = {

    session.execute(
      "insert into farallones.sse_filter_businessalerts(orgid,siteid,application,triggerid,counter,topicname) values ('O0','S0','parking','trigger0',1,'/streamv1/O0/S0/businessalert/parking/trigger0') "
    )
    session.execute(
      "insert into farallones.sse_filter_businessalerts(orgid,siteid,application,triggerid,counter,topicname) values ('O0','S0','lightning','trigger0',1,'/streamv1/O0/S0/businessalert/lightning/trigger0') "
    )
    session.execute(
      "insert into farallones.sse_filter_businessalerts(orgid,siteid,application,triggerid,counter,topicname) values ('O0','S0','parking','trigger1',1,'/streamv1/O0/S0/businessalert/parking/trigger1') "
    )
    session.execute(
      "insert into farallones.sse_filter_businessalerts(orgid,siteid,application,triggerid,counter,topicname) values ('O0','S0','lightning','trigger1',1,'/streamv1/O0/S0/businessalert/lightning/trigger1') "
    )
    session.execute(
      "insert into farallones.sse_filter_businessalerts(orgid,siteid,application,triggerid,counter,topicname) values ('O1','S1','parking','trigger0',1,'/streamv1/O1/S1/businessalert/parking/trigger0') "
    )
    session.execute(
      "insert into farallones.sse_filter_businessalerts(orgid,siteid,application,triggerid,counter,topicname) values ('O1','S1','lightning','trigger0',1,'/streamv1/O1/S1/businessalert/lightning/trigger0') "
    )
    session.execute(
      "insert into farallones.sse_filter_businessalerts(orgid,siteid,application,triggerid,counter,topicname) values ('O1','S1','parking','trigger1',1,'/streamv1/O1/S1/businessalert/parking/trigger1') "
    )
    session.execute(
      "insert into farallones.sse_filter_businessalerts(orgid,siteid,application,triggerid,counter,topicname) values ('O1','S1','lightning','trigger1',1,'/streamv1/O1/S1/businessalert/lightning/trigger1') "
    )
    session.execute(
      "insert into farallones.sse_filter_businessalerts(orgid,siteid,application,triggerid,counter,topicname) values ('O1','S1','+','trigger1',1,'/streamv1/O1/S1/businessalert/+/trigger1') "
    )
    session.execute(
      "insert into farallones.sse_filter_businessalerts(orgid,siteid,application,triggerid,counter,topicname) values ('O1','S1','+','trigger0',1,'/streamv1/O1/S1/businessalert/+/trigger0') "
    )
    session.execute(
      "insert into farallones.sse_filter_businessalerts(orgid,siteid,application,triggerid,counter,topicname) values ('O1','S1','parking','+',1,'/streamv1/O1/S1/businessalert/parking/+')"
    )
    session.execute(
      "insert into farallones.sse_filter_businessalerts(orgid,siteid,application,triggerid,counter,topicname) values ('O1','S1','+','+',1,'/streamv1/O1/S1/businessalert/+/+') "
    )
    session
      .execute("select * from farallones.sse_filter_businessalerts")
      .forEach(x => println("sse_filter_businessalerts::::" + x))
  }

  def deleteFilter(session: Session) = {
    session.execute("Truncate farallones.orghierarchy_by_nodeid")
    session.execute("Truncate farallones.sse_filter")
    session.execute("Truncate farallones.sse_filter_businessalerts")
  }

  def getbyNodeID(session: Session) = {
    val row = session.execute(
      "select orgid,siteid,nodeid from farallones.orghierarchy_by_nodeid where nodeid = 'JSV7_0' ALLOW FILTERING"
    )
    println("Row::" + row)
  }

}
