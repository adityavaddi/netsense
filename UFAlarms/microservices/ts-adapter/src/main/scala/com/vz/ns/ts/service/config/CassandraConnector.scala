package com.vz.ns.ts.service.config

import com.outworkers.phantom.dsl._
import com.vz.ns.ts.service.config.ConfigLoader._
import com.vz.ns.ts.service.service.Logging

object CassandraConnector extends Logging {

  val connector: KeySpaceDef = {
    log.info("Connecting to cassandra")
    log.debug("cassandraHost: " + cassandraHost)
    log.debug("cassandraPort: " + cassandraPort)
    log.debug("cassandraKeyspace: " + cassandraKeyspace)
    ContactPoint.apply(host = cassandraHost, port = cassandraPort).keySpace(cassandraKeyspace)
  }
}
