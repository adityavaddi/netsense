package com.vz.nsp.eventsimulator.service.util

import org.slf4j.LoggerFactory

/**
 * Created by jittara on 4/28/17.
 * Logging the application
 */
trait Logging {

  lazy val log = LoggerFactory.getLogger(this.getClass)

}
