package com.vz.ns.ts.service.service

import org.slf4j.LoggerFactory

trait Logging {

  lazy val log = LoggerFactory.getLogger(this.getClass)
}
