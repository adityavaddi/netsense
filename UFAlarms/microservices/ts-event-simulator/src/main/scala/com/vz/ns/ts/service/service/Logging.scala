package com.vz.ns.ts.service.service

import org.slf4j.LoggerFactory

/**
 * Created by donthgo on 5/2/17.
 */
trait Logging {

  lazy val log = LoggerFactory.getLogger(this.getClass)
}
