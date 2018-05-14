package com.verizon.netsense.services.bridge.utils

//import org.slf4j.LoggerFactory

import com.typesafe.scalalogging.Logger

trait Logging {

  lazy val log = Logger(this.getClass) //LoggerFactory.getLogger(this.getClass)
}
