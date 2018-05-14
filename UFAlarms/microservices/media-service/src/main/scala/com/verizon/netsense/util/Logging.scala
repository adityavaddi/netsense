package com.verizon.netsense.util

import org.slf4j.LoggerFactory

/**
 * Created by maidapr on 4/18/17.
 */
trait Logging {

  lazy val log = LoggerFactory.getLogger(this.getClass)

}
