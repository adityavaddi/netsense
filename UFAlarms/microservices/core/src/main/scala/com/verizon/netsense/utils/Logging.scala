package com.verizon.netsense.utils

import org.slf4j.{Logger, LoggerFactory}

/**
 * Created by maidapr on 4/18/17.
 */
trait Logging {

  implicit lazy val log: Logger = LoggerFactory.getLogger(this.getClass)

}
