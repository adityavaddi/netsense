package com.verizon.netsense.services.gps

import com.verizon.netsense.services.gps.config.TestSuiteConfig
import com.verizon.netsense.services.gps.db.EmbeddedDatabase
import org.scalatest._

trait BaseSpec extends FlatSpecLike
  with MustMatchers
  with BeforeAndAfterAll
  with BeforeAndAfterEach
  with TestSuiteConfig
  with EmbeddedDatabase {

}


