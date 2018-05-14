package com.verizon.netsense.config

import com.typesafe.config.ConfigFactory

object S3Config {
  lazy val config = ConfigFactory.load()

  lazy val s3Config = config.getConfig("s3")

  lazy val s3Bucket = s3Config.getString("bucket")
}
