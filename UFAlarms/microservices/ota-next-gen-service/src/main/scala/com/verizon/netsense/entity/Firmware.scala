package com.verizon.netsense.entity

case class Firmware(firmwareid: String,
                    commitid: String,
                    version: String,
                    `type`: String,
                    when: Long,
                    size: Long,
                    s3: String) extends Entity
