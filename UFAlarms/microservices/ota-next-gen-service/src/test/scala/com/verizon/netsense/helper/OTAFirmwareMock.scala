package com.verizon.netsense.helper

import com.verizon.netsense.entity.Firmware

object OTAFirmwareMock extends OTAFirmware {
  def createFirmwareID(commitID: String, `type`: String): String = OTAFirmwareImpl.createFirmwareID(commitID, `type`)

  def getCommitFromFwID(fwID: String): String = OTAFirmwareImpl.getCommitFromFwID(fwID)

  def getTypeFromFwID(fwID: String): String = OTAFirmwareImpl.getTypeFromFwID(fwID)

  def getFirmwares(): List[Firmware] = {
    List(Firmware(
      firmwareid = "F007-merlin",
      commitid = "F007",
      version = "V001",
      `type` = "merlin",
      when = 1,
      size = 1,
      s3 = "merlin-V001-F007.zip"
    ))
  }

  def getTmpUrl(key: String): String = {
    "http://127.0.0.1/mock"
  }
}
