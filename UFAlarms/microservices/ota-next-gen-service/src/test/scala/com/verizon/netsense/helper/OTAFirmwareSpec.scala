package com.verizon.netsense.helper

import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, FunSuiteLike, MustMatchers}
import org.scalatest.concurrent.ScalaFutures

class OTAFirmwareSpec extends FunSuiteLike
  with MustMatchers
  with ScalaFutures
  with BeforeAndAfterAll
  with BeforeAndAfterEach {

  override protected def beforeAll(): Unit =
    super.beforeAll()

  override def afterAll(): Unit =
    super.afterAll()

  test("check firmware regex") {
    val OTAFirmwareImpl.falconRegex(fName, fVersion, fCommitID, fDate) = "fiat-lux-falcon-q-2.6.2.b2e668f-20180117-ubuntu.fwpkg"

    assert(fName == "falcon-q")
    assert(fVersion == "2.6.2")
    assert(fCommitID == "b2e668f")
    assert(fDate == "20180117")

    val OTAFirmwareImpl.merlinRegex(mName, mVersion, mCommitID, mDate) = "fiat-lux-merlin-2.7.0.a788b38-20171219-ubuntu.fwpkg"

    assert(mName == "merlin")
    assert(mVersion == "2.7.0")
    assert(mCommitID == "a788b38")
    assert(mDate == "20171219")

    val OTAFirmwareImpl.vdkRegex(vName, vVersion, vCommitID, vDate) = "fiat-tabulae-vdkmaster-2.7.0.debf9eb-20180102-ubuntu.fwpkg"

    assert(vName == "vdkmaster")
    assert(vVersion == "2.7.0")
    assert(vCommitID == "debf9eb")
    assert(vDate == "20180102")

    val OTAFirmwareImpl.cnextRegex(cName, cVersion, cCommitID, cDate) = "fiat-lux-cnext-2.7.0.a788b38-20171219-ubuntu.fwpkg"

    assert(cName == "cnext")
    assert(cVersion == "2.7.0")
    assert(cCommitID == "a788b38")
    assert(cDate == "20171219")
  }

  test("check filterFirmware positive") {
    assert(OTAFirmwareImpl.filterFirmware("fiat-lux-falcon-q-2.6.2.b2e668f-20180117-ubuntu.fwpkg"))
    assert(OTAFirmwareImpl.filterFirmware("fiat-lux-merlin-2.7.0.a788b38-20171219-ubuntu.fwpkg"))
    assert(OTAFirmwareImpl.filterFirmware("fiat-lux-merlin-2.8.0.925cb22-20180123-ubuntu.fwpkg"))
    assert(OTAFirmwareImpl.filterFirmware("fiat-lux-merlin-2.8.0.c998c20-20180124-ubuntu.fwpkg"))
    assert(OTAFirmwareImpl.filterFirmware("fiat-tabulae-vdkmaster-2.7.0.debf9eb-20180102-ubuntu.fwpkg"))
    assert(OTAFirmwareImpl.filterFirmware("fiat-tabulae-vdkmaster-2.8.0.6ea8e47-20180124-ubuntu.fwpkg"))
    assert(OTAFirmwareImpl.filterFirmware("fiat-tabulae-vdkmaster-2.8.0.925cb22-20180123-ubuntu.fwpkg"))
    assert(OTAFirmwareImpl.filterFirmware("fiat-lux-falcon-q-2.6.2.b2e668f-20180117-ubuntu.fwpkg"))
    assert(OTAFirmwareImpl.filterFirmware("fiat-lux-falcon-q-2.7.0.5a574b7-20180206-ubuntu.fwpkg"))
    assert(OTAFirmwareImpl.filterFirmware("fiat-lux-merlin-2.7.0.5a574b7-20180206-ubuntu.fwpkg"))
    assert(OTAFirmwareImpl.filterFirmware("fiat-tabulae-vdkmaster-2.8.0.925cb22-20180123-ubuntu.fwpkg"))
    assert(OTAFirmwareImpl.filterFirmware("fiat-lux-cnext-2.7.0.5a574b7-20180206-ubuntu.fwpkg"))
  }

  test("check filterFirmware negative") {
    assert(!OTAFirmwareImpl.filterFirmware("fiat-lux-falcon-q-2.6.2.b2e668f.zip"))
    assert(!OTAFirmwareImpl.filterFirmware("fiat-lux-falcon-q-2.7.0.5a574b7.zip"))
    assert(!OTAFirmwareImpl.filterFirmware("fiat-lux-merlin-2.7.0.5a574b7.zip"))
    assert(!OTAFirmwareImpl.filterFirmware("fiat-tabulae-vdkmaster-2.8.0.925cb22.zip"))
    assert(!OTAFirmwareImpl.filterFirmware("fiat-lux-cnext-2.7.0.5a574b7.zip"))
    assert(!OTAFirmwareImpl.filterFirmware("random string"))
  }

  test("getCommitFromFwID") {
    assert(OTAFirmwareImpl.getCommitFromFwID("123-type") == "123")
    assert(OTAFirmwareMock.getCommitFromFwID("123-type") == "123")
    assert(OTAFirmwareImpl.getCommitFromFwID("123") == "123")
    assert(OTAFirmwareMock.getCommitFromFwID("123") == "123")
    assert(OTAFirmwareImpl.getCommitFromFwID("123-type-789") == "123")
    assert(OTAFirmwareImpl.getCommitFromFwID("123-type-789") == "123")
  }

  test("getTypeFromFwID") {
    assert(OTAFirmwareImpl.getTypeFromFwID("123-type") == "type")
    assert(OTAFirmwareMock.getTypeFromFwID("123-type") == "type")
    assert(OTAFirmwareImpl.getTypeFromFwID("123") == "UnknownType")
    assert(OTAFirmwareMock.getTypeFromFwID("123") == "UnknownType")
    assert(OTAFirmwareImpl.getTypeFromFwID("123-type-789") == "type")
    assert(OTAFirmwareImpl.getTypeFromFwID("123-type-789") == "type")
  }
}