package com.verizon.netsense.helper

import awscala.s3.{S3, S3Object, S3ObjectSummary}
import awscala.{DateTime, Region}
import com.typesafe.config.ConfigFactory
import com.verizon.netsense.config.S3Config
import com.verizon.netsense.entity.Firmware
import com.verizon.netsense.metrics.Instrumented
import com.verizon.netsense.utils.Logging
import nl.grons.metrics.scala.Timer

trait OTAFirmware extends Logging {
  def getFirmwares(): List[Firmware]

  def getTmpUrl(key: String): String

  def createFirmwareID(commitID: String, `type`: String): String

  def getCommitFromFwID(fwID: String): String

  def getTypeFromFwID(fwID: String): String
}

object OTAFirmwareImpl
  extends OTAFirmware
    with Instrumented {

  lazy val config = ConfigFactory.load()
  lazy val s3Config = config.getConfig("s3")
  lazy val accessKeyId = s3Config.getString("access-key-id")
  lazy val secretAccessKey = s3Config.getString("secret-access-key")

  implicit val region: Region = Region.US_WEST_2
  implicit val s3: S3 = S3.apply(accessKeyId = accessKeyId, secretAccessKey = secretAccessKey)

  log.info(s"connecting to s3 via ${accessKeyId}")

  val getFwtimer: Timer = metrics.timer("get-firmware-from-s3")
  val getURLtimer: Timer = metrics.timer("get-url-from-s3")

  val falconRegex = "^fiat-lux-(falcon-q)-([0-9][.][0-9][.][0-9])[.]([0-9a-fA-F]{7})[-]([0-9]{8})[-]ubuntu.fwpkg$".r
  val merlinRegex = "^fiat-lux-(merlin)-([0-9][.][0-9][.][0-9])[.]([0-9a-fA-F]{7})[-]([0-9]{8})[-]ubuntu.fwpkg$".r
  val vdkRegex = "^fiat-tabulae-(vdkmaster)-([0-9][.][0-9][.][0-9])[.]([0-9a-fA-F]{7})[-]([0-9]{8})[-]ubuntu.fwpkg$".r
  val cnextRegex = "^fiat-lux-(cnext)-([0-9][.][0-9][.][0-9])[.]([0-9a-fA-F]{7})[-]([0-9]{8})[-]ubuntu.fwpkg$".r

  def filterFirmware(name: String): Boolean = {
    try {
      name match {
        case falconRegex(_, _, _, _) => true
        case merlinRegex(_, _, _, _) => true
        case vdkRegex(_, _, _, _) => true
        case cnextRegex(_, _, _, _) => true
        case key =>
          log.error(s"${key} is not matching any regex")
          false
      }
    } catch {
      case e: Exception =>
        log.error(s"got exception ${e.getMessage} matching ${name} : ${e.getStackTrace.toString}")
        false
    }
  }

  def createFirmware(name: String, version: String, commitID: String, dateString: String, key: String, obj: S3ObjectSummary): Firmware = {
    val dateRegex = "^([0-9]{4})([0-9]{2})([0-9]{2})$".r

    val when = dateString match {
      case dateRegex(year, month, day) =>
        DateTime.parse(s"${year}-${month}-${day}").getMillis()
      case _ =>
        obj.getLastModified.getTime
    }

    Firmware(
      firmwareid = createFirmwareID(commitID, name),
      commitid = commitID,
      version = version,
      `type` = name,
      when = when,
      size = obj.getSize,
      s3 = key
    )
  }

  def createFirmwareID(commitID: String, `type`: String): String = s"${commitID}-${`type`}"

  def getCommitFromFwID(fwID: String): String = {
    val arr = fwID.split("-")

    if (arr.length == 0) {
      "UnknownID"
    } else {
      arr(0)
    }
  }

  def getTypeFromFwID(fwID: String): String = {
    val arr = fwID.split("-")

    if (arr.length <= 1) {
      "UnknownType"
    } else {
      arr(1)
    }
  }

  def getFirmwares(): List[Firmware] = {
    getFwtimer.time(
      s3.bucket(S3Config.s3Bucket) match {
        case None => List()
        case Some(bucket) =>
          val summaries: Seq[S3ObjectSummary] = bucket.objectSummaries()

          summaries
            .toList
            .filter(obj => filterFirmware(obj.getKey))
            .map(obj => {
              val key: String = obj.getKey

              key match {
                case falconRegex(name, version, commitID, date) =>
                  createFirmware(name, version, commitID, date, key, obj)
                case merlinRegex(name, version, commitID, date) =>
                  createFirmware(name, version, commitID, date, key, obj)
                case vdkRegex(name, version, commitID, date) =>
                  createFirmware(name, version, commitID, date, key, obj)
                case cnextRegex(name, version, commitID, date) =>
                  createFirmware(name, version, commitID, date, key, obj)
              }
            })
      }
    )
  }

  def getTmpUrl(key: String): String = {
    val validFor = 15

    getURLtimer.time(
      s3.bucket(S3Config.s3Bucket) match {
        case None => "ERROR_S3_BUCKET"
        case Some(bucket) =>
          bucket.getObject(key) match {
            case None => "ERROR_S3_OBJ"
            case Some(obj) =>
              obj.generatePresignedUrl(DateTime.now.plusMinutes(validFor)).toString
          }
      }
    )
  }
}