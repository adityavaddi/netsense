package com.vz.nsp.datasample.simulator.util

import java.io.File
import java.util.UUID

import com.github.tototoshi.csv.{CSVReader, CSVWriter}
import com.vz.nsp.datasample.simulator.util.NodeSensorReader.NodeSensor

import scala.util.Random

/**
 * Created by maidapr on 5/18/17.
 */
object NodeCsvWriter {
//object NodeCsvWriter extends App {

  /*

  val f = new File("nodeids.csv")
  val writer = CSVWriter.open(f)
  val stringList = List.range(0, 100).foldLeft(List[String]()) { (z, f) =>
    z :+ UUID.randomUUID().toString
  }
  writer.writeRow(stringList)
  writer.close()
   */

  val alphabets = "abcdefghijklmnopqrstuvwxyz12345"

  def random7DigitNumber = (1000000 + Random.nextInt(9000000)).toString

  def generateRandomNodeId = "N" + random7DigitNumber + alphabets.charAt(Random.nextInt(alphabets.length - 1))

  lazy val sensorsArray = Array("jt",
                                "mt",
                                "l",
                                "lIR",
                                "t",
                                "p",
                                "pc",
                                "T",
                                "v",
                                "vp",
                                "mi",
                                "mip",
                                "ai",
                                "aip",
                                "mw",
                                "aw",
                                "mP",
                                "aP",
                                "mPF",
                                "aPF",
                                "lt",
                                "rf",
                                "bc",
                                "WDT",
                                "bR")

  val f = new File("sensor-sample-kafka-simulator/src/main/resources/nodesensors.csv")
  val w = CSVWriter.open(f)
  val stringList = List.range(0, 1000).foldLeft(List[NodeSensor]()) { (z, f) =>
    z :+ NodeSensor(generateRandomNodeId, Random.shuffle(sensorsArray.toList).head)
  }
  w.writeRow(stringList)
  w.close()

}
