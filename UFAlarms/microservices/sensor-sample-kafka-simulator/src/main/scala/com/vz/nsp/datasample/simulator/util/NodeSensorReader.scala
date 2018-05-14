package com.vz.nsp.datasample.simulator.util

import java.io.{File, InputStream, InputStreamReader}

import com.github.tototoshi.csv.CSVReader

import scala.util.Random

/**
 * Created by maidapr on 5/19/17.
 */
object NodeSensorReader {

  case class NodeSensor(nodeid: String, sensor: String)

  private val inputStream: InputStream             = this.getClass.getClassLoader.getResourceAsStream("nodesensors.csv")
  private val inputStreamReader: InputStreamReader = new InputStreamReader(inputStream)

  implicit val reader      = CSVReader.open(inputStreamReader)
  implicit val elementList = reader.all().head

  val regex = "NodeSensor\\((.*),(.*)\\)".r

  object NodeSensorString {
    def unapply(str: String): Option[NodeSensor] = str match {
      case regex(n, s) => Some(NodeSensor(n, s))
      case _           => None
    }
  }

  def randomElement = NodeSensorString.unapply(elementList(Random.nextInt(999)))

}
