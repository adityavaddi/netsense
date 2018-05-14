package com.vz.nsp.eventsimulator.service.util

import java.util.UUID

import com.vz.nsp.eventsimulator.service.config.ConfigLoader
import com.vz.nsp.eventsimulator.service.model.Event._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Random

/**
 * Created by Jittara on 4/28/17.
 * Message formats of the messages
 */
object MessageFormat {

  val random = new scala.util.Random

  def getOne(arr: Array[AnyRef]) =
    arr(random.nextInt(arr.size))
  def scheduleEvent =
    ScheduleEvent(
      Array(
        getOne(NodeUtil.nodesArray.toArray()).toString
          .concat(random.nextInt(ConfigLoader.numberOfNodes + 1).toString)
      ),
      random.nextInt((100) + 1),
      7,
      random.nextInt((23) + 1),
      random.nextInt((59) + 1),
      random.nextInt((59) + 1),
      5,
      5,
      5
    )

  def loginEvent =
    LoginEvent(
      "Login",
      getOne(NodeUtil.loginAuthArray).toString,
      "f3:01:7e:9e:a7:0b",
      "unode-v4",
      "xxxxxxxx",
      "192.168.66.145",
      "dd:49:61:9b:d6:0b",
      "LoginReq",
      "SensitySim",
      getOne(NodeUtil.nodesArray.toArray()).toString
        .concat(random.nextInt(ConfigLoader.numberOfNodes + 1).toString),
      "x",
      "",
      1,
      "a3441a938",
      ""
    )
  def alarmEvent =
    AlarmEvent(
      getOne(NodeUtil.alarmSeverityArray).toString,
      getOne(NodeUtil.alarmaTypesArray).toString,
      "message ",
      "DeviceAlarm",
      getOne(NodeUtil.nodesArray.toArray()).toString
        .concat(random.nextInt(ConfigLoader.numberOfNodes + 1).toString)
    )

  def actionEvent =
    ActionEvent(
      Array(
        getOne(NodeUtil.nodesArray.toArray()).toString
          .concat(random.nextInt(ConfigLoader.numberOfNodes + 1).toString)
      ),
      getOne(NodeUtil.namesArray).toString,
      getOne(NodeUtil.actionTypesArray).toString
    )
  def deviceEvent = DeviceEvent(
    Array(
      getOne(NodeUtil.nodesArray.toArray()).toString
        .concat(random.nextInt(ConfigLoader.numberOfNodes + 1).toString)
    ),
    getOne(NodeUtil.namesArray).toString,
    getOne(NodeUtil.sensorsArray1).toString,
    Random.nextInt((1000) + 1).toLong,
    System.currentTimeMillis()
  )

  def lightEvent =
    LightEvent(
      Array(
        getOne(NodeUtil.nodesArray.toArray()).toString
          .concat(random.nextInt(ConfigLoader.numberOfNodes + 1).toString)
      ),
      getOne(NodeUtil.namesArray).toString,
      3,
      1,
      (random.nextInt((100) + 1)),
      "",
      "volatile"
    )
  def sensorEvent =
    SensorEvent(
      "SensorSample",
      getOne(NodeUtil.nodesArray.toArray()).toString
        .concat(random.nextInt(ConfigLoader.numberOfNodes + 1).toString),
      getOne(NodeUtil.sensorsArray1).toString,
      "null",
      1
    )

  /**
   * Sample Alaram message
   */
  def alarmFormat: Array[Byte] = MsgPackUtil.packer(alarmEvent)

  /**
   * Sample Sensor message
   */
  def sensorFormat: Array[Byte] = MsgPackUtil.packer(sensorEvent)

  /**
   * Sample Login message
   */
  def loginFormat: Array[Byte] = MsgPackUtil.packer(loginEvent)

  def getEvent(baseEvent: BaseEvent)(implicit ec: ExecutionContext): Future[Array[Byte]] =
    Future(MsgPackUtil.packer(baseEvent))

  /**
   * Helpers to generate the Sample device events from DeviceEvent
   *
   * @return - Future Value of MsgPack encoded Array[Byte] of simulated DeviceEvent
   */
  def generateDeviceEvent(implicit ec: ExecutionContext): Future[Array[Byte]] = getEvent(deviceEvent)

  def generateActionEvent(implicit ec: ExecutionContext): Future[Array[Byte]] = getEvent(actionEvent)

  def generateLightEvent(implicit ec: ExecutionContext): Future[Array[Byte]] = getEvent(lightEvent)

  def generateScheduleEvent(implicit ec: ExecutionContext): Future[Array[Byte]] = getEvent(scheduleEvent)

}
