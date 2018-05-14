package com.verizon.netsense.services.bridge.flow

import java.util.Properties

import com.verizon.netsense.utils.Logging
import io.moquette.server.Server

/**
 * Created by thimija on 7/19/17.
 *
 */
trait EmbeddedMQTT extends Logging {

  /*
    Embedded MQTT host & port
   */
  val _host: String = "0.0.0.0"
  val _port: String = "1883"

  val mqttBroker = new Server()

  /**
   * Start MQTT Broker
   */
  def startBroker(): Unit = {
    startServer()
    addShutdownHook
  }

  /**
   * Start MQTT Server
   */
  private def startServer(): Unit = {
    val configProps = new Properties()
    configProps.setProperty("port", _port)
    configProps.setProperty("host", _host)
    configProps.setProperty("allow_anonymous", "true")

    mqttBroker.startServer(configProps)

    addShutdownHook
  }

  /**
   * Stop MQTT Broker
   */
  def stopBroker(): Unit =
    mqttBroker.stopServer()

  /**
   * Stop MQTT Server
   */
  private def addShutdownHook: Unit =
    sys.addShutdownHook({
      log.debug("Stopping embedded mqttbroker")
      mqttBroker.stopServer()
      log.debug("Stopped embedded mqttbroker")
    })

}
