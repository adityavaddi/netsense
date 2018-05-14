package com.vz.nsp.eventsimulator.service.util

import com.vz.nsp.eventsimulator.service.config.ConfigLoader

/**
 * Created by Jittara on 4/5/17.
 *
 */
object MQTTConnection extends Logging {

  //Broker URL
  lazy val brokerUrl = ConfigLoader.protocol + "://" + ConfigLoader.host + ":" + ConfigLoader.mqttPort
  // Topic
  lazy val subscriberTopic = ConfigLoader.mqttSubscriberTopic
  lazy val publisherTopic  = ConfigLoader.mqttTopic

  /**
   * MqttConnectOptions preparation
   */
  import org.eclipse.paho.client.mqttv3.MqttConnectOptions

  def getMqttConnection: MqttConnectOptions = {
    // MqttConnectOptions with specific username, password, maxInflight and keepAliveInterval
    lazy val obj = new MqttConnectOptions
    obj setUserName ConfigLoader.userName
    obj setPassword ConfigLoader.password.toCharArray
    obj setMaxInflight ConfigLoader.maxInflight
    obj setKeepAliveInterval ConfigLoader.keepAlive
    log.debug("Connecting to MQTT with username " + ConfigLoader.userName + " Password " + ConfigLoader.password)
    obj
  }
}
