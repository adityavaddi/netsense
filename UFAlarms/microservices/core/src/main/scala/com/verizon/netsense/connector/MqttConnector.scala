package com.verizon.netsense.connector

import javax.net.ssl.SSLSocketFactory

import akka.stream.alpakka.mqtt.MqttConnectionSettings
import com.verizon.netsense.utils.{ConfigLoader, Logging, SslUtils}
import org.eclipse.paho.client.mqttv3.persist.MemoryPersistence

/**
 * Created by brefsdal on 4/3/17.
 */
object MqttConnector extends Logging {

  private val mqttConfig   = ConfigLoader.config.getConfig("mqtt")
  private val useSSL   = mqttConfig.getString("usessl")

  private val mqttInternalHost = mqttConfig.getString("internal.host")
  private val mqttInternalPort = mqttConfig.getInt("internal.port")

  private val mqttExternalHost = mqttConfig.getString("external.host")
  private val mqttExternalPort = mqttConfig.getInt("external.port")

  private val mqttClientId   = mqttConfig.getString("clientId")
  private val mqttUser   = mqttConfig.getString("auth_user")
  private val mqttPwd   = mqttConfig.getString("auth_pw")

  private val sslConn: Option[SSLSocketFactory] = useSSL match {
    case _ if useSSL.startsWith("true") =>
      val cacertfile = scala.io.Source.fromResource("sensity.ca.crt").bufferedReader()
      val certfile   = scala.io.Source.fromResource("sensity.com.crt").bufferedReader()
      val keyfile    = scala.io.Source.fromResource("sensity.com.key").bufferedReader()
      val password   = mqttConfig.getString("password")

      Some(
        SslUtils.getSocketFactory(
          cacertfile,
          certfile,
          keyfile,
          password
        )
      )
    case _ =>
      None
  }

  private def buildMqttHost(host: String, port: Int) = useSSL match {
    case _ if useSSL.startsWith("true") =>
      s"ssl://$host:$port"
    case _ =>
      s"tcp://$host:$port"
  }

  private def buildMqttSettings(host: String,
                                userName: Option[String] = None,
                                password: Option[String] = None,
                                useSSL: Option[SSLSocketFactory]): MqttConnectionSettings = {

    val authCredentials: Option[(String, String)] = (userName, password) match {
      case (Some(user), Some(pwd)) => Some(user, pwd)
      case _ => None
    }
    MqttConnectionSettings(
      host,
      mqttClientId,
      new MemoryPersistence,
      auth = authCredentials,
      socketFactory = sslConn,
      cleanSession = true,
      will = None
    )
  }

  def mqttSettings(useInternalMqtt: Boolean = true) = useInternalMqtt match {
    case true  => buildMqttSettings(buildMqttHost(mqttInternalHost, mqttInternalPort),
      None, None, None)
    case _ => buildMqttSettings(buildMqttHost(mqttExternalHost, mqttExternalPort),
      Some(mqttUser), Some(mqttPwd), sslConn)
  }
}