package com.vz.nsp.eventsimulator.mqtt;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import org.apache.commons.lang.math.RandomUtils;
import org.eclipse.paho.client.mqttv3.MqttClient;
import org.eclipse.paho.client.mqttv3.MqttConnectOptions;
import org.eclipse.paho.client.mqttv3.MqttException;
import org.eclipse.paho.client.mqttv3.persist.MemoryPersistence;

public class MqttConnectionTest {

  static String brokerIp  = "";
  static String username  = "farallones";
  static String password  = "sensity1";
  static int    qos       = 1;
  static int    nbClients = 500;

  public static void testMqttCon() {
 //String broker = "tcp://" + "34.203.191.34" + ":1883";
	  String broker = "tcp://" + "tcproxy-netsense.mon-marathon-service.mesos" + ":1883";
    MemoryPersistence persistence = new MemoryPersistence();
    
    


   
    try {
        for (int i = 0; i < nbClients; i++) {
            int id = RandomUtils.nextInt((int) 2e6);
            MqttClient sampleClient = new MqttClient(broker, String.valueOf(id), persistence);
            MqttConnectOptions connOpts = new MqttConnectOptions();
            connOpts.setUserName(username);
            connOpts.setPassword(password.toCharArray());
            connOpts.setCleanSession(true); //remove MQTT session objects, like queue created by RabbitMQ
            sampleClient.connect(connOpts); //just connect, don't subscribe
         //   sampleClient.getTopic("sensor");
         //   MqttMessage message = new MqttMessage();
         //   message.setPayload("test message".getBytes());
          //  sampleClient.publish("test", message);
            
        }
        
        System.out.println("done connecting");
        try {
            Thread.sleep(20000);
        } catch (Exception e) { }
    } catch (MqttException me) {
        me.printStackTrace();
    }
  }

  public static void main(String[] args) {
    testMqttCon();
  }
}