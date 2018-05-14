package com.verizon.netsense.connector

import akka.actor.ActorSystem
import com.verizon.netsense.utils.ConfigLoader._
import com.verizon.netsense.utils.Logging


object ConnectionRetry extends Logging{
  def rabbitMQMaxRetryCheck(currentRetryCount: Long) (implicit system: ActorSystem): Unit = {
    maxRetryCheck(currentRetryCount, rabbitMQRetryCount)
  }

  def mqttMaxRetryCheck(currentRetryCount: Long) (implicit system: ActorSystem): Unit = {
    maxRetryCheck(currentRetryCount, mqttRetryCount)
  }

  //This method terminates the currently running JVM if the max retry count is reached
  private def maxRetryCheck(currentRetryCount: Long, maxRetryCount: Long) (implicit system: ActorSystem): Unit = {
    if (currentRetryCount > maxRetryCount) {
      log.error(s"Max retries ${currentRetryCount} reached. Exiting application")
      //Terminate the actor system before terminating the current JVM otherwise 'System.exit' may not work.
      system.terminate()
      System.exit(1)
    }
  }
}