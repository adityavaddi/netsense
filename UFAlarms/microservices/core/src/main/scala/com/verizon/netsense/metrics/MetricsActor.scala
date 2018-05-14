package com.verizon.netsense.metrics

import akka.actor.Actor

/**
 * Created by davor on 5/5/17.
 */
trait MetricsActor extends Actor with Instrumented {
  private[this] val loading = metrics.timer("message-actor")
  object ReceiveMetrics {
    def apply(receive: Receive): Receive = {
      case value =>
        loading.time {
          receive(value)
        }
    }
  }

}
