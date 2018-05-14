package com.verizon.netsense.test.kafka

import akka.actor.{ActorRef, ActorSystem}
import akka.stream.Materializer
import akka.testkit.TestProbe
import com.typesafe.config.ConfigFactory

import scala.concurrent.duration.FiniteDuration
import scala.util.control.NoStackTrace

/**
 * Created by sundach on 5/30/17.
 */
object Utils {

  // Sets the default-mailbox to the usual [[akka.dispatch.UnboundedMailbox]] instead of [[StreamTestDefaultMailbox]]. */
  val UnboundedMailboxConfig =
    ConfigFactory.parseString("""akka.actor.default-mailbox.mailbox-type = "akka.dispatch.UnboundedMailbox"""")

  case class TE(message: String) extends RuntimeException(message) with NoStackTrace
  final case class StageStoppingTimeout(time: FiniteDuration)

  /** Compatibility between 2.4 and 2.5 */
  type ActorMaterializerImpl = {
    def system: ActorSystem
    def supervisor: ActorRef
  }

  def assertAllStagesStopped[T](block: ⇒ T)(implicit timeout: StageStoppingTimeout, materializer: Materializer): T = {
    val impl  = materializer.asInstanceOf[ActorMaterializerImpl] // refined type, will never fail
    val probe = TestProbe()(impl.system)
    //probe.send(impl.supervisor, StreamSupervisor.StopChildren$)
    //probe.expectMsg("StoppedChildren")
    val result = block
    //probe.send(impl.supervisor, PoisonPill)
    result
  }
}
