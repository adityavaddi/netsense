package com.verizon.netsense.services

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.Http.ServerBinding
import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import com.codahale.metrics.health.HealthCheck.Result
import com.verizon.netsense.metrics.Metrics.healthCheckRegistry
import com.verizon.netsense.utils.ConfigLoader.{healthCheckHost, healthCheckPort}
import org.codehaus.jackson.map.ObjectMapper
import org.codehaus.jackson.map.annotate.JsonSerialize

import scala.concurrent.Future

object HealthCheckService {

  private val objectMapper = new ObjectMapper().setSerializationInclusion(JsonSerialize.Inclusion.NON_NULL)

  healthCheckRegistry.register("appStatus", () => Result.healthy())

  val route =
    get {
      path("status") {
        val healthChecks = healthCheckRegistry.runHealthChecks()
        complete(HttpEntity(ContentTypes.`application/json`, objectMapper.writeValueAsString(healthChecks)))
      }
    }

  def bindHttp()(implicit sys: ActorSystem, mat: ActorMaterializer): Future[ServerBinding] =
    Http().bindAndHandle(route, healthCheckHost, healthCheckPort)
}
