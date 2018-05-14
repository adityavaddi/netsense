package com.vz.ns.ts.service.service

import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.unmarshalling.Unmarshal
import com.vz.ns.ts.service.config.ConfigLoader._
import com.vz.ns.ts.service.model.TsAccount
import com.vz.ns.ts.service.util.{AkkaRuntime, HttpRequestService}
import de.heikoseeberger.akkahttpjackson.JacksonSupport._
import org.apache.oltu.oauth2.client.{OAuthClient, URLConnectionClient}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future, TimeoutException}
import scala.util.parsing.json.JSONObject

class TsAccountService(tsTokenUtil: TsTokenService, httpRequestService: HttpRequestService, akkaRuntime: AkkaRuntime)
    extends Logging {

  import akkaRuntime._

  def createTsAccount = {
    log.debug("Creating TS Account super method")
    val userTokenValue = tsTokenUtil.userToken()
    userTokenValue.getOrElse({
      createAccount
      Thread.sleep(2000) // Giving time for TS to sink up
    })
  }

  def createAccount = {
    log.debug("Creating TS Account")
    val tsAccountPayload =
      Map("email" -> tsAccountUserName, "password" -> tsAccountPswd, "noEmailVerification" -> true)

    val response: Future[HttpResponse] = httpRequestService.makeRequest(
      HttpRequest(method = HttpMethods.POST, uri = s"${tsNorthApi}/api/v2/accounts")
        .withHeaders(RawHeader("Authorization", s"Bearer ${tsTokenUtil.clientToken().get}"))
        .withEntity(ContentTypes.`application/json`, JSONObject(tsAccountPayload).toString())
    )
    response.foreach(r => log.debug("Response from POST account: " + r.entity))
  }

  def getAccountFromTs: Future[HttpResponse] = {
    log.debug("Get account from TS")
    val response: Future[HttpResponse] = httpRequestService.makeRequest(
      HttpRequest(method = HttpMethods.GET, uri = s"${tsNorthApi}/api/v2/accounts/me")
        .withHeaders(RawHeader("Authorization", s"Bearer ${tsTokenUtil.userToken().get}"))
    )
    response.foreach(r => log.debug("Response from GET account: " + r.entity))
    response
  }

  def getAccountIdFromAccount(): Future[String] = {
    log.debug("Get account ID from account")
    val map: Future[String] = getAccountFromTs.flatMap(x => {
      val tsaccount = Unmarshal(x.entity).to[TsAccount]
      tsaccount.map(x => {
        log.debug("Account Id From getAccountIdFromAccount: " + x.id.getOrElse("No Account Id Found"))
        x.id.getOrElse(null)
      })
    })
    map
  }

  def getAccountId(): Option[String] = {
    log.debug("Get account ID")
    try {
      Some(Await.result(getAccountIdFromAccount, 10 second))
    } catch {
      case e: TimeoutException => {
        log.error("Timeout Exception: Failed to fetch accountId from TS");
        Some(null)
      }
    }
  }
}

object TsAccountService {
  val oAuthClient = new OAuthClient(new URLConnectionClient)
  def apply(tsTokenUtil: TsTokenService,
            httpRequestService: HttpRequestService,
            akkaRuntime: AkkaRuntime): TsAccountService =
    new TsAccountService(tsTokenUtil, httpRequestService, akkaRuntime)
}
