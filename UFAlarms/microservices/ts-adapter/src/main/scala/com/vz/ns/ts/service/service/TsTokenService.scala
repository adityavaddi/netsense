package com.vz.ns.ts.service.service

import com.vz.ns.ts.service.config.ConfigLoader._
import org.apache.oltu.oauth2.client.request.OAuthClientRequest
import org.apache.oltu.oauth2.client.response.OAuthJSONAccessTokenResponse
import org.apache.oltu.oauth2.client.{OAuthClient, URLConnectionClient}
import org.apache.oltu.oauth2.common.exception.OAuthProblemException
import org.apache.oltu.oauth2.common.message.types.GrantType

class TsTokenService(oAuthClient: OAuthClient) extends Logging {

  def clientToken() = {
    log.debug("Getting client token")
    log.debug("API USED:" + tsNorthApi + "/oauth2/token")

    val request: OAuthClientRequest =
      OAuthClientRequest
        .tokenLocation(s"${tsNorthApi}/oauth2/token")
        .setGrantType(GrantType.CLIENT_CREDENTIALS)
        .setClientId(clientId)
        .setClientSecret(clientSecret)
        .setScope(scope)
        .buildBodyMessage

    val token: Option[String] = try {
      val token1: OAuthJSONAccessTokenResponse = oAuthClient.accessToken(request)
      Some(token1.getAccessToken)
    } catch {
      case ex: OAuthProblemException => {
        None
      }
      case _: Throwable => {
        None
      }
    }
    log.debug("Response from clientToken: " + token.getOrElse("UNABLE TO GET CLIENT TOKEN"))
    token
  }

  def userToken(): Option[String] = {
    log.debug("Getting user token")
    log.debug("API USED:" + tsNorthApi + "/oauth2/token")

    val clientRequest = OAuthClientRequest
      .tokenLocation(s"${tsNorthApi}/oauth2/token")
      .setGrantType(GrantType.PASSWORD)
      .setClientId(clientId)
      .setClientSecret(clientSecret)
      .setScope(tsAccountScope)
      .setUsername(tsAccountUserName)
      .setPassword(tsAccountPswd)
      .buildBodyMessage

    val token: Option[String] = try {
      Some(oAuthClient.accessToken(clientRequest).getAccessToken)
    } catch {
      case ex: OAuthProblemException => {
        None
      }
      case _: Throwable => {
        None
      }
    }
    token.foreach(t => log.debug("Response from userToken: " + t))
    token
  }

}

object TsTokenService {
  val oAuthClient             = new OAuthClient(new URLConnectionClient)
  def apply(): TsTokenService = new TsTokenService(oAuthClient)
}
