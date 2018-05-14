package com.vz.ns.ts.service.service

import akka.actor.ActorSystem
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{HttpMethods, HttpRequest, HttpResponse, StatusCodes}
import com.vz.ns.ts.service.util.{AkkaRuntime, HttpRequestService}
import org.apache.oltu.oauth2.client.OAuthClient
import org.apache.oltu.oauth2.client.request.OAuthClientRequest
import org.apache.oltu.oauth2.client.response.OAuthJSONAccessTokenResponse
import org.mockito.ArgumentCaptor.forClass
import org.mockito.ArgumentMatchers
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito._
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{FunSuite, Matchers}

import scala.concurrent.Future.successful

class TsAccountServiceTest extends FunSuite with Matchers with MockitoSugar {

  test("should create account in TS") {
    val httpRequestService = mock[HttpRequestService]
    val runtime            = AkkaRuntime(ActorSystem.create("ts-adapter-test"))
    val argumentCaptor     = forClass(classOf[HttpRequest])

    val mockOAuthClient   = mock[OAuthClient]
    val mockOAuthResponse = mock[OAuthJSONAccessTokenResponse]
    when(mockOAuthClient.accessToken(ArgumentMatchers.any(classOf[OAuthClientRequest]))).thenReturn(mockOAuthResponse)
    when(mockOAuthResponse.getAccessToken).thenReturn("some token hello")

    val tsTokenService = new TsTokenService(mockOAuthClient)

    val tsAccountService = TsAccountService(tsTokenService, httpRequestService, runtime)

    when(httpRequestService.makeRequest(any(classOf[HttpRequest])))
      .thenReturn(successful(HttpResponse(StatusCodes.OK)))

    tsAccountService.createAccount

    verify(httpRequestService).makeRequest(argumentCaptor.capture())
    val httpRequestParams: HttpRequest = argumentCaptor.getValue

    (httpRequestParams.method) shouldEqual HttpMethods.POST
    (httpRequestParams.uri.path.toString()) shouldEqual "/api/v2/accounts"
    var rawHeader = RawHeader("Authorization", "Bearer some token hello")
    val auth      = List(rawHeader)
    (httpRequestParams.headers.toList) shouldEqual auth
    val result =
      "{\"email\" : \"sensity.user76@one.verizon.com\", \"password\" : \"helloWorld@1989\", \"noEmailVerification\" : true}"
    (httpRequestParams.entity.toString) shouldEqual s"HttpEntity.Strict(application/json,$result)"
  }

  test("should get account from TS") {
    val httpRequestService = mock[HttpRequestService]
    val runtime            = AkkaRuntime(ActorSystem.create("ts-adapter-test"))
    val argumentCaptor     = forClass(classOf[HttpRequest])

    val mockOAuthClient   = mock[OAuthClient]
    val mockOAuthResponse = mock[OAuthJSONAccessTokenResponse]
    when(mockOAuthClient.accessToken(ArgumentMatchers.any(classOf[OAuthClientRequest]))).thenReturn(mockOAuthResponse)
    when(mockOAuthResponse.getAccessToken).thenReturn("some token hello")

    val tsTokenService = new TsTokenService(mockOAuthClient)

    val tsAccountService = TsAccountService(tsTokenService, httpRequestService, runtime)

    when(httpRequestService.makeRequest(any(classOf[HttpRequest])))
      .thenReturn(successful(HttpResponse(StatusCodes.OK)))

    tsAccountService.getAccountFromTs

    verify(httpRequestService).makeRequest(argumentCaptor.capture())
    val httpRequestParams: HttpRequest = argumentCaptor.getValue

    (httpRequestParams.method) shouldEqual HttpMethods.GET
    (httpRequestParams.uri.path.toString()) shouldEqual "/api/v2/accounts/me"
    var rawHeader = RawHeader("Authorization", "Bearer some token hello")
    val auth      = List(rawHeader)
    (httpRequestParams.headers.toList) shouldEqual auth
  }
}
