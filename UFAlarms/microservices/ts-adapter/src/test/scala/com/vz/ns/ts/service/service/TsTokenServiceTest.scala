package com.vz.ns.ts.service.service

import org.apache.oltu.oauth2.client.OAuthClient
import org.apache.oltu.oauth2.client.request.OAuthClientRequest
import org.apache.oltu.oauth2.client.response.OAuthJSONAccessTokenResponse
import org.mockito.Mockito._
import org.mockito.{ArgumentCaptor, ArgumentMatchers}
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{FunSuite, Matchers}

class TsTokenServiceTest extends FunSuite with Matchers with MockitoSugar {

  test("should get the client token") {
    val mockOAuthClient                                    = mock[OAuthClient]
    val mockOAuthResponse                                  = mock[OAuthJSONAccessTokenResponse]
    val argumentCaptor: ArgumentCaptor[OAuthClientRequest] = ArgumentCaptor.forClass(classOf[OAuthClientRequest])

    when(mockOAuthClient.accessToken(ArgumentMatchers.any(classOf[OAuthClientRequest]))).thenReturn(mockOAuthResponse)
    when(mockOAuthResponse.getAccessToken).thenReturn("some token hello")

    val tsTokenService        = new TsTokenService(mockOAuthClient)
    val token: Option[String] = tsTokenService.clientToken

    token shouldEqual Some("some token hello")

    verify(mockOAuthClient).accessToken(argumentCaptor.capture())
    val aAuthRequest: OAuthClientRequest = argumentCaptor.getValue
    aAuthRequest.getLocationUri shouldEqual "http://localhost/oauth2/token"
  }

  test("should fetch the user token") {
    val mockOAuthClient                                    = mock[OAuthClient]
    val mockOAuthResponse                                  = mock[OAuthJSONAccessTokenResponse]
    val argumentCaptor: ArgumentCaptor[OAuthClientRequest] = ArgumentCaptor.forClass(classOf[OAuthClientRequest])

    when(mockOAuthClient.accessToken(ArgumentMatchers.any(classOf[OAuthClientRequest]))).thenReturn(mockOAuthResponse)
    when(mockOAuthResponse.getAccessToken).thenReturn("some token hello")

    val tsTokenUtil           = new TsTokenService(mockOAuthClient)
    val token: Option[String] = tsTokenUtil.userToken

    token shouldEqual Some("some token hello")

    verify(mockOAuthClient).accessToken(argumentCaptor.capture())
    val aAuthRequest: OAuthClientRequest = argumentCaptor.getValue
    aAuthRequest.getLocationUri shouldEqual "http://localhost/oauth2/token"
  }
}
