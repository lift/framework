/*
 * Copyright 2010 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.liftweb {
package oauth {

import java.net.{URLDecoder, URLEncoder}
import net.liftweb.common._
import net.liftweb.http._
import scala.xml._

trait OAuth {
  def oathPathPrefix: List[String]

  import OAuthUtil._

  lazy val dispatch: LiftRules.DispatchPF = {
    val AccessToken = oathPathPrefix ::: List("access_token")
    val RequestToken = oathPathPrefix ::: List("request_token")
    val AuthExchange = oathPathPrefix ::: List("auth_exchange")

    {
      case req@Req(AccessToken, "", _) => () => accessToken(req)
      case req@Req(RequestToken, "", _) => () => requestToken(req)
      case req@Req(AuthExchange, "", _) => () => authExchange(req)
    }
  }

  protected def oauthConsumerMeta: OAuthConsumerMeta
  protected def oauthAccessorMeta: OAuthAccessorMeta
  protected def oauthValidator: OAuthValidator
  protected def oauthTokenMeta: OAuthTokenMeta
  protected def oauthUserMeta: OAuthUserMeta

  protected def getConsumer(requestMessage: OAuthMessage): Box[OAuthConsumer] =
  for {
    key <- requestMessage.getConsumerKey ?~ OAuthUtil.Problems.PARAMETER_ABSENT._1 ~> OAuthProblem(OAuthUtil.Problems.PARAMETER_ABSENT, OAuthUtil.OAUTH_CONSUMER_KEY)
    consumer <- oauthConsumerMeta.find(key.name) ?~ "Token Rejected" ~> OAuthProblem(OAuthUtil.Problems.TOKEN_REJECTED)
  } yield consumer

  def MAX_TTL = new java.util.Date(3000000000000L)

  def accessToken(req: Req): Box[LiftResponse] = {
    val requestMessage = new HttpRequestMessage(req)
    val ret: Box[LiftResponse] =
    for {
      accessor <- oauthAccessorMeta.fromAuthorizedRequestToken(requestMessage)
      validMessage <- oauthValidator.validateMessage(requestMessage, accessor)
    } yield {



      // delete used request token
      oauthTokenMeta.bulkDelete_!!(requestMessage.getToken)

      // create access token
      val token = oauthTokenMeta.create(accessor.oauthConsumer, accessor.user, "access", 1, MAX_TTL)

      val response = OAuthUtil.formEncode(List(OAuthUtil.Parameter(OAuthUtil.OAUTH_TOKEN, token.token), OAuthUtil.Parameter(OAuthUtil.OAUTH_TOKEN_SECRET, token.secret)))
      PlainTextResponse(response, Nil, 200)
    }

    handleFailure(ret)
  }

  protected def handleFailure(in: Box[LiftResponse]): Box[LiftResponse] = in

  def requestToken(req: Req): Box[LiftResponse] = {
    val requestMessage = new HttpRequestMessage(req)
    handleFailure(
      for {
        consumer <- getConsumer(requestMessage)
        accessor <- oauthAccessorMeta(consumer)
        validMessage <- oauthValidator.validateMessage(requestMessage, accessor)
      } yield {


        val token = oauthTokenMeta.create(consumer, consumer.user, "request")

        val response = OAuthUtil.formEncode(List(Parameter(OAUTH_TOKEN, token.token), Parameter(OAUTH_TOKEN_SECRET, token.secret)))
        PlainTextResponse(response, Nil, 200)
      })
  }

  // case class XmlCodeResponse(override val xml: Node, override val code: Int) extends XmlResponse(xml)

  def authExchange(req: Req): Box[LiftResponse] = {
    val requestMessage = new HttpRequestMessage(req)
    handleFailure(for {
        consumer <- getConsumer(requestMessage)
        accessor <- oauthAccessorMeta(consumer)
        validMessage <- oauthValidator.validateMessage(requestMessage, accessor)
        username <- requestMessage.getParameter("fs_username")
        password <- requestMessage.getParameter("fs_password")
        user <- oauthUserMeta.findByUsernameAndPassword(username.value, password.value)
      } yield {
        val token = oauthTokenMeta.create(accessor.oauthConsumer, Full(user), "access", 1, MAX_TTL)
        XmlResponse(Utility.trim(<credentials>
              <oauth_token>{token.token}</oauth_token>
              <oauth_token_secret>{token.secret}</oauth_token_secret>
                                 </credentials>))
      })
  }

  /*
   def handleException(e: OAuthProblemException) = {
   Full(PlainTextResponse(e.toString, Nil, e.httpCode))
   }
   */
}

object OAuthUtil {
  val VERSION_1_0 = "1.0"

  val ENCODING = "UTF-8"

  val OAUTH_CONSUMER_KEY = "oauth_consumer_key"
  val OAUTH_TOKEN = "oauth_token"
  val OAUTH_TOKEN_SECRET = "oauth_token_secret"
  val OAUTH_SIGNATURE_METHOD = "oauth_signature_method"
  val OAUTH_SIGNATURE = "oauth_signature"
  val OAUTH_TIMESTAMP = "oauth_timestamp"
  val OAUTH_NONCE = "oauth_nonce"
  val OAUTH_VERSION = "oauth_version"
  val OAUTH_CALLBACK = "oauth_callback"
  val OAUTH_CALLBACK_CONFIRMED = "oauth_callback_confirmed"
  val OAUTH_VERIFIER = "oauth_verifier"

  val HMAC_SHA1 = "HMAC-SHA1"
  val RSA_SHA1 = "RSA-SHA1"

  object Problems {
    val BAD_REQUEST = 400
    val UNAUTHORIZED = 401

    val VERSION_REJECTED = ("version_rejected", BAD_REQUEST)
    val PARAMETER_ABSENT = ("parameter_absent", BAD_REQUEST)
    val PARAMETER_REJECTED = ("parameter_rejected", BAD_REQUEST)
    val TIMESTAMP_REFUSED = ("timestamp_refused", BAD_REQUEST)
    val NONCE_USED = ("nonce_used", UNAUTHORIZED)
    val SIGNATURE_METHOD_REJECTED = ("signature_method_rejected", BAD_REQUEST)
    val SIGNATURE_INVALID = ("signature_invalid", UNAUTHORIZED)
    val TOKEN_EXPIRED = ("token_expired", UNAUTHORIZED)
    val TOKEN_REJECTED = ("token_rejected", UNAUTHORIZED)
    val PERMISSION_DENIED = ("permission_denined", UNAUTHORIZED)
  }

  object ProblemParams {
    val OAUTH_ACCEPTABLE_VERSIONS = "oauth_acceptable_versions"
    val OAUTH_ACCEPTABLE_TIMESTAMPS = "oauth_acceptable_timestamps"
    val OAUTH_PARAMETERS_ABSENT = "oauth_parameters_absent"
    val OAUTH_PARAMETERS_REJECTED = "oauth_parameters_rejected"
    val OAUTH_ACCEPTABLE_SIGNATURE_METHODS = "oauth_acceptable_signature_methods"
  }

  def decodePercent(s: String) = URLDecoder.decode(s, ENCODING)

  def formEncode(parameters: List[OAuthUtil.Parameter]) = {
    parameters.map(p => percentEncode(p.name)+"="+percentEncode(p.value)).mkString("&")
  }

  def percentEncode(s: String): String = {
    if (s == null) "" else {
      URLEncoder.encode(s, ENCODING).replace("+", "%20").replace("*", "%2A").replace("%7E", "~")
    }
  }

  def percentEncode(strings: Seq[String]): String = {
    strings.map(percentEncode(_)).mkString("&")
  }

  final case class Parameter(name: String, value: String)
}

}
}
