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

import java.util.regex.Pattern
import net.liftweb.http.{Req, RequestType}
import net.liftweb.common._

class OAuthMessage(val method: RequestType, val uri: String, val parameters: List[OAuthUtil.Parameter]) {
  def getParameter(name: String): Box[OAuthUtil.Parameter] = Box(parameters.find(_.name == name)) ?~
  OAuthUtil.Problems.PARAMETER_ABSENT._1 ~>
  OAuthProblem(OAuthUtil.Problems.PARAMETER_ABSENT, OAuthUtil.ProblemParams.OAUTH_PARAMETERS_ABSENT -> OAuthUtil.percentEncode(name))

  def getParameters(name: String): List[OAuthUtil.Parameter] = parameters.filter(_.name == name)

  def getConsumerKey = getParameter(OAuthUtil.OAUTH_CONSUMER_KEY)
  def getToken = getParameter(OAuthUtil.OAUTH_TOKEN)
  def getSignatureMethod = getParameter(OAuthUtil.OAUTH_SIGNATURE_METHOD)
  def getSignature = getParameter(OAuthUtil.OAUTH_SIGNATURE)
}

object OAuthMessage {
  val AUTH_SCHEME = "OAuth"
  val AUTHORIZATION = Pattern.compile("\\s*(\\w*)\\s+(.*)")
  val NVP = Pattern.compile("(\\S*)\\s*\\=\\s*\"([^\"]*)\"")

  def decodeAuthorization(authorization: String): List[OAuthUtil.Parameter] =
  for {
    auth <- (Box !! authorization).toList
    matcher <- List(AUTHORIZATION.matcher(authorization)) if (matcher.matches() &&
                                                              AUTH_SCHEME.equalsIgnoreCase(matcher.group(1)))
    nvp <- matcher.group(2).split("\\s*,\\s*")
    m2 <- List(NVP.matcher(nvp)) if m2.matches()
  } yield new OAuthUtil.Parameter(OAuthUtil.decodePercent(m2.group(1)),
                                  OAuthUtil.decodePercent(m2.group(2)))
}

class HttpRequestMessage(req: Req) extends OAuthMessage(req.requestType,
                                                        HttpRequestMessage.computeUrl(req),
                                                        HttpRequestMessage.getParameters(req))

object HttpRequestMessage {
  def computeUrl(req: Req) = {
    val request = req.request
    request.scheme+"://"+request.serverName+":"+request.serverPort+request.uri
  }


  def getParameters(req: Req): List[OAuthUtil.Parameter] = {
    (for {
        header <- req.headers("Authorization").toList
        parameter <- OAuthMessage.decodeAuthorization(header) if !"realm".equalsIgnoreCase(parameter.name)
      } yield parameter) :::
    (for {
        entry <- req.params.toList
        name = entry._1
        value <- entry._2
      } yield {
        new OAuthUtil.Parameter(name, value)
      })

  }
}

}
}
