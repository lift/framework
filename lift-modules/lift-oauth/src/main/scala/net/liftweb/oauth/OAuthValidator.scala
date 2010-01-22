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

import net.liftweb.common.Full
import net.liftweb.util._
import net.liftweb.common._
import Helpers._

trait OAuthValidator {
  val MIN_VERSION = 1.0
  val MAX_VERSION = 1.0
  val MAX_TIMESTAMP_AGE_MSEC = 5 * 60 * 1000

  val SINGLE_PARAMETERS = Set(OAuthUtil.OAUTH_CONSUMER_KEY, OAuthUtil.OAUTH_TOKEN, OAuthUtil.OAUTH_TOKEN_SECRET,
                              OAuthUtil.OAUTH_CALLBACK, OAuthUtil.OAUTH_SIGNATURE_METHOD, OAuthUtil.OAUTH_SIGNATURE,
                              OAuthUtil.OAUTH_TIMESTAMP, OAuthUtil.OAUTH_NONCE, OAuthUtil.OAUTH_VERSION)

  protected def oauthNonceMeta: OAuthNonceMeta


  def validateMessage(message: OAuthMessage, accessor: OAuthAccessor): Box[OAuthMessage] =
  (checkSingleParameters _ andThen validateVersion _ andThen validateTimestampAndNonce _
   andThen validateSignature(accessor) )(Full(message))

  private def checkSingleParameters(message: Box[OAuthMessage]): Box[OAuthMessage] = {
    message.flatMap(msg => msg.parameters.foldLeft[Map[String, List[String]]](Map()) {
        case (map, next) => map + (next.name -> (next.value :: map.getOrElse(next.name, Nil)))
      }.filter{ case (name, value) => value.length < 2}.toList match {
        case Nil => message
        case xs =>

          ParamFailure(OAuthUtil.Problems.PARAMETER_REJECTED._1, Empty, Empty ,
                       OAuthProblem(OAuthUtil.Problems.PARAMETER_REJECTED, for {
                (name, values) <- xs
                value <- values
              } yield (name -> value)))
      }
    )}



  private def validateVersion(message: Box[OAuthMessage]): Box[OAuthMessage] =
  for {
    msg <- message
    verParam <- msg.getParameter(OAuthUtil.OAUTH_VERSION)
    version <- tryo(verParam.value.toDouble).filter(v => v < MIN_VERSION || MAX_VERSION < v) ?~
    OAuthUtil.Problems.VERSION_REJECTED._1 ~> OAuthProblem(OAuthUtil.Problems.VERSION_REJECTED,
                                                           (OAuthUtil.ProblemParams.OAUTH_ACCEPTABLE_VERSIONS, MIN_VERSION + "-" + MAX_VERSION))
  } yield msg


  private def validateTimestampAndNonce(message: Box[OAuthMessage]): Box[OAuthMessage] =
  for {
    msg <- message
    timeStampStr <- msg.getParameter(OAuthUtil.OAUTH_TIMESTAMP)
    timestamp <- validateTimestamp(timeStampStr)
    msg2 <- validateNonce(msg, timestamp)
  } yield msg2


  private def validateTimestamp(timestampStr: OAuthUtil.Parameter): Box[Long] = {
    val currentTimeMsec = millis
    val min = (currentTimeMsec - MAX_TIMESTAMP_AGE_MSEC + 500) / 1000L
    val max = (currentTimeMsec + MAX_TIMESTAMP_AGE_MSEC + 500) / 1000L
    for {
      timestamp <- Helpers.asLong(timestampStr.value) ?~ OAuthUtil.Problems.PARAMETER_REJECTED._1 ~>
      OAuthProblem(OAuthUtil.Problems.PARAMETER_REJECTED, (OAuthUtil.ProblemParams.OAUTH_PARAMETERS_REJECTED, OAuthUtil.formEncode(List(timestampStr))))
      ts2 <- Full(timestamp).filter(ts => timestamp >= min || max >= timestamp) ?~
      OAuthUtil.Problems.TIMESTAMP_REFUSED._1 ~>
      OAuthProblem(OAuthUtil.Problems.TIMESTAMP_REFUSED,
                   (OAuthUtil.ProblemParams.OAUTH_ACCEPTABLE_TIMESTAMPS, min + "-" + max))
    } yield timestamp
  }

  private def validateNonce(message: OAuthMessage, timestamp: Long): Box[OAuthMessage] =
  for {
    consumerKey <- message.getConsumerKey
    token = message.getToken.map(_.value).openOr("")
    nonce <- message.getParameter(OAuthUtil.OAUTH_NONCE)

    newNonce <- oauthNonceMeta.find(consumerKey.value, token, timestamp, nonce.value) match {
      case Full(nonce) => ParamFailure(OAuthUtil.Problems.NONCE_USED._1, Empty, Empty, OAuthProblem(OAuthUtil.Problems.NONCE_USED))
      case _ =>
        Full(oauthNonceMeta.create(consumerKey.value, token, timestamp, nonce.value))
    }
    } yield {

    val minTimestamp = (millis - MAX_TIMESTAMP_AGE_MSEC + 500) / 1000
    oauthNonceMeta.bulkDelete_!!(minTimestamp)
    message
    }


  private def validateSignature(accessor: OAuthAccessor)(message: Box[OAuthMessage]): Box[OAuthMessage] =
  for {
    msg <- message
    one <- msg.getParameter(OAuthUtil.OAUTH_CONSUMER_KEY)
    two <- msg.getParameter(OAuthUtil.OAUTH_SIGNATURE_METHOD)
    three <- msg.getParameter(OAuthUtil.OAUTH_SIGNATURE)
    // message.requireParameters(OAuth.OAUTH_CONSUMER_KEY, OAuth.OAUTH_SIGNATURE_METHOD, OAuth.OAUTH_SIGNATURE)
    validator <- OAuthSignatureMethod.newSigner(msg, accessor)
    res <- validator.validate(msg)
  } yield msg

}

}
}
