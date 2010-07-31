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

import java.net.URI
import javax.crypto.Mac
import javax.crypto.SecretKey
import javax.crypto.spec.SecretKeySpec
import net.liftweb.http._
import net.liftweb.common._
import net.liftweb.util.Helpers

abstract class OAuthSignatureMethod(accessor: OAuthAccessor) {
  def validate(message: OAuthMessage): Box[OAuthMessage] =
  for {
    signature <- message.getSignature
    sigMethod <- message.getSignatureMethod
    baseString = getBaseString(message)
    bs2 <- Full(baseString).filter(bs => isValid(signature.value, bs)) ?~
    OAuthUtil.Problems.SIGNATURE_INVALID._1 ~>
    OAuthProblem(OAuthUtil.Problems.SIGNATURE_INVALID, ("oauth_signature", signature.value) ::
      ("oauth_signature_base_string", baseString) ::
      ("oauth_signature_method", sigMethod.value) :: Nil)
  } yield message

  def getBaseString(message: OAuthMessage): String =
  OAuthUtil.percentEncode(message.method.method ) + "&" +
  OAuthUtil.percentEncode(normalizeUrl(message.uri)) + "&" +
  OAuthUtil.percentEncode(normalizeParameters(message.parameters))


  private[oauth] def normalizeUrl(url: String) = {
    val uri = new URI(url);
    val scheme = uri.getScheme().toLowerCase()
    var authority = uri.getAuthority().toLowerCase()
    val dropPort = (scheme.equals("http") && uri.getPort() == 80) || (scheme.equals("https") && uri.getPort() == 443)
    if (dropPort) {
      // find the last : in the authority
      val index = authority.lastIndexOf(":")
      if (index >= 0) {
        authority = authority.substring(0, index)
      }
    }
    var path = uri.getRawPath()
    if (path == null || path.length() <= 0) {
      path = "/" // conforms to RFC 2616 section 3.2.2
    }
    // we know that there is no query and no fragment here.
    scheme + "://" + authority + path
  }

  private def normalizeParameters(parameters: List[OAuthUtil.Parameter]) = {
    val filteredParameters = parameters.filter(_.name != OAuthUtil.OAUTH_SIGNATURE)
    val sortedParameters = filteredParameters.sort((p1, p2) => {
        val k1 = OAuthUtil.percentEncode(p1.name) + ' ' + OAuthUtil.percentEncode(p1.value)
        val k2 = OAuthUtil.percentEncode(p2.name) + ' ' + OAuthUtil.percentEncode(p2.value)
        k1.compareTo(k2) <= 0
      })
    OAuthUtil.formEncode(sortedParameters)
  }

  def getConsumerSecret: String = accessor.consumerSecret
  def getTokenSecret: Box[String] = accessor.tokenSecret

  def isValid(signature: String, baseString: String): Boolean
  def getSignature(baseString: String): Box[String]
}

object OAuthSignatureMethod {
  val SIGNATURE_METHODS = Map("HMAC-SHA1" -> HMAC_SHA1,
                              "PLAINTEXT" -> PLAINTEXT)

  def newSigner(message: OAuthMessage, accessor: OAuthAccessor): Box[OAuthSignatureMethod] = {
    for {
      sigMeth <- message.getSignatureMethod
      meth <- Box(SIGNATURE_METHODS.get(sigMeth.value.toUpperCase)) ?~
      OAuthUtil.Problems.SIGNATURE_METHOD_REJECTED._1 ~>
      OAuthProblem(OAuthUtil.Problems.SIGNATURE_METHOD_REJECTED, (OAuthUtil.ProblemParams.OAUTH_ACCEPTABLE_SIGNATURE_METHODS,
                                                                  OAuthUtil.percentEncode(SIGNATURE_METHODS.keySet.toList)))
    } yield meth(accessor)
  }
}

trait OAuthSignatureMethodBuilder {
  def apply(accessor: OAuthAccessor): OAuthSignatureMethod
}

// HMAC_SHA1 Signature Generator

class HMAC_SHA1(accessor: OAuthAccessor) extends OAuthSignatureMethod(accessor) {
  private val ENCODING = OAuthUtil.ENCODING
  private val MAC_NAME = "HmacSHA1"
  override def isValid(signature: String, baseString: String) = {
    Thread.sleep(Helpers.randomLong(10)) // Avoid a timing attack
    Helpers.secureEquals(getSignature(baseString) openOr 
                         signature.reverse.toString, signature)
  }
  override def getSignature(baseString: String) = for {
    cs <- computeSignature(baseString)
  } yield Helpers.base64Encode(cs)

  def computeSignature(baseString: String): Box[Array[Byte]] =
  for {
    ts <- getTokenSecret
  }   yield {
    val keyString = OAuthUtil.percentEncode(getConsumerSecret) + '&' + OAuthUtil.percentEncode(ts)
    val keyBytes = keyString.getBytes(ENCODING)
    val key = new SecretKeySpec(keyBytes, MAC_NAME)
    val mac = Mac.getInstance(MAC_NAME)
    mac.init(key)
    val text = baseString.getBytes(ENCODING)
    mac.doFinal(text)
  }
}

object HMAC_SHA1 extends OAuthSignatureMethodBuilder {
  def apply(accessor: OAuthAccessor): OAuthSignatureMethod = new HMAC_SHA1(accessor)
}

  
// Plaintext Signature Generator

class PLAINTEXT(accessor: OAuthAccessor) extends OAuthSignatureMethod(accessor) {
  override def isValid(signature: String, baseString: String) = {
    Thread.sleep(Helpers.randomLong(10)) // Avoid a timing attack
    Helpers.secureEquals(getSignature(baseString) openOr
                         signature.reverse.toString, signature)
  }

  override def getSignature(baseString: String): Box[String] =
  for {
    ts <- getTokenSecret
  } yield OAuthUtil.percentEncode(getConsumerSecret) + '&' + OAuthUtil.percentEncode(ts)
}

object PLAINTEXT extends OAuthSignatureMethodBuilder {
  def apply(accessor: OAuthAccessor): OAuthSignatureMethod = new PLAINTEXT(accessor)
}

}
}
