/*
 * Copyright 2008-2010 WorldWide Conferencing, LLC
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
package openid {

import _root_.org.openid4java.discovery.Identifier;
import _root_.org.openid4java.discovery.DiscoveryInformation;
import _root_.org.openid4java.message.ax.FetchRequest;
import _root_.org.openid4java.message.ax.FetchResponse;
import _root_.org.openid4java.message.ax.AxMessage;
import _root_.org.openid4java.message._
import _root_.org.openid4java.OpenIDException;
import _root_.org.openid4java.consumer._

import _root_.java.util.List;
import _root_.java.io.IOException;

import _root_.net.liftweb._
import http._
import provider._
import util._
import common._

import _root_.scala.xml.{NodeSeq, Text}

trait OpenIDVendor {
  type UserType

  type ConsumerType <: OpenIDConsumer[UserType]

  private object RedirectBackTo extends SessionVar[Box[String]](Empty)
  lazy val PathRoot = "openid"

  lazy val LoginPath = "login"

  lazy val LogOutPath = "logout"

  lazy val ResponsePath = "response"

  def PostParamName = "openid_identifier" // "openIdUrl"

  lazy val SnippetPrefix = "openId"

  def postLogin(id: Box[Identifier],res: VerificationResult): Unit

  def postUrl = "/"+ PathRoot + "/" + LoginPath

  /**
   * A session var that keeps track of the OpenID object through the request/response
   */
  object OpenIDObject extends SessionVar[ConsumerType](createAConsumer)

  def createAConsumer: ConsumerType

  def currentUser: Box[UserType]

  def snippetPF: LiftRules.SnippetPF = NamedPF ("OpenID Default") {
    case SnippetPrefix :: "ifLoggedIn" :: Nil => showIfLoggedIn
    case SnippetPrefix :: "ifLoggedOut" :: Nil => showIfLoggedOut
    case SnippetPrefix :: "userBox" :: Nil => showUserBox
  }

  def displayUser(id: UserType): NodeSeq

  def logoutLink: NodeSeq = <xml:group> <a href={"/"+PathRoot+"/"+LogOutPath}>Log Out</a></xml:group>

  def loginForm: NodeSeq = <form method="post" action={"/"+PathRoot+"/"+LoginPath}>
    OpenID <input class="openidfield" name={PostParamName}/> <input type='submit' value="Log In"/>
                           </form>

  def showUserBox(ignore: NodeSeq): NodeSeq = <div class="openidbox">{
      currentUser match {
        case Full(user) => displayUser(user) ++ logoutLink
        case _ => loginForm
      }
    }</div>

  def showIfLoggedIn(in: NodeSeq): NodeSeq = currentUser match {
    case Full(_) => in
    case _ => Text("")
  }

  def showIfLoggedOut(in: NodeSeq): NodeSeq = currentUser match {
    case Full(_) => Text("")
    case _ => in
  }

  def logUserOut(): Unit

  /**
   * Try to log a user into the system with a given openId
   */
  def loginAndRedirect(openId: String, onComplete: (Box[Identifier], Box[VerificationResult], Box[Exception]) => LiftResponse) {
    val oid = OpenIDObject.is
    oid.onComplete = Full(onComplete)

    throw ResponseShortcutException.shortcutResponse(try {
        oid.authRequest(openId, "/"+PathRoot+"/"+ResponsePath)
      } catch {
        case e: Exception => onComplete(Empty, Empty, Full(e))
      })
  }

  /**
   * Based on an exception, generate an error message
   */
  protected def generateOpenIDFailureMessage(exception: Exception): String =
    (S ? "OpenID Failure") + ": " + exception.getMessage

  def dispatchPF: LiftRules.DispatchPF = NamedPF("Login default") {
    case Req(PathRoot :: LogOutPath :: _, "", _) =>
      () => {
        logUserOut()
        Full(RedirectResponse(S.referer openOr "/", S responseCookies :_*))
      }

    case r @ Req(PathRoot :: LoginPath :: _, "", PostRequest)
      if r.param(PostParamName).isDefined =>
      () => {
        try {
          RedirectBackTo(S.referer)
          Full(OpenIDObject.is.authRequest(r.param(PostParamName).get, "/"+PathRoot+"/"+ResponsePath))
        } catch {
          case e: Exception => {
            S.error(generateOpenIDFailureMessage(e))
            // FIXME -- log the name and the error
            Full(RedirectResponse(S.referer openOr "/", S responseCookies :_*))
          }
        }
      }

    case r @ Req(PathRoot :: ResponsePath :: _, "", _) =>
      () => {
        for (req <- S.request;
             ret <- {
            val (id, res) = OpenIDObject.is.verifyResponse(req.request)

            OpenIDObject.onComplete match {
              case Full(f) => Full(f(id, Full(res), Empty))

              case _ => postLogin(id, res)
                val rb = RedirectBackTo.is
                Full(RedirectResponse(rb openOr "/", S responseCookies :_*))
            }
          }) yield ret


      }
  }
}

trait SimpleOpenIDVendor extends OpenIDVendor {
  type UserType = Identifier
  type ConsumerType = OpenIDConsumer[UserType]

  def currentUser = OpenIDUser.is

  /**
   * Generate a welcome message for the OpenID identifier
   */
  protected def generateWelcomeMessage(id: Identifier): String =
    (S ? "Welcome")+ ": "+ id

  /**
   * If verification failed, generate a polite message to that
   * effect.
   */
  protected def generateAuthenticationFailure(res: VerificationResult): String =
    S ? "Failed to authenticate"

  def postLogin(id: Box[Identifier],res: VerificationResult): Unit = {
    id match {
      case Full(id) => S.notice(generateWelcomeMessage(id))

      case _ => S.error(generateAuthenticationFailure(res))
    }

    OpenIDUser(id)
  }

  def logUserOut() {
    OpenIDUser.remove
  }

  /**
   * Generate a welcome message.
   */
  def displayUser(in: UserType): NodeSeq = Text("Welcome "+in)

  def createAConsumer = new AnyRef with OpenIDConsumer[UserType]
}

object SimpleOpenIDVendor extends SimpleOpenIDVendor


object OpenIDUser extends SessionVar[Box[Identifier]](Empty)

/** * Sample Consumer (Relying Party) implementation.  */
trait OpenIDConsumer[UserType] extends Logger {
  val manager = new ConsumerManager

  var onComplete: Box[(Box[Identifier], Box[VerificationResult], Box[Exception]) => LiftResponse] = Empty

  /**
   * Set this to a function that can modify (eg add extensions) to the auth request before send
   */
  var beforeAuth: Box[(DiscoveryInformation,AuthRequest) => Unit] = Empty

  // --- placing the authentication request ---
  def authRequest(userSuppliedString: String, targetUrl: String): LiftResponse =
  {
    // configure the return_to URL where your application will receive
    // the authentication responses from the OpenID provider
    val returnToUrl = S.encodeURL(S.hostAndPath + targetUrl)

    info("Creating openId auth request.  returnToUrl: "+returnToUrl)

    // perform discovery on the user-supplied identifier
    val discoveries = manager.discover(userSuppliedString)

    // attempt to associate with the OpenID provider
    // and retrieve one service endpoint for authentication
    val discovered = manager.associate(discoveries)

    S.containerSession.foreach(_.setAttribute("openid-disc", discovered))

    // obtain a AuthRequest message to be sent to the OpenID provider
    val authReq = manager.authenticate(discovered, returnToUrl)

    beforeAuth foreach {f => f(discovered, authReq)}

    if (! discovered.isVersion2() )
    {
      // Option 1: GET HTTP-redirect to the OpenID Provider endpoint
      // The only method supported in OpenID 1.x
      // redirect-URL usually limited ~2048 bytes
      RedirectResponse(authReq.getDestinationUrl(true))
    }
    else
    {
      // Option 2: HTML FORM Redirection (Allows payloads >2048 bytes)
      val pm =  authReq.getParameterMap()
      val info: Seq[(String, String)] = pm.keySet.toArray.
      map(k => (k.toString, pm.get(k).toString))

      XhtmlResponse(
        <html xmlns="http://www.w3.org/1999/xhtml">
          <head>
            <title>OpenID HTML FORM Redirection</title>
          </head>
          <body onload="document.forms['openid-form-redirection'].submit();">
            <form name="openid-form-redirection" action={authReq.getDestinationUrl(false)} method="post" accept-charset="utf-8">
              {
                info.map{ case(key, value) =>
                    <input type="hidden" name={key} value={value}/>
                }
              }
              <button type="submit">Continue...</button>
            </form>
          </body>
        </html>, Empty, Nil, Nil, 200, true)
    }
  }

  // --- processing the authentication response ---
  def verifyResponse(httpReq: HTTPRequest): (Box[Identifier], VerificationResult) =
  {
    // extract the parameters from the authentication response
    // (which comes in as a HTTP request from the OpenID provider)
    val paramMap = new java.util.HashMap[String, String]
    httpReq.params.foreach(e => paramMap.put(e.name, e.values.headOption getOrElse null))
    val response =	new ParameterList(paramMap);

    // retrieve the previously stored discovery information
    val discovered = httpReq.session.attribute("openid-disc") match {
      case d: DiscoveryInformation => d
      case _ => throw ResponseShortcutException.redirect("/")
    }

    // extract the receiving URL from the HTTP request
    var receivingURL = httpReq.url
    val queryString = httpReq.queryString openOr ""
    if (queryString != null && queryString.length() > 0) {
      receivingURL += "?" + queryString;
    }


    // verify the response; ConsumerManager needs to be the same
    // (static) instance used to place the authentication request
    val verification = manager.verify(receivingURL.toString(),
                                      response, discovered)

    // examine the verification result and extract the verified identifier

    val verified = verification.getVerifiedId();

    (Box.legacyNullTest(verified), verification)
  }
}

}
}
