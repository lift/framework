/*
 * Copyright 2007-2010 WorldWide Conferencing, LLC
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
package http {

import _root_.net.liftweb.common._
import _root_.scala.xml.{Node, Group, NodeSeq}
import _root_.net.liftweb.util._
import _root_.net.liftweb.http.provider._
import js._
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.json.{JsonAST, Printer}

/**
 * 200 response but without body.
 */
case class OkResponse() extends LiftResponse with HeaderDefaults {
  def toResponse = InMemoryResponse(Array(), headers, cookies, 200)
}

trait HeaderDefaults {
  val headers: List[(String, String)] = S.getHeaders(Nil)
  val cookies: List[HTTPCookie] = S.responseCookies
}

/**
 * 201 Created Response
 *
 * The Resource was created. We then return the resource, post-processing, to
 * the client. Usually used with HTTP PUT.
 */
case class CreatedResponse(xml: Node, mime: String) extends NodeResponse {
  def docType = Empty

  def code = 201

  def headers: List[(String, String)] = List("Content-Type" -> mime)

  def cookies: List[HTTPCookie] = Nil

  def out = xml
}

/**
 * 202 response but without body.
 */
case class AcceptedResponse() extends LiftResponse with HeaderDefaults {
  def toResponse = InMemoryResponse(Array(), headers, cookies, 202)
}

/**
 * 204 response but without body.
 */
case class NoContentResponse() extends LiftResponse with HeaderDefaults {
  def toResponse = InMemoryResponse(Array(), headers, cookies, 204)
}

/**
 * 205 response but without body.
 */
case class ResetContentResponse() extends LiftResponse with HeaderDefaults {
  def toResponse = InMemoryResponse(Array(), headers, cookies, 205)
}

/**
 * 301 Redirect.
 */
case class PermRedirectResponse(uri: String, request: Req, cookies: HTTPCookie*) extends LiftResponse {
  def toResponse = InMemoryResponse(Array(), List("Location" -> request.updateWithContextPath(uri)), cookies.toList, 301)
}

/**
 * 307 Redirect.
 */
case class TemporaryRedirectResponse(uri: String, request: Req, cookies: HTTPCookie*) extends LiftResponse {
  def toResponse = InMemoryResponse(Array(), List("Location" -> request.updateWithContextPath(uri)), cookies.toList, 307)
}

/**
 * 400 Bad Request
 *
 * Your Request was missing an important element. Use this as a last resort if
 * the request appears incorrect.
 */
case class BadResponse() extends LiftResponse with HeaderDefaults {
  def toResponse = InMemoryResponse(Array(), headers, cookies, 400)
}

/**
 * 401 Unauthorized Response.
 */
case class UnauthorizedResponse(realm: String) extends LiftResponse {
  def toResponse = InMemoryResponse(Array(), List("WWW-Authenticate" -> ("Basic realm=\"" + realm + "\"")), Nil, 401)
}

object Qop extends Enumeration(0, "auth", "auth-int", "auth,auth-int") {
  val AUTH, AUTH_INT, AUTH_AND_AUTH_INT = Value
}

/**
 * 401 Unauthorized Response.
 */
case class UnauthorizedDigestResponse(override val realm: String, qop: Qop.Value, nonce: String, opaque: String) extends UnauthorizedResponse(realm) {
  override def toResponse = InMemoryResponse(Array(), List("WWW-Authenticate" -> (
          "Digest realm=\"" + realm + "\", " +
                  "qop=\"" + qop + "\", " +
                  "nonce=\"" + nonce + "\", " +
                  "opaque=\"" + opaque + "\""
          )), Nil, 401)
}

object ForbiddenResponse {
  def apply() = new ForbiddenResponse("")
}


/**
 * 403 Forbidden
 *
 * The server understood the request, but is refusing to fulfill it.
 * Authorization will not help and the request SHOULD NOT be repeated.
 */
case class ForbiddenResponse(message: String) extends LiftResponse with HeaderDefaults {
  def toResponse = InMemoryResponse(message.getBytes("UTF-8"), "Content-Type" -> "text/plain; charset=utf-8" :: headers, cookies, 403)
}


object NotFoundResponse {
  def apply() = new NotFoundResponse("")
}

/**
 * 404 Not Found
 *
 * The server has not found anything matching the Request-URI.
 */
case class NotFoundResponse(message: String) extends LiftResponse with HeaderDefaults {
  def toResponse = InMemoryResponse(message.getBytes("UTF-8"), "Content-Type" -> "text/plain; charset=utf-8" :: headers, cookies, 404)
}

/**
 * 405 Method Not Allowed
 *
 * This Resource does not allow this method. Use this when the resource can't
 * understand the method no matter the circumstances.
 */
case class MethodNotAllowedResponse() extends LiftResponse with HeaderDefaults {
  def toResponse = InMemoryResponse(Array(), headers, cookies, 405)
}

/**
 * 406 Not Acceptable
 *
 * This Resource does not allow this method. Use this when the resource can't
 * understand the method no matter the circumstances.
 */
case class NotAcceptableResponse(msg: String) extends LiftResponse with HeaderDefaults {
  def toResponse = InMemoryResponse(msg.getBytes("UTF-8"), headers, cookies, 406)
}

object NotAcceptableResponse {
  def apply() = new NotAcceptableResponse("")
}

/**
 * 410 Resource Gone
 *
 * The requested Resource used to exist but no longer does.
 */
case class GoneResponse() extends LiftResponse with HeaderDefaults {
  def toResponse = InMemoryResponse(Array(), headers, cookies, 410)
}

/**
 * 415 Resource Gone
 *
 * The requested Resource used to exist but no longer does.
 */
case class UnsupportedMediaTypeResponse() extends LiftResponse with HeaderDefaults {
  def toResponse = InMemoryResponse(Array(), headers, cookies, 415)
}

/**
 * 500 Internal Server Error
 *
 * The server encountered an unexpected condition which prevented
 * it from fulfilling the request.
 */
case class InternalServerErrorResponse() extends LiftResponse with HeaderDefaults {
  def toResponse = InMemoryResponse(Array(), headers, cookies, 500)
}

/**
 * 501 Not Implemented
 *
 * The server does not support the functionality required to
 * fulfill the request. This is the appropriate response when the
 * server does not recognize the request method and is not capable
 * of supporting it for any resource.
 */
case class NotImplementedResponse() extends LiftResponse with HeaderDefaults {
  def toResponse = InMemoryResponse(Array(), headers, cookies, 501)
}

/**
 * 502 Bad Gateway
 *
 * The server, while acting as a gateway or proxy, received an invalid
 * response from the upstream server it accessed in attempting
 * to fulfill the request.
 */
case class BadGatewayResponse() extends LiftResponse with HeaderDefaults {
  def toResponse = InMemoryResponse(Array(), headers, cookies, 502)
}

/**
 * 503 Bad Gateway
 *
 * The server, while acting as a gateway or proxy, received an invalid
 * response from the upstream server it accessed in attempting
 * to fulfill the request.
 */
case class ServiceUnavailableResponse(retryAfter: Long) extends LiftResponse {
  def toResponse = InMemoryResponse(Array(), List("Retry-After" -> retryAfter.toString), Nil, 503)
}

object JavaScriptResponse {
  def apply(js: JsCmd): LiftResponse = JavaScriptResponse(js, S.getHeaders(Nil), S.responseCookies, 200)
}

/**
 * Impersonates a HTTP response having Content-Type = text/javascript
 */
case class JavaScriptResponse(js: JsCmd, headers: List[(String, String)], cookies: List[HTTPCookie], code: Int) extends LiftResponse {
  def toResponse = {
    val bytes = js.toJsCmd.getBytes("UTF-8")
    InMemoryResponse(bytes, ("Content-Length", bytes.length.toString) :: ("Content-Type", "text/javascript; charset=utf-8") :: headers, cookies, code)
  }
}

trait LiftResponse {
  def toResponse: BasicResponse
}

object JsonResponse {
  def headers: List[(String, String)] = S.getHeaders(Nil)
  def cookies: List[HTTPCookie] = S.responseCookies

  def apply(json: JsExp): LiftResponse = 
    new JsonResponse(json, headers, cookies, 200)
  
  def apply(json: JsonAST.JValue): LiftResponse = 
    apply(json, headers, cookies, 200)

  def apply(json: JsonAST.JValue, code: Int): LiftResponse = 
    apply(json, headers, cookies, code)


  def apply(_json: JsonAST.JValue, _headers: List[(String, String)], _cookies: List[HTTPCookie], code: Int): LiftResponse = {
    new JsonResponse(new JsExp {
      lazy val toJsCmd = Printer.pretty(JsonAST.render((_json)))
    }, _headers, _cookies, code)
  }
}

case class JsonResponse(json: JsExp, headers: List[(String, String)], cookies: List[HTTPCookie], code: Int) extends LiftResponse {
  def toResponse = {
    val bytes = json.toJsCmd.getBytes("UTF-8")
    InMemoryResponse(bytes, ("Content-Length", bytes.length.toString) :: ("Content-Type", "application/json; charset=utf-8") :: headers, cookies, code)
  }
}

sealed trait BasicResponse extends LiftResponse {
  def headers: List[(String, String)]

  def cookies: List[HTTPCookie]

  def code: Int

  def size: Long
}

/**
 * Wraps a LiftResponse along with a HTTP reason-phrase. The
 * reason-phrase will be set in the HTTP status line after 
 * the status code as per HTTP specifications. 
 *
 * @param response - the response to be wrapped
 * @param reason - the reason-phrase
 */
case class ResponseWithReason(response: LiftResponse, reason: String) extends LiftResponse {

  def toResponse = response.toResponse
}

private[http] case object EmptyResponse extends BasicResponse {
  def headers: List[(String, String)] = Nil

  def cookies: List[HTTPCookie] = Nil

  def code = 200

  def size = 0

  def toResponse = this
}

final case class InMemoryResponse(data: Array[Byte], headers: List[(String, String)], cookies: List[HTTPCookie], code: Int) extends BasicResponse {
  def toResponse = this

  def size = data.length

  override def toString = "InMemoryResponse(" + (new String(data, "UTF-8")) + ", " + headers + ", " + cookies + ", " + code + ")"
}

final case class StreamingResponse(data: {def read(buf: Array[Byte]): Int}, onEnd: () => Unit, size: Long, headers: List[(String, String)], cookies: List[HTTPCookie], code: Int) extends BasicResponse {
  def toResponse = this

  override def toString = "StreamingResponse( steaming_data , " + headers + ", " + cookies + ", " + code + ")"
}


object OutputStreamResponse {

  def apply(out: (java.io.OutputStream) => Unit) = 
    new OutputStreamResponse(out, -1, Nil, Nil, 200)

  def apply(out: (java.io.OutputStream) => Unit, size: Long) = 
    new OutputStreamResponse(out, size, Nil, Nil, 200)

  def apply(out: (java.io.OutputStream) => Unit, headers: List[(String, String)]) = 
    new OutputStreamResponse(out, -1, headers, Nil, 200)

  def apply(out: (java.io.OutputStream) => Unit, size: Long, headers: List[(String, String)]) = 
    new OutputStreamResponse(out, size, headers, Nil, 200)

}

/**
 * Use this response to write your data directly to the response pipe. Along with StreamingResponse
 * you have an aternative to send data to the client.
 */
case class OutputStreamResponse(out: (java.io.OutputStream) => Unit,  
  size: Long, 
  headers: List[(String, String)], 
  cookies: List[HTTPCookie], 
  code: Int) extends BasicResponse {

  def toResponse = this

}

case class RedirectResponse(uri: String, cookies: HTTPCookie*) extends LiftResponse {
  // The Location URI is not resolved here, instead it is resolved with context path prior of sending the actual response
  def toResponse = InMemoryResponse(Array(), List("Location" -> uri, "Content-Type" -> "text/plain"), cookies toList, 302)
}

case class SeeOtherResponse(uri: String, cookies: HTTPCookie*) extends LiftResponse {
  // The Location URI is not resolved here, instead it is resolved with context path prior of sending the actual response
  def toResponse = InMemoryResponse(Array(), List("Location" -> uri, "Content-Type" -> "text/plain"), cookies toList, 303)
}

object DoRedirectResponse {
  def apply(url: String): LiftResponse = RedirectResponse(url, Nil: _*)
}

case class RedirectWithState(override val uri: String, state: RedirectState, override val cookies: HTTPCookie*) extends RedirectResponse(uri, cookies: _*)

object RedirectState {
  def apply(f: () => Unit, msgs: (String, NoticeType.Value)*): RedirectState = new RedirectState(Full(f), msgs: _*)
}
case class RedirectState(func: Box[() => Unit], msgs: (String, NoticeType.Value)*)

object MessageState {
  implicit def tuple2MessageState(msg: (String, NoticeType.Value)) = MessageState(msg)
}

case class MessageState(override val msgs: (String, NoticeType.Value)*) extends RedirectState(Empty, msgs: _*)

/**
 * Stock XHTML doctypes available to the lift programmer.
 */
object DocType {
  val xhtmlTransitional = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">"

  val xhtmlStrict = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"

  val xhtmlFrameset = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">"

  val xhtml11 = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">"

  val xhtmlMobile = "<!DOCTYPE html PUBLIC \"-//WAPFORUM//DTD XHTML Mobile 1.0//EN\" \"http://www.wapforum.org/DTD/xhtml-mobile10.dtd\">"

  val html5 = "<!DOCTYPE html>"
}

/**
 * Avoid using this in favor of LiftRules.docType
 *
 */
@deprecated
object ResponseInfo {

   def docType: PartialFunction[Req, Box[String]] = new PartialFunction[Req, Box[String]](){
     def isDefinedAt(req: Req): Boolean  = true

     def apply(req: Req): Box[String] = LiftRules.docType.vend(req)
   }

   def docType_=(f: PartialFunction[Req, Box[String]]) = LiftRules.docType.default.set { (req: Req) => 
     if (f.isDefinedAt(req))
       f(req)
     else
       Full(DocType.xhtmlTransitional)
   }
}


object PlainTextResponse {
  def apply(text: String): PlainTextResponse = PlainTextResponse(text, Nil, 200)

  def apply(text: String, code: Int): PlainTextResponse = PlainTextResponse(text, Nil, code)
}

case class PlainTextResponse(text: String, headers: List[(String, String)], code: Int) extends LiftResponse {
  def toResponse = {
    val bytes = text.getBytes("UTF-8")
    InMemoryResponse(bytes, ("Content-Length", bytes.length.toString) :: ("Content-Type", "text/plain; charset=utf-8") :: headers, Nil, code)
  }
}

object CSSResponse {
  def apply(text: String): CSSResponse = CSSResponse(text, Nil, 200)

  def apply(text: String, code: Int): CSSResponse = CSSResponse(text, Nil, code)
}

case class CSSResponse(text: String, headers: List[(String, String)], code: Int) extends LiftResponse {
  def toResponse = {
    val bytes = text.getBytes("UTF-8")
    InMemoryResponse(bytes, ("Content-Length", bytes.length.toString) :: ("Content-Type", "text/css; charset=utf-8") :: headers, Nil, code)
  }
}

trait NodeResponse extends LiftResponse {
  def out: Node

  def headers: List[(String, String)]

  def cookies: List[HTTPCookie]

  def code: Int

  def docType: Box[String]

  def renderInIEMode: Boolean = false

  def includeXmlVersion = true

  val isIE6 = LiftRules.calcIE6ForResponse()

  def flipDocTypeForIE6 = false

  def toResponse = {
    val encoding: String = if (!includeXmlVersion) "" else _encoding

    val doc = docType.map(_ + "\n") openOr ""

    val sb = new StringBuilder(64000)

    if (flipDocTypeForIE6 && isIE6) {
      sb.append(doc)
      sb.append(encoding)
    } else {
      sb.append(encoding)
      sb.append(doc)
    }
    AltXML.toXML(out, _root_.scala.xml.TopScope,
      sb, false, !LiftRules.convertToEntity.vend, renderInIEMode)

    sb.append("  \n  ")

    val ret = sb.toString

    InMemoryResponse(ret.getBytes("UTF-8"), headers, cookies, code)
  }

   private val _encoding: String =
   LiftRules.calculateXmlHeader(this, out, headers.ciGet("Content-Type"))
}

case class XhtmlResponse(out: Node, docType: Box[String],
                         headers: List[(String, String)],
                         cookies: List[HTTPCookie],
                         code: Int,
                         override val renderInIEMode: Boolean) extends NodeResponse {
  private[http] var _includeXmlVersion = true

  override def includeXmlVersion = _includeXmlVersion

  override def flipDocTypeForIE6 = LiftRules.flipDocTypeForIE6
}


/**
 * Allows you to create custom 200 responses for clients using different
 * Content-Types.
 */
case class XmlMimeResponse(xml: Node, mime: String) extends NodeResponse {
  def docType = Empty

  def code = 200

  def headers: List[(String, String)] = List("Content-Type" -> mime)

  def cookies: List[HTTPCookie] = Nil

  def out = xml
}

class XmlResponse(val xml: Node, val code: Int, val mime: String, val cookies: List[HTTPCookie]) extends NodeResponse {
  def docType = Empty

  def headers: List[(String, String)] = List("Content-Type" -> mime)

  def out: Node = xml
}

object XmlResponse {
  /** Construct XmlResponse with 200 OK response code and "text/xml" mime type */
  def apply(xml: Node) = new XmlResponse(xml, 200, "text/xml; charset=utf-8", Nil)

  /** Construct XmlResponse with given response code and "text/xml" mime type */
  def apply(xml: Node, code: Int) = new XmlResponse(xml, code, "text/xml; charset=utf-8", Nil)

  /** Construct XmlResponse with 200 OK response code and given mime type */
  def apply(xml: Node, mime: String) = new XmlResponse(xml, 200, mime, Nil)

  /** Construct XmlResponse with given response code and mime type */
  def apply(xml: Node, code: Int, mime: String) = new XmlResponse(xml, code, mime, Nil)

  /** Construct XmlResponse with 200 OK response code, "text/xml" mime type and given cookies */
  def apply(xml: Node, cookies: List[HTTPCookie]) = new XmlResponse(xml, 200, "text/xml; charset=utf-8", cookies)

  /** Construct XmlResponse with given response code, given cookies and "text/xml" mime type */
  def apply(xml: Node, code: Int, cookies: List[HTTPCookie]) = new XmlResponse(xml, code, "text/xml; charset=utf-8", cookies)

  /** Construct XmlResponse with 200 OK response code, given mime type and given cookies */
  def apply(xml: Node, mime: String, cookies: List[HTTPCookie]) = new XmlResponse(xml, 200, mime, cookies)

  /** Construct XmlResponse with given response code, mime type and cookies */
  def apply(xml: Node, code: Int, mime: String, cookies: List[HTTPCookie]) = new XmlResponse(xml, code, mime, cookies)

}

/**
 * Returning an Atom document.
 */
case class AtomResponse(xml: Node) extends NodeResponse {
  def docType = Empty

  def code = 200

  def headers: List[(String, String)] = List("Content-Type" -> "application/atom+xml; charset=utf-8")

  def cookies: List[HTTPCookie] = Nil

  def out = xml
}

/**
 * Returning an OpenSearch Description Document.
 */
case class OpenSearchResponse(xml: Node) extends NodeResponse {
  def docType = Empty

  def code = 200

  def headers: List[(String, String)] = List("Content-Type" -> "application/opensearchdescription+xml; charset=utf-8")

  def cookies: List[HTTPCookie] = Nil

  def out = xml
}

/**
 * The Atom entity was successfully created and is shown to the client.
 */
case class AtomCreatedResponse(xml: Node) extends LiftResponse {
  def toResponse = CreatedResponse(xml, "application/atom+xml").toResponse
}

/**
 * Returning an Atom category document.
 */
case class AtomCategoryResponse(xml: Node) extends LiftResponse {
  def toResponse = XmlMimeResponse(xml, "application/atomcat+xml").toResponse
}

/**
 * Returning an Atom Service Document.
 */
case class AtomServiceResponse(xml: Node) extends LiftResponse {
  def toResponse = XmlMimeResponse(xml, "application/atomsvc+xml").toResponse
}

}
}
