/*
 * Copyright 2007-2011 WorldWide Conferencing, LLC
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

package net.liftweb
package http

import net.liftweb.common._
import scala.xml.Node
import net.liftweb.util._
import net.liftweb.http.provider._
import js._
import net.liftweb.util.Helpers._
import net.liftweb.json.JsonAST
import java.io.{OutputStream, OutputStreamWriter, Writer, ByteArrayOutputStream}

/**
 * 200 response but without body.
 */
case class OkResponse() extends LiftResponse with HeaderDefaults {
  def toResponse = InMemoryResponse(Array(), headers, cookies, 200)
}

trait HeaderDefaults {
  val headers: List[(String, String)] = S.getResponseHeaders(Nil)
  val cookies: List[HTTPCookie] = S.responseCookies
}

/**
 * 201 Created Response
 *
 * The Resource was created. We then return the resource, post-processing, to
 * the client. Usually used with HTTP PUT.
 */
case class CreatedResponse(xml: Node, mime: String, addlHeaders: List[(String, String)] = XmlResponse.addlHeaders) extends XmlNodeResponse {
  def docType = Empty

  def code = 201

  val headers: List[(String, String)] = S.getResponseHeaders(("Content-Type" -> mime) :: addlHeaders)

  def cookies: List[HTTPCookie] = Nil

  def out = xml
}

/**
 * 201 Created Response
 *
 * The Json Resource was created. We then return the resource, post-processing, to
 * the client. Usually used with HTTP PUT.
 */
object CreatedResponse {

  lazy val jsonPrinter: scala.text.Document => String =
    LiftRules.jsonOutputConverter.vend

  def apply(json: JsonAST.JValue, addlHeaders: List[(String, String)]): LiftResponse = {
    val headers: List[(String, String)] = S.getResponseHeaders( Nil ) ++  addlHeaders

    new JsonResponse(new JsExp {
      lazy val toJsCmd = jsonPrinter(JsonAST.render(json))
    }, headers, Nil, 201)
  }

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

object Qop extends Enumeration {
  val AUTH = Value("auth")
  val AUTH_INT = Value("auth-int")
  val AUTH_AND_AUTH_INT = Value("auth,auth-int")
}

/**
 * Companion object with builder
 */
object UnauthorizedDigestResponse {
  def apply(realm: String, qop: Qop.Value, nonce: String, opaque: String): UnauthorizedDigestResponse = 
    new UnauthorizedDigestResponse(realm,
                                   qop,
                                   nonce,
                                   opaque)
}

/**
 * 401 Unauthorized Response.
 */
class UnauthorizedDigestResponse(override val realm: String, qop: Qop.Value, nonce: String, opaque: String) extends UnauthorizedResponse(realm) {
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
  def apply(js: JsCmd): LiftResponse = JavaScriptResponse(js, S.getResponseHeaders(Nil), S.responseCookies, 200)
}

/**
 * Impersonates a HTTP response having Content-Type = text/javascript
 */
case class JavaScriptResponse(js: JsCmd, headers: List[(String, String)], cookies: List[HTTPCookie], code: Int) extends LiftResponse {
  def toResponse = {
    val bytes = js.toJsCmd.getBytes("UTF-8")
    InMemoryResponse(bytes, ("Content-Length", bytes.length.toString) :: ("Content-Type", "application/javascript; charset=utf-8") :: headers, cookies, code)
  }
}

trait LiftResponse {
  def toResponse: BasicResponse
}

object JsonResponse {
  def headers: List[(String, String)] = S.getResponseHeaders(Nil)
  def cookies: List[HTTPCookie] = S.responseCookies

  def apply(json: JsExp): LiftResponse = 
    new JsonResponse(json, headers, cookies, 200)
  
  def apply(json: JsonAST.JValue): LiftResponse = 
    apply(json, headers, cookies, 200)

  def apply(json: JsonAST.JValue, code: Int): LiftResponse = 
    apply(json, headers, cookies, code)


  def apply(_json: JsonAST.JValue, _headers: List[(String, String)], _cookies: List[HTTPCookie], code: Int): LiftResponse = {
    new JsonResponse(new JsExp {
      lazy val toJsCmd = jsonPrinter(JsonAST.render((_json)))
    }, _headers, _cookies, code)
  }

  lazy val jsonPrinter: scala.text.Document => String = 
    LiftRules.jsonOutputConverter.vend
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

  def apply(out: (OutputStream) => Unit) = 
    new OutputStreamResponse(out, -1, Nil, Nil, 200)

  def apply(out: (OutputStream) => Unit, size: Long) = 
    new OutputStreamResponse(out, size, Nil, Nil, 200)

  def apply(out: (OutputStream) => Unit, headers: List[(String, String)]) = 
    new OutputStreamResponse(out, -1, headers, Nil, 200)

  def apply(out: (OutputStream) => Unit, size: Long, headers: List[(String, String)]) = 
    new OutputStreamResponse(out, size, headers, Nil, 200)

}

/**
 * Use this response to write your data directly to the response pipe. Along with StreamingResponse
 * you have an alternative to send data to the client.
 */
case class OutputStreamResponse(out: (OutputStream) => Unit,  
  size: Long, 
  headers: List[(String, String)], 
  cookies: List[HTTPCookie], 
  code: Int) extends BasicResponse {

  def toResponse = this

}


/**
 * 301 Redirect.
 */
case class PermRedirectResponse(uri: String, request: Req, cookies: HTTPCookie*) extends LiftResponse {
  def toResponse = InMemoryResponse(Array(), List("Location" -> uri), cookies.toList, 301)
}

/**
 * 307 Redirect.
 */
case class TemporaryRedirectResponse(uri: String, request: Req, cookies: HTTPCookie*) extends LiftResponse {
  def toResponse = InMemoryResponse(Array(), List("Location" -> uri), cookies.toList, 307)
}

/**
 * Companion object to RedirectResponse
 */
object RedirectResponse {
  /**
   * Construct an instnace of RedirectResponse
   */
  def apply(uri: String, cookies: HTTPCookie*): RedirectResponse = 
    new RedirectResponse(uri, S.request or CurrentReq.box openOr Req.nil, 
                         cookies :_*)

}

/**
 * 302
 */
case class RedirectResponse(uri: String, request: Req, cookies: HTTPCookie*) extends LiftResponse {
  // The Location URI is not resolved here, instead it is resolved with context path prior of sending the actual response
  def toResponse = InMemoryResponse(Array(), List("Location" -> uri,
    "Content-Type" -> "text/plain"), cookies.toList, 302)
}


/**
 * Companion object to RedirectResponse
 */
object SeeOtherResponse {
  /**
   * Construct an instnace of SeeOtherResponse
   */
  def apply(uri: String, cookies: HTTPCookie*): SeeOtherResponse = 
    new SeeOtherResponse(uri, S.request or CurrentReq.box openOr Req.nil, 
                         cookies :_*)
}

/**
 * 303
 */
case class SeeOtherResponse(uri: String, request: Req, cookies: HTTPCookie*) extends LiftResponse {
  // The Location URI is not resolved here, instead it is resolved with context path prior of sending the actual response
  def toResponse = InMemoryResponse(Array(), List("Location" -> uri,
    "Content-Type" -> "text/plain"), cookies.toList, 303)
}

object DoRedirectResponse {
  def apply(url: String): LiftResponse = RedirectResponse.apply(url, List[HTTPCookie]() :_*)
}

object RedirectWithState {
  def apply(uri: String, state: RedirectState, cookies: HTTPCookie*): RedirectWithState =
    this.apply(uri, S.request or CurrentReq.box openOr Req.nil, state, cookies :_*)


  def apply(uri: String, req: Req, state: RedirectState, cookies: HTTPCookie*): RedirectWithState =
    new RedirectWithState(uri, req, state, cookies :_*)

  def unapply(in: Any): Option[(String, RedirectState, Seq[HTTPCookie])] =
    in match {
      case rdws: RedirectWithState => Some((rdws.uri, rdws.state,
                                            rdws.cookies))
      case _ => None
    }
}

class RedirectWithState(override val uri: String, val req: Req, val state: RedirectState, override val cookies: HTTPCookie*) extends RedirectResponse(uri, req, cookies: _*)

object RedirectState {
  def apply(f: () => Unit, msgs: (String, NoticeType.Value)*): RedirectState = new RedirectState(Full(f), msgs: _*)
}
case class RedirectState(func: Box[() => Unit], msgs: (String, NoticeType.Value)*)

object MessageState {
  implicit def tuple2MessageState(msg: (String, NoticeType.Value)) = MessageState(msg)

  def apply(msgs: (String, NoticeType.Value)*): MessageState =
     new MessageState(msgs :_*)
}

class MessageState(override val msgs: (String, NoticeType.Value)*) extends RedirectState(Empty, msgs: _*)

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

  val htmlProperties: HtmlProperties = S.htmlProperties

  def headers: List[(String, String)]

  def cookies: List[HTTPCookie]

  def code: Int

  def docType: Box[String]

  def renderInIEMode: Boolean = false

  def includeXmlVersion = true

  val isIE6 = LiftRules.calcIE6ForResponse()

  def flipDocTypeForIE6 = false

  protected def writeDocType(writer: Writer): Unit = {
    val doc: String = docType.map(_ + "\n") openOr ""
    val encoding: String = if (!includeXmlVersion) "" else _encoding

    if (flipDocTypeForIE6 && isIE6) {
      writer.append(doc)
      writer.append(encoding)
    } else {
      writer.append(encoding)
      writer.append(doc)
    }
  }

  protected lazy val _encoding: String =  
    LiftRules.calculateXmlHeader(this, out, headers.ciGet("Content-Type"))  

  def toResponse = {
    val bos = new ByteArrayOutputStream(64000)
    val writer = new OutputStreamWriter(bos, "UTF-8")

    writeDocType(writer)

    htmlProperties.htmlWriter(out, writer)

    writer.append("  \n  ")

    writer.flush()
    bos.flush()

    InMemoryResponse(bos.toByteArray, headers, cookies, code)
  }
}

trait XmlNodeResponse extends LiftResponse {
  def out: Node

  def headers: List[(String, String)]

  def addlHeaders: List[(String, String)]

  def cookies: List[HTTPCookie]

  def code: Int

  def docType: Box[String]

  /**
   * The encoding for this XML response
   */
  def encoding: String = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"

  protected def writeDocType(writer: Writer): Unit = {
    val doc: String = docType.map(_ + "\n") openOr ""

      writer.append(encoding)
      writer.append(doc)
  }

  def toResponse = {
    val bos = new ByteArrayOutputStream(64000)
    val writer = new OutputStreamWriter(bos, "UTF-8")

    writeDocType(writer)

    def htmlWriter: (Node, Writer) => Unit =
    (n: Node, w: Writer) => {
      val sb = new StringBuilder(64000)
      AltXML.toXML(n, scala.xml.TopScope,
                   sb, false, !LiftRules.convertToEntity.vend,
                   false)
      w.append(sb)
      w.flush()
    }

    htmlWriter(out, writer)

    writer.append("  \n  ")

    writer.flush()
    bos.flush()

    InMemoryResponse(bos.toByteArray, headers, cookies, code)
  }
}


case class XhtmlResponse(out: Node, 
                         private val __docType: Box[String],
                         private val _headers: List[(String, String)],
                         cookies: List[HTTPCookie],
                         code: Int,
                         override val renderInIEMode: Boolean) extends NodeResponse {
  private[http] var _includeXmlVersion: Boolean = true

  override def includeXmlVersion: Boolean = _includeXmlVersion

  override val flipDocTypeForIE6: Boolean = LiftRules.flipDocTypeForIE6

  val docType: Box[String] = htmlProperties.docType

  protected override def writeDocType(writer: Writer): Unit = {
    htmlProperties.htmlOutputHeader.foreach {
      writer.append
    }
  }

  override protected lazy val _encoding: String = htmlProperties.encoding openOr ""

  val headers: List[(String, String)] =
    _headers.find(_._1 equalsIgnoreCase "content-type") match {
      case Some(_) => _headers
      case _ => htmlProperties.contentType match {
        case Full(ct) => ("Content-Type" -> ct) :: _headers
        case _ => _headers
      }
    }
}


/**
 * Allows you to create custom 200 responses for clients using different
 * Content-Types.
 */
case class XmlMimeResponse(xml: Node, mime: String, addlHeaders: List[(String, String)] = XmlResponse.addlHeaders) extends XmlNodeResponse {
  def docType = Empty

  def code = 200

  val headers: List[(String, String)] = S.getResponseHeaders(("Content-Type" -> mime) :: addlHeaders)

  def cookies: List[HTTPCookie] = Nil

  def out = xml
}

class XmlResponse(val xml: Node, val code: Int, val mime: String, val cookies: List[HTTPCookie],
                  val addlHeaders: List[(String, String)] = XmlResponse.addlHeaders) extends XmlNodeResponse {
  def docType = Empty

  val headers: List[(String, String)] = S.getResponseHeaders(("Content-Type" -> mime) :: addlHeaders)

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

  private object _addlHeaders extends ThreadGlobal[List[(String, String)]]

  /**
   * Additional headers for the XmlResponse
   */
  def addlHeaders: List[(String, String)] = _addlHeaders.box openOr Nil

  def withHeaders[T](headers: (String, String)*)(f: => T): T = {
    val cur = _addlHeaders.box openOr Nil
    _addlHeaders.doWith(headers.toList ::: cur)(f)
  }

}

object AppXmlResponse {
  /** Construct XmlResponse with 200 OK response code and "application/xml" mime type */
  def apply(xml: Node) = new XmlResponse(xml, 200, "application/xml; charset=utf-8", Nil)

  /** Construct XmlResponse with given response code and "application/xml" mime type */
  def apply(xml: Node, code: Int) = new XmlResponse(xml, code, "application/xml; charset=utf-8", Nil)

  /** Construct XmlResponse with 200 OK response code and given mime type */
  def apply(xml: Node, mime: String) = new XmlResponse(xml, 200, mime, Nil)

  /** Construct XmlResponse with given response code and mime type */
  def apply(xml: Node, code: Int, mime: String) = new XmlResponse(xml, code, mime, Nil)

  /** Construct XmlResponse with 200 OK response code, "application/xml" mime type and given cookies */
  def apply(xml: Node, cookies: List[HTTPCookie]) = new XmlResponse(xml, 200, "application/xml; charset=utf-8", cookies)

  /** Construct XmlResponse with given response code, given cookies and "application/xml" mime type */
  def apply(xml: Node, code: Int, cookies: List[HTTPCookie]) = new XmlResponse(xml, code, "application/xml; charset=utf-8", cookies)

}

/**
 * Returning an Atom document.
 */
case class AtomResponse(xml: Node, addlHeaders: List[(String, String)] = XmlResponse.addlHeaders) extends XmlNodeResponse {
  def docType = Empty

  def code = 200

  val headers: List[(String, String)] = S.getResponseHeaders(("Content-Type" -> "application/atom+xml; charset=utf-8") :: addlHeaders)

  def cookies: List[HTTPCookie] = Nil

  def out = xml
}

/**
 * Returning an OpenSearch Description Document.
 */
case class OpenSearchResponse(xml: Node, addlHeaders: List[(String, String)] = XmlResponse.addlHeaders) extends XmlNodeResponse {
  def docType = Empty

  def code = 200

  val headers: List[(String, String)] = S.getResponseHeaders(("Content-Type" -> "application/opensearchdescription+xml; charset=utf-8") ::
  addlHeaders)

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

