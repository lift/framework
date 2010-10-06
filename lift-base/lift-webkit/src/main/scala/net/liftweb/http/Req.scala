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

import _root_.net.liftweb.util.Helpers
import Helpers._
import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import _root_.net.liftweb.json._
import _root_.net.liftweb.http.provider._
import _root_.net.liftweb.util.Helpers
import _root_.java.io.{InputStream, ByteArrayInputStream, File, FileInputStream,
                       FileOutputStream}
import _root_.scala.xml._
import sitemap._
import _root_.scala._


@serializable
sealed trait ParamHolder {
  def name: String
}
@serializable
case class NormalParamHolder(name: String, value: String) extends ParamHolder
@serializable
abstract class FileParamHolder(val name: String, val mimeType: String,
                               val fileName: String) extends ParamHolder
{
  def file: Array[Byte]

  def fileStream: InputStream
}

class InMemFileParamHolder(override val name: String, override val mimeType: String,
                           override val fileName: String, val file: Array[Byte]) extends
FileParamHolder(name, mimeType, fileName)
{
  def fileStream: InputStream = new ByteArrayInputStream(file)
}

class OnDiskFileParamHolder(override val name: String, override val mimeType: String,
                            override val fileName: String, val localFile: File) extends
FileParamHolder(name, mimeType, fileName)
{
  def fileStream: InputStream = new FileInputStream(localFile)

  def file: Array[Byte] = Helpers.readWholeStream(fileStream)

  protected override def finalize {
    tryo(localFile.delete)
  }
}

object OnDiskFileParamHolder {
  def apply(n: String, mt: String, fn: String, inputStream: InputStream): OnDiskFileParamHolder =
  {
    val file: File = File.createTempFile("lift_mime", "upload")
    val fos = new FileOutputStream(file)
    val ba = new Array[Byte](8192)
    def doUpload() {
      inputStream.read(ba) match {
        case x if x < 0 =>
        case 0 => doUpload()
        case x => fos.write(ba, 0, x); doUpload()
      }

    }

    doUpload()
    inputStream.close
    fos.close
    new OnDiskFileParamHolder(n, mt, fn, file)
  }
}

object FileParamHolder {
  def apply(n: String, mt: String, fn: String, file: Array[Byte]): FileParamHolder =
  new InMemFileParamHolder(n, mt, fn, file)

  def unapply(in: Any): Option[(String, String, String, Array[Byte])] = in match {
    case f: FileParamHolder => Some((f.name, f.mimeType, f.fileName, f.file))
    case _ => None
  }
}

/**
 * Helper object for constructing Req instances
 */
object Req {
  object NilPath extends ParsePath(Nil, "", true, false)

  private[http] lazy val localHostName = {
    import _root_.java.net._
    InetAddress.getLocalHost.getHostName
  }

  def apply(original: Req, rewrite: List[LiftRules.RewritePF]): Req = {

    def processRewrite(path: ParsePath, params: Map[String, String]): RewriteResponse =
    NamedPF.applyBox(RewriteRequest(path, original.requestType, original.request), rewrite) match {
      case Full(resp@RewriteResponse(_, _, true)) => resp
      case _: EmptyBox => RewriteResponse(path, params)
      case Full(resp) => processRewrite(resp.path, params ++ resp.params)
    }

    val rewritten = processRewrite(original.path, Map.empty)

    new Req(rewritten.path, original.contextPath, original.requestType, original.contentType, original.request,
      original.nanoStart, original.nanoEnd, original.paramCalculator, original.addlParams ++ rewritten.params)
  }

  def apply(request: HTTPRequest, rewrite: List[LiftRules.RewritePF], nanoStart: Long): Req = {
    val reqType = RequestType(request)
    val contextPath = LiftRules.calculateContextPath() openOr request.contextPath
    val turi = if (request.uri.length >= contextPath.length) request.uri.substring(contextPath.length) else ""
    val tmpUri = if (turi.length > 0) turi else "/"

    val tmpPath = parsePath(tmpUri)

    def processRewrite(path: ParsePath, params: Map[String, String]): RewriteResponse =
    NamedPF.applyBox(RewriteRequest(path, reqType, request), rewrite) match {
      case Full(resp@RewriteResponse(_, _, true)) => resp
      case _: EmptyBox => RewriteResponse(path, params)
      case Full(resp) => processRewrite(resp.path, params ++ resp.params)
    }



    // val (uri, path, localSingleParams) = processRewrite(tmpUri, tmpPath, TreeMap.empty)
    val rewritten = processRewrite(tmpPath, Map.empty)

    val localParams: Map[String, List[String]] = Map(rewritten.params.toList.map {case (name, value) => name -> List(value)}: _*)

    // val session = request.getSession
    //  val body = ()
    val eMap = Map.empty[String, List[String]]

    val contentType = request.contentType

    //    val (paramNames: List[String], params: Map[String, List[String]], files: List[FileParamHolder], body: Box[Array[Byte]]) =
    val paramCalculator = () => {

      // calculate the query parameters
      lazy val queryStringParam:  (List[String], Map[String, List[String]]) = {
        val params: List[(String, String)] =
        for {
          queryString <- request.queryString.toList
          nameVal <- queryString.split("&").toList.map(_.trim).filter(_.length > 0)
          (name, value) <- nameVal.split("=").toList match {
            case Nil => Empty
            case n :: v :: _ => Full((urlDecode(n), urlDecode(v)))
            case n :: _ => Full((urlDecode(n), ""))
          }} yield (name, value)

        val names: List[String] = params.map(_._1).removeDuplicates
        val nvp: Map[String, List[String]] = params.foldLeft(Map[String, List[String]]()) {
          case (map, (name, value)) => map + (name -> (map.getOrElse(name, Nil) ::: List(value)))
        }

        (names, nvp)
      }

      if ((reqType.post_? ||
           reqType.put_?) && contentType.dmap(false){
	_.toLowerCase match {
	  case x => 
	    x.startsWith("text/xml") || 
	  x.startsWith("text/json") ||
	  x.startsWith("application/json")
	}}) {
        ParamCalcInfo(queryStringParam._1, queryStringParam._2 ++ localParams, Nil, tryo(readWholeStream(request.inputStream)))
      } else if (request multipartContent_?) {
        val allInfo = request extractFiles

        val normal: List[NormalParamHolder] = allInfo.flatMap {case v: NormalParamHolder => List(v) case _ => Nil}
        val files: List[FileParamHolder] = allInfo.flatMap {case v: FileParamHolder => List(v) case _ => Nil}

        val params = normal.foldLeft(eMap)((a, b) =>
          a + (b.name -> (a.getOrElse(b.name, Nil) ::: List(b.value))))

        ParamCalcInfo((queryStringParam._1 ::: normal.map(_.name)).removeDuplicates, queryStringParam._2 ++ localParams ++ params, files, Empty)
      } else if (reqType.get_?) {
        ParamCalcInfo(queryStringParam._1, queryStringParam._2 ++ localParams, Nil, Empty)
      } else if (contentType.dmap(false)(_.toLowerCase.startsWith("application/x-www-form-urlencoded"))) {
        val params = localParams ++ (request.params.sort {(s1, s2) => s1.name < s2.name}).map(n => (n.name, n.values))
        ParamCalcInfo(request paramNames, params, Nil, Empty)
      } else {
        ParamCalcInfo(queryStringParam._1, queryStringParam._2 ++ localParams, Nil, tryo(readWholeStream(request inputStream)))
      }
    }

    new Req(rewritten.path, contextPath, reqType,
            contentType, request, nanoStart,
            System.nanoTime, paramCalculator, Map())
  }

  private def fixURI(uri: String) = uri indexOf ";jsessionid" match {
    case -1 => uri
    case x@_ => uri substring (0, x)
  }

  def nil = new Req(NilPath, "", GetRequest, Empty, null,
                    System.nanoTime, System.nanoTime,
                    () => ParamCalcInfo(Nil, Map.empty, Nil, Empty), Map())

  def parsePath(in: String): ParsePath = {
    val p1 = fixURI((in match {case null => "/"; case s if s.length == 0 => "/"; case s => s}).replaceAll("/+", "/"))
    val front = p1.startsWith("/")
    val back = p1.length > 1 && p1.endsWith("/")

    val orgLst = p1.replaceAll("/$", "/index").split("/").
    toList.map(_.trim).filter(_.length > 0)

    val (lst, suffix) = NamedPF(orgLst, LiftRules.suffixSplitters.toList)

    ParsePath(lst.map(urlDecode), suffix, front, back)
  }

  var fixHref = _fixHref _

  private def _fixHref(contextPath: String, v: Seq[Node], fixURL: Boolean, rewrite: Box[String => String]): Text = {
    val hv = v.text
    val updated = if (hv.startsWith("/") &&
                      !LiftRules.excludePathFromContextPathRewriting.vend(hv)) contextPath + hv else hv

    Text(if (fixURL && rewrite.isDefined &&
             !updated.startsWith("mailto:") &&
             !updated.startsWith("javascript:") &&
             !updated.startsWith("http://") &&
             !updated.startsWith("https://") &&
             !updated.startsWith("#"))
         rewrite.open_!.apply(updated) else updated)
  }

  /**
   * Corrects the HTML content,such as applying context path to URI's, session information if cookies are disabled etc.
   */
  def fixHtml(contextPath: String, in: NodeSeq): NodeSeq = {
    val rewrite = URLRewriter.rewriteFunc

    def fixAttrs(toFix: String, attrs: MetaData, fixURL: Boolean): MetaData = {
      if (attrs == Null) Null
      else if (attrs.key == toFix) {
        new UnprefixedAttribute(toFix, Req.fixHref(contextPath, attrs.value, fixURL, rewrite), fixAttrs(toFix, attrs.next, fixURL))
      } else attrs.copy(fixAttrs(toFix, attrs.next, fixURL))
    }

    def _fixHtml(contextPath: String, in: NodeSeq): NodeSeq = {
      in.map {
        v =>
        v match {
          case Group(nodes) => Group(_fixHtml(contextPath, nodes))
          case e: Elem if e.label == "form" => Elem(v.prefix, v.label, fixAttrs("action", v.attributes, true), v.scope, _fixHtml(contextPath, v.child): _*)
          case e: Elem if e.label == "script" => Elem(v.prefix, v.label, fixAttrs("src", v.attributes, false), v.scope, _fixHtml(contextPath, v.child): _*)
          case e: Elem if e.label == "a" => Elem(v.prefix, v.label, fixAttrs("href", v.attributes, true), v.scope, _fixHtml(contextPath, v.child): _*)
          case e: Elem if e.label == "link" => Elem(v.prefix, v.label, fixAttrs("href", v.attributes, false), v.scope, _fixHtml(contextPath, v.child): _*)
          case e: Elem => Elem(v.prefix, v.label, fixAttrs("src", v.attributes, true), v.scope, _fixHtml(contextPath, v.child): _*)
          case _ => v
        }
      }
    }
    _fixHtml(contextPath, in)
  }

  private[liftweb] def defaultCreateNotFound(in: Req) =
  XhtmlResponse((<html> <body>The Requested URL {in.contextPath + in.uri} was not found on this server</body> </html>),
                LiftRules.docType.vend(in), List("Content-Type" -> "text/html; charset=utf-8"), Nil, 404, S.ieMode)

  def unapply(in: Req): Option[(List[String], String, RequestType)] = Some((in.path.partPath, in.path.suffix, in.requestType))
}

case class ParamCalcInfo(paramNames: List[String],
            params: Map[String, List[String]],
            uploadedFiles: List[FileParamHolder],
            body: Box[Array[Byte]])

/**
 * Contains request information
 */
@serializable
class Req(val path: ParsePath,
          val contextPath: String,
          val requestType: RequestType,
          val contentType: Box[String],
          val request: HTTPRequest,
          val nanoStart: Long,
          val nanoEnd: Long,
          private[http] val paramCalculator: () => ParamCalcInfo,
          private[http] val addlParams: Map[String, String]) extends HasParams
{
  override def toString = "Req(" + paramNames + ", " + params + ", " + path +
  ", " + contextPath + ", " + requestType + ", " + contentType + ")"

  /**
   * Returns true if the content-type is text/xml
   */
  def xml_? = contentType != null && contentType.dmap(false)(_.toLowerCase.startsWith("text/xml"))

  def json_? = contentType != null && contentType.dmap(false){
    _.toLowerCase match {
      case x if x.startsWith("text/json") => true
      case x if x.startsWith("application/json") => true
      case _ => false
    }
  }

  def snapshot = {
    val paramCalc = paramCalculator()
    new Req(path,
     contextPath,
     requestType,
     contentType,
     request.snapshot,
     nanoStart,
     nanoEnd,
     () => paramCalc,
     addlParams)
  }
  val section = path(0) match {case null => "default"; case s => s}
  val view = path(1) match {case null => "index"; case s@_ => s}

  val id = pathParam(0)

  def pathParam(n: Int) = head(path.wholePath.drop(n + 2), "")

  def path(n: Int): String = head(path.wholePath.drop(n), null)

  def param(n: String): Box[String] =
    params.get(n) match {
    case Some(s :: _) => Full(s)
    case _ => Empty
  }

  lazy val headers: List[(String, String)] =
  for (h <- request.headers;
       p <- h.values
  ) yield (h name, p)


  def headers(name: String): List[String] = headers.filter(_._1.equalsIgnoreCase(name)).map(_._2)

  def header(name: String): Box[String] = headers(name) match {
    case x :: _ => Full(x)
    case _ => Empty
  }

  lazy val ParamCalcInfo(paramNames: List[String],
            _params: Map[String, List[String]],
            uploadedFiles: List[FileParamHolder],
            body: Box[Array[Byte]]) = paramCalculator()

  lazy val params: Map[String, List[String]] = addlParams.foldLeft(_params){
    case (map, (key, value)) => map + (key -> (value :: map.getOrElse(key, Nil)))
  }

  lazy val cookies = request.cookies match {
    case null => Nil
    case ca => ca.toList
  }

  lazy val json: Box[JsonAST.JValue] = 
    if (!json_?) Empty
    else try {
      import _root_.java.io._
      body.map(b => JsonParser.parse(new InputStreamReader(new ByteArrayInputStream(b))))
    } catch {
      case e: Exception => Failure(e.getMessage, Full(e), Empty)
    }

  private def containerRequest = Box !! request
    /**
   * The hostname to which the request was sent. This is taken from the "Host" HTTP header, or if that
   * does not exist, the DNS name or IP address of the server.
   */
  lazy val hostName: String = containerRequest.map(_.serverName) openOr Req.localHostName

  /**
   * The host and path of the request up to and including the context path. This does
   * not include the template path or query string.
   */
  lazy val hostAndPath: String =
    containerRequest.map(r => (r.scheme, r.serverPort) match {
      case ("http", 80) => "http://" + r.serverName + contextPath
      case ("https", 443) => "https://" + r.serverName + contextPath
      case (sch, port) => sch + "://" + r.serverName + ":" + port + contextPath
    }) openOr ""


  lazy val xml: Box[Elem] = if (!xml_?) Empty
  else 
    try {
      import _root_.java.io._
      body.map(b => XML.load(new ByteArrayInputStream(b)))
    } catch {
      case e: Exception => Failure(e.getMessage, Full(e), Empty)
    }

  lazy val location: Box[Loc[_]] = LiftRules.siteMap.flatMap(_.findLoc(this))

  def testLocation: Either[Boolean, Box[LiftResponse]] = {
    if (LiftRules.siteMap.isEmpty) Left(true)
    else location.map(_.testAccess) match {
      case Full(Left(true)) => Left(true)
      case Full(Right(Full(resp))) =>
        object theResp extends RequestVar(resp.apply())
        Right(Full(theResp.is))
      case _ => Right(Empty)
    }
  }


  lazy val buildMenu: CompleteMenu = location.map(_.buildMenu) openOr
  CompleteMenu(Nil)

  def createNotFound: LiftResponse = 
    NamedPF((this, Empty), LiftRules.uriNotFound.toList) match {
      case DefaultNotFound => Req.defaultCreateNotFound(this)
      case NotFoundAsResponse(resp) => resp
      case NotFoundAsNode(node) => LiftRules.convertResponse((node, 404),
        S.getHeaders(LiftRules.defaultHeaders((node, this))),
        S.responseCookies,
        this)
    }

  def createNotFound(f: Failure): LiftResponse = 
    NamedPF((this, Full(f)), LiftRules.uriNotFound.toList) match {
      case DefaultNotFound => Req.defaultCreateNotFound(this)
      case NotFoundAsResponse(resp) => resp
      case NotFoundAsNode(node) => LiftRules.convertResponse((node, 404),
        S.getHeaders(LiftRules.defaultHeaders((node, this))),
        S.responseCookies,
        this)
    }


  private[http] def createNotFound(f: (ParsePath) => Box[LiftResponse]): Box[LiftResponse] = 
    NamedPF((this, Empty), LiftRules.uriNotFound.toList) match {
      case DefaultNotFound => Full(Req.defaultCreateNotFound(this))
      case NotFoundAsResponse(resp) => Full(resp)
      case NotFoundAsTemplate(path) => 
         val newReq = new Req(path, 
                              this.contextPath, 
                              this.requestType, 
                              this.contentType, 
                              this.request,
                              this.nanoStart, 
                              this.nanoEnd, 
                              this.paramCalculator, 
                              this.addlParams)
         S.withReq(newReq) {
          f(path)
         }
      case NotFoundAsNode(node) => Full(LiftRules.convertResponse((node, 404),
        S.getHeaders(LiftRules.defaultHeaders((node, this))),
        S.responseCookies,
        this))
    }
  
  def post_? = requestType.post_?

  def get_? = requestType.get_?

  def put_? = requestType.put_?

  def fixHtml(in: NodeSeq): NodeSeq = Req.fixHtml(contextPath, in)

  lazy val uri: String = request match {
    case null => "Outside HTTP Request (e.g., on Actor)"
    case request =>
      val ret = for {uri <- Box.legacyNullTest(request.uri)
                     cp <- Box.legacyNullTest(contextPath)
                     part <- (request.uri.length >= cp.length) match {
                              case true => Full(request.uri.substring(cp.length))
                              case _ => Empty}} yield {
           part match {
            case "" => "/"
            case x => Req.fixURI(x)
          }
      }
      ret openOr "/"
  }

  /**
   * The IP address of the request
   */
  def remoteAddr: String = request.remoteAddress

  /**
   * Parse the if-modified-since header and return the milliseconds since epoch
   * of the parsed date.
   */
  lazy val ifModifiedSince: Box[java.util.Date] =
  for{req <- Box !! request
      ims <- req.header("if-modified-since")
      id <- boxParseInternetDate(ims)
  } yield id

  def testIfModifiedSince(when: Long): Boolean = (when == 0L) ||
  ((when / 1000L) > ((ifModifiedSince.map(_.getTime) openOr 0L) / 1000L))

  def testFor304(lastModified: Long, headers: (String, String)*): Box[LiftResponse] =
  if (!testIfModifiedSince(lastModified))
  Full(InMemoryResponse(new Array[Byte](0), headers.toList, Nil, 304))
  else
  Empty

  /**
   * The user agent of the browser that sent the request
   */
  lazy val userAgent: Box[String] =
  for (r <- Box.legacyNullTest(request);
       uah <- request.header("User-Agent"))
  yield uah

  lazy val isIE6: Boolean = (userAgent.map(_.indexOf("MSIE 6") >= 0)) openOr false
  lazy val isIE7: Boolean = (userAgent.map(_.indexOf("MSIE 7") >= 0)) openOr false
  lazy val isIE8: Boolean = (userAgent.map(_.indexOf("MSIE 8") >= 0)) openOr false
  lazy val isIE = isIE6 || isIE7 || isIE8

  lazy val isSafari2: Boolean = (userAgent.map(s => s.indexOf("Safari/") >= 0 &&
                                               s.indexOf("Version/2.") >= 0)) openOr false

  lazy val isSafari3: Boolean = (userAgent.map(s => s.indexOf("Safari/") >= 0 &&
                                               s.indexOf("Version/3.") >= 0)) openOr false
  lazy val isSafari4: Boolean = (userAgent.map(s => s.indexOf("Safari/") >= 0 &&
                                               s.indexOf("Version/4.") >= 0)) openOr false
  lazy val isSafari5: Boolean = (userAgent.map(s => s.indexOf("Safari/") >= 0 &&
                                               s.indexOf("Version/5.") >= 0)) openOr false

  def isSafari3_+ = isSafari3 || isSafari4 || isSafari5
  def isSafari = isSafari2 || isSafari3_+

  lazy val isIPhone = isSafari && (userAgent.map(s => s.indexOf("(iPhone;") >= 0) openOr false)

  lazy val isFirefox2: Boolean = (userAgent.map(_.indexOf("Firefox/2") >= 0)) openOr false
  lazy val isFirefox3: Boolean = (userAgent.map(_.indexOf("Firefox/3") >= 0)) openOr false
  lazy val isFirefox35: Boolean = (userAgent.map(_.indexOf("Firefox/3.5") >= 0)) openOr false
  lazy val isFirefox36: Boolean = (userAgent.map(_.indexOf("Firefox/3.6") >= 0)) openOr false
  lazy val isFirefox40: Boolean = (userAgent.map(_.indexOf("Firefox/4") >= 0)) openOr false

  def isFirefox35_+ : Boolean = isFirefox35 || isFirefox36  || isFirefox40

  def isFirefox = isFirefox2 || isFirefox3 || isFirefox35_+

  lazy val isChrome2 = (userAgent.map(_.indexOf("Chrome/2.") >= 0)) openOr false
  lazy val isChrome3 = (userAgent.map(_.indexOf("Chrome/3.") >= 0)) openOr false
  lazy val isChrome4 = (userAgent.map(_.indexOf("Chrome/4.") >= 0)) openOr false
  lazy val isChrome5 = (userAgent.map(_.indexOf("Chrome/5.") >= 0)) openOr false
  lazy val isChrome6 = (userAgent.map(_.indexOf("Chrome/6.") >= 0)) openOr false

  def isChrome3_+ = isChrome3 || isChrome4 || isChrome5 || isChrome6

  def isChrome = isChrome2 || isChrome3 || isChrome4 || isChrome5 || isChrome6

  lazy val isOpera9: Boolean = (userAgent.map(s => s.indexOf("Opera/9.") >= 0) openOr false)

  def isOpera = isOpera9

  lazy val acceptsJavaScript_? = {
    request.headers.filter(_.name.toLowerCase == "accept").
    find(h => h.values.find(s =>
        s.toLowerCase.indexOf("text/javascript") >= 0 ||
        s.toLowerCase.indexOf("application/javascript") >= 0 ||
        s.toLowerCase.indexOf("*/*") >= 0
      ).isDefined).isDefined
  }

  def updateWithContextPath(uri: String): String = if (uri.startsWith("/")) contextPath + uri else uri
}

case class RewriteRequest(path: ParsePath, requestType: RequestType, httpRequest: HTTPRequest)
case class RewriteResponse(path: ParsePath, params: Map[String, String], stopRewriting: Boolean)

/**
 * The representation of an URI path
 */
@serializable
case class ParsePath(partPath: List[String], suffix: String, absolute: Boolean, endSlash: Boolean) {
  def drop(cnt: Int) = ParsePath(partPath.drop(cnt), suffix, absolute, endSlash)

  lazy val wholePath = if (suffix.length > 0) partPath.dropRight(1) ::: List(partPath.last + "." + suffix)
  else partPath
}

/**
 * Maintains the context of resolving the URL when cookies are disabled from container. It maintains
 * low coupling such as code within request processing is not aware of the actual response that
 * ancodes the URL.
 */
object RewriteResponse {
  def apply(path: List[String], params: Map[String, String]) = new RewriteResponse(ParsePath(path, "", true, false), params, false)

  def apply(path: List[String]) = new RewriteResponse(ParsePath(path, "", true, false), Map.empty, false)

  def apply(path: List[String], suffix: String) = new RewriteResponse(ParsePath(path, suffix, true, false), Map.empty, false)

  def apply(path: ParsePath, params: Map[String, String]) = new RewriteResponse(path, params, false)
}

object URLRewriter {
  private val funcHolder = new ThreadGlobal[(String) => String]

  def doWith[R](f: (String) => String)(block: => R): R = {
    funcHolder.doWith(f) {
      block
    }
  }

  def rewriteFunc: Box[(String) => String] = Box.legacyNullTest(funcHolder value)
}

}
}
