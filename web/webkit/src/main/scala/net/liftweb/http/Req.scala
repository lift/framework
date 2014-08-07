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

import java.io.{InputStream, ByteArrayInputStream, File, FileInputStream,
                       FileOutputStream}
import scala.xml._

import common._
import json._
import util._
import Helpers._
import http.provider._
import sitemap._


object UserAgentCalculator extends Factory {
  /**
   * The default regular expression for IE
   */
  val iePattern = """(MSIE ([0-9]+)|Trident/7.*rv:([0-9]+))""".r

  /**
   * You can change the mechanism by which the user agent for IE
   * is calculated by doing the Factory thing with this object
   */
  object ieCalcFunction extends FactoryMaker[Box[String] => 
    Box[Double]](defaultIeCalcFunction _)

  /**
   * The built-in mechanism for calculating IE
   */
  def defaultIeCalcFunction(userAgent: Box[String]): Box[Double] = 
    for {
      userAgent <- userAgent
      ieMatch = iePattern.pattern.matcher(userAgent)
      findResult = ieMatch.find if findResult
      ieVersionString <- Box.legacyNullTest(ieMatch.group(2)) or Box.legacyNullTest(ieMatch.group(3))
      ver <- Helpers.asDouble(ieVersionString)
    } yield ver

  /**
   * The default regular expression for Safari
   */
  val safariPattern = """Version.([0-9]+)[.0-9]+ ([^S])*Safari\/""".r

  /**
   * You can change the mechanism by which the user agent for Safari
   * is calculated by doing the Factory thing with this object
   */
  object safariCalcFunction extends FactoryMaker[Box[String] => 
    Box[Double]](defaultSafariCalcFunction _)

  /**
   * The built-in mechanism for calculating Safari
   */
  def defaultSafariCalcFunction(userAgent: Box[String]): Box[Double] = 
    for {
      ua <- userAgent
      m = safariPattern.pattern.matcher(ua)
      ver <- if (m.find) Helpers.asDouble(m.group(1)) else Empty
    } yield ver

  /**
   * The default regular expression for Firefox
   */
  val firefoxPattern = """Firefox.([1-9][0-9]*\.[0-9])""".r

  /**
   * You can change the mechanism by which the user agent for Firefox
   * is calculated by doing the Factory thing with this object
   */
  object firefoxCalcFunction extends FactoryMaker[Box[String] => 
    Box[Double]](defaultFirefoxCalcFunction _)

  /**
   * The built-in mechanism for calculating Firefox
   */
  def defaultFirefoxCalcFunction(userAgent: Box[String]): Box[Double] = 
    for {
      ua <- userAgent
      m = firefoxPattern.pattern.matcher(ua)
      ver <- if (m.find) Helpers.asDouble(m.group(1)) else Empty
    } yield ver

  /**
   * The default regular expression for Chrome
   */
  val chromePattern = """Chrome.([1-9][0-9]*\.[0-9])""".r

  /**
   * You can change the mechanism by which the user agent for Chrome
   * is calculated by doing the Factory thing with this object
   */
  object chromeCalcFunction extends FactoryMaker[Box[String] => 
    Box[Double]](defaultChromeCalcFunction _)

  /**
   * You can change the mechanism by which Lift calculates
   * if the User-Agent represents an iPhone.  Put your
   * special calculation function in here
   */
  object iPhoneCalcFunction extends FactoryMaker[Box[Box[String] => 
    Boolean]](Empty)

  /**
   * You can change the mechanism by which Lift calculates
   * if the User-Agent represents an iPad.  Put your
   * special calculation function in here
   */
  object iPadCalcFunction extends FactoryMaker[Box[Box[String] => 
    Boolean]](Empty)


  /**
   * The built-in mechanism for calculating Chrome
   */
  def defaultChromeCalcFunction(userAgent: Box[String]): Box[Double] = 
    for {
      ua <- userAgent
      m = chromePattern.pattern.matcher(ua)
      ver <- if (m.find) Helpers.asDouble(m.group(1)) else Empty
    } yield ver
}

trait UserAgentCalculator {
  lazy val ieVersion: Box[Int] = UserAgentCalculator.ieCalcFunction.vend.apply(userAgent).map(_.toInt)

  lazy val isIE6: Boolean = ieVersion.map(_ == 6) openOr false
  lazy val isIE7: Boolean = ieVersion.map(_ == 7) openOr false
  lazy val isIE8: Boolean = ieVersion.map(_ == 8) openOr false
  lazy val isIE9: Boolean = ieVersion.map(_ == 9) openOr false
  lazy val ieIE10: Boolean = ieVersion.map(_ == 10) openOr false
  lazy val isIE11: Boolean = ieVersion.map(_ == 11) openOr false
  lazy val isIE = ieVersion.isDefined

  lazy val safariVersion: Box[Int] = 
    UserAgentCalculator.safariCalcFunction.vend.apply(userAgent).map(_.toInt)

  def isSafari2: Boolean = false

  lazy val isSafari3: Boolean = safariVersion.map(_ == 3) openOr false
  lazy val isSafari4: Boolean = safariVersion.map(_ == 4) openOr false
  lazy val isSafari5: Boolean = safariVersion.map(_ == 5) openOr false
  
  def isSafari3_+ = safariVersion.map(_ >= 3) openOr false
  def isSafari = safariVersion.isDefined

  /**
   * Is the Req coming from an iPhone
   */
  lazy val isIPhone: Boolean = 
    UserAgentCalculator.iPhoneCalcFunction.vend.
  map(_.apply(userAgent)) openOr 
    isSafari && (userAgent.map(s => 
      s.indexOf("(iPhone") >= 0) openOr false)

  /**
   * Is the Req coming from an iPad
   */
  lazy val isIPad: Boolean = 
    UserAgentCalculator.iPadCalcFunction.vend.
  map(_.apply(userAgent)) openOr 
  isSafari && (userAgent.map(s =>
    s.indexOf("(iPad") >= 0) openOr false)

  lazy val firefoxVersion: Box[Double] = 
    UserAgentCalculator.firefoxCalcFunction.vend.apply(userAgent)

  lazy val isFirefox2: Boolean = firefoxVersion.map(v => v >= 2d && v < 3d) openOr false
  lazy val isFirefox3: Boolean = firefoxVersion.map(v => v >= 3d && v < 3.5d) openOr false
  lazy val isFirefox35: Boolean = firefoxVersion.map(v => v >= 3.5d && v < 3.6d) openOr false
  lazy val isFirefox36: Boolean = firefoxVersion.map(v => v >= 3.6d && v < 4d) openOr false
  lazy val isFirefox40: Boolean = firefoxVersion.map(v => v >= 4d) openOr false

  def isFirefox35_+ : Boolean = firefoxVersion.map(_ >= 3.5d) openOr false

  def isFirefox = firefoxVersion.isDefined


  lazy val chromeVersion: Box[Double] = 
    UserAgentCalculator.chromeCalcFunction.vend.apply(userAgent)

  lazy val isChrome2 = chromeVersion.map(v => v >= 2d && v < 3d) openOr false
  lazy val isChrome3 = chromeVersion.map(v => v >= 3d && v < 4d) openOr false
  lazy val isChrome4 = chromeVersion.map(v => v >= 4d && v < 5d) openOr false
  lazy val isChrome5 = chromeVersion.map(v => v >= 5d && v < 6d) openOr false
  lazy val isChrome6 = chromeVersion.map(v => v >= 6d && v < 7d) openOr false

  def isChrome3_+ = chromeVersion.map(_ >= 3d) openOr false

  def isChrome = chromeVersion.isDefined

  lazy val isOpera9: Boolean = (userAgent.map(s => s.indexOf("Opera/9.") >= 0) openOr false)

  def isOpera = isOpera9

  /**
   * What's the user agent?
   */
  def userAgent: Box[String]
}

sealed trait ParamHolder extends Serializable{
  def name: String
}

final case class NormalParamHolder(name: String, value: String) extends ParamHolder

/**
 * A FileParamHolder contains a file uploaded via a multipart
 * form.
 *
 * @param name The name of the form field for this file
 * @param mimeType the mime type, as specified in the Content-Type field
 * @param fileName The local filename on the client
 */
abstract class FileParamHolder(val name: String, val mimeType: String,
                               val fileName: String) extends ParamHolder with Serializable
{
  /**
   * Returns the contents of the uploaded file as a Byte array.
   */
  def file: Array[Byte]

  /**
   * Returns an input stream that can be used to read the
   * contents of the uploaded file.
   */
  def fileStream: InputStream

  /**
   * Returns the length of the uploaded file.
   */
  def length : Long
}

/**
 * This FileParamHolder stores the uploaded file directly into memory.
 *
 * @param name The name of the form field for this file
 * @param mimeType the mime type, as specified in the Content-Type field
 * @param fileName The local filename on the client
 * @param file The contents of the uploaded file in a byte array
 */
class InMemFileParamHolder(override val name: String, override val mimeType: String,
                           override val fileName: String, val file: Array[Byte]) extends
FileParamHolder(name, mimeType, fileName)
{
  /**
   * Returns an input stream that can be used to read the
   * contents of the uploaded file.
   */
  def fileStream: InputStream = new ByteArrayInputStream(file)

  /**
   * Returns the length of the uploaded file.
   */
  def length : Long = if (file == null) 0 else file.length
}

/**
 * This FileParamHolder stores the uploaded file in a
 * temporary file on disk.
 *
 * @param name The name of the form field for this file
 * @param mimeType the mime type, as specified in the Content-Type field
 * @param fileName The local filename on the client
 * @param localFile The local copy of the uploaded file
 */
class OnDiskFileParamHolder(override val name: String, override val mimeType: String,
                            override val fileName: String, val localFile: File) extends
FileParamHolder(name, mimeType, fileName)
{
  /**
   * Returns an input stream that can be used to read the
   * contents of the uploaded file.
   */
  def fileStream: InputStream = new FileInputStream(localFile)

  /**
   * Returns the contents of the uploaded file as a Byte array.
   */
  def file: Array[Byte] = Helpers.readWholeStream(fileStream)

  /**
   * Returns the length of the uploaded file.
   */
  def length : Long = if (localFile == null) 0 else localFile.length

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
 * A Thread-global containing the current Req.  Set very, very early
 */
object CurrentReq extends ThreadGlobal[Req]

/**
 * A ThreadGlobal containing the low-level HTTPRequest and HTTPResponse instances
 * Set very, very early.
 */
object CurrentHTTPReqResp extends ThreadGlobal[(HTTPRequest, HTTPResponse)]

private final case class AvoidGAL(func: () => ParamCalcInfo) {
  lazy val thunk: ParamCalcInfo = func()
}

/**
 * Helper object for constructing Req instances
 */
object Req {
  object NilPath extends ParsePath(Nil, "", true, false)

  private[http] lazy val localHostName = {
    import java.net._
    InetAddress.getLocalHost.getHostName
  }

  def apply(original: Req, rewrite: List[LiftRules.RewritePF]): Req = 
    this.apply(original, rewrite, Nil, Nil)

  def apply(original: Req, rewrite: List[LiftRules.RewritePF], statelessTest: List[LiftRules.StatelessTestPF],
            otherStatelessTest: List[LiftRules.StatelessReqTestPF]): Req = {

    def processRewrite(path: ParsePath, params: Map[String, String]): RewriteResponse =
    NamedPF.applyBox(RewriteRequest(path, original.requestType, original.request), rewrite) match {
      case Full(resp@RewriteResponse(_, _, true)) => resp
      case _: EmptyBox => RewriteResponse(path, params)
      case Full(resp) => processRewrite(resp.path, params ++ resp.params)
    }

    val rewritten = processRewrite(original.path, Map.empty)

    val wholePath = rewritten.path.wholePath


    val stateless =  NamedPF.applyBox(StatelessReqTest(wholePath, original.request), otherStatelessTest) or
      NamedPF.applyBox(wholePath, statelessTest)

    new Req(rewritten.path, original.contextPath, 
            original.requestType, original.contentType, original.request,
            original.nanoStart, original.nanoEnd, 
            stateless openOr original.stateless_?,
            original.paramCalculator, original.addlParams ++ rewritten.params)
  }

  def apply(request: HTTPRequest, rewrite: List[LiftRules.RewritePF],  nanoStart: Long): Req =
    this.apply(request, rewrite, Nil, Nil, nanoStart)


  def apply(request: HTTPRequest, rewrite: List[LiftRules.RewritePF], statelessTest: List[LiftRules.StatelessTestPF],
            otherStatelessTest: List[LiftRules.StatelessReqTestPF], nanoStart: Long): Req = {
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
            
            val names: List[String] = params.map(_._1).distinct
      val nvp: Map[String, List[String]] = params.foldLeft(Map[String, List[String]]()) {
        case (map, (name, value)) => map + (name -> (map.getOrElse(name, Nil) ::: List(value)))
      }
      
      (names, nvp)
    }

    // make this a thunk so it only gets calculated once
    val paramCalcInfo: AvoidGAL = new AvoidGAL(() => {
      // post/put of XML or JSON... eagerly read the stream
      if ((reqType.post_? ||
           reqType.put_?) && contentType.dmap(false){
	_.toLowerCase match {
	  case x => 
	    x.startsWith("text/xml") || 
	    x.startsWith("application/xml") || 
	  x.startsWith("text/json") ||
	  x.startsWith("application/json")
	}}) {
        ParamCalcInfo(queryStringParam._1, 
                      queryStringParam._2 ++ localParams, 
                      Nil, 
                      Full(BodyOrInputStream(request.inputStream)))
        // it's multipart
      } else if (request.multipartContent_?) {
        val allInfo = request.extractFiles
        
        val normal: List[NormalParamHolder] = 
          allInfo.flatMap {
            case v: NormalParamHolder => List(v)
            case _ => Nil}

        val files: List[FileParamHolder] = allInfo.flatMap {
          case v: FileParamHolder => List(v)
          case _ => Nil}
        
        val params = normal.foldLeft(eMap)((a, b) =>
          a + (b.name -> (a.getOrElse(b.name, Nil) ::: List(b.value))))
        
        ParamCalcInfo((queryStringParam._1 ::: 
                       normal.map(_.name)).distinct,
                      queryStringParam._2 ++ localParams ++
                      params, files, Empty)
        // it's a GET
      } else if (reqType.get_?) {
        ParamCalcInfo(queryStringParam._1,
                      queryStringParam._2 ++ localParams, Nil, Empty)
      } else if (contentType.dmap(false)(_.toLowerCase.
                                         startsWith("application/x-www-form-urlencoded"))) {
        val params = localParams ++ (request.params.sortWith
                                     {(s1, s2) => s1.name < s2.name}).
                                           map(n => (n.name, n.values))
        ParamCalcInfo(request.paramNames, params, Nil, Empty)
      } else {
        ParamCalcInfo(queryStringParam._1, 
                      queryStringParam._2 ++ localParams, 
                      Nil, Full(BodyOrInputStream(request.inputStream)))
      }
    })


    val paramCalculator: () => ParamCalcInfo = () => {
      paramCalcInfo.thunk
    }

    val wholePath = rewritten.path.wholePath

    val stateless =  NamedPF.applyBox(StatelessReqTest(wholePath, request), otherStatelessTest) or
      NamedPF.applyBox(wholePath, statelessTest)

    new Req(rewritten.path, contextPath, reqType,
            contentType, request, nanoStart,
            System.nanoTime, stateless openOr false, paramCalculator, Map())
  }

  private def fixURI(uri: String) = uri indexOf ";jsessionid" match {
    case -1 => uri
    case x@_ => uri.substring(0, x)
  }

  /**
   * Create a nil request... useful for testing
   */
  def nil = new Req(NilPath, "", GetRequest, Empty, null,
                    System.nanoTime, System.nanoTime, false,
                    () => ParamCalcInfo(Nil, Map.empty, Nil, Empty), Map()) {
    override lazy val standardRequest_? = false
  }

  def parsePath(in: String): ParsePath = {
    val p1 = fixURI((in match {case null => "/"; case s if s.length == 0 => "/"; case s => s}).replaceAll("/+", "/"))
    val front = p1.startsWith("/")
    val back = p1.length >= 1 && p1.endsWith("/")

    val orgLst = p1.replaceAll("/$", "/index").split("/").
    toList.map(_.trim).filter(_.length > 0)

    val (lst, suffix) = NamedPF(orgLst, LiftRules.suffixSplitters.toList)

    ParsePath(lst.map(urlDecode), suffix, front, back)
  }

  var fixHref = _fixHref _

  private def _fixHref(contextPath: String, v: Seq[Node], fixURL: Boolean, rewrite: Box[String => String]): Text = {
    val hv = v.text
    val updated = if (hv.startsWith("/") &&
                      !hv.startsWith("//") &&
                      !LiftRules.excludePathFromContextPathRewriting.vend(hv)) contextPath + hv else hv

    Text(if (fixURL && rewrite.isDefined &&
             !updated.startsWith("mailto:") &&
             !updated.startsWith("javascript:") &&
             !updated.startsWith("http://") &&
             !updated.startsWith("https://") &&
             !updated.startsWith("//") &&
             !updated.startsWith("#"))
         rewrite.openOrThrowException("legacy code").apply(updated) else updated)
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
          case e: Elem if e.label == "form" => Elem(v.prefix, v.label, fixAttrs("action", v.attributes, true), v.scope, e.minimizeEmpty, _fixHtml(contextPath, v.child): _*)
          case e: Elem if e.label == "script" => Elem(v.prefix, v.label, fixAttrs("src", v.attributes, false), v.scope, e.minimizeEmpty, _fixHtml(contextPath, v.child): _*)
          case e: Elem if e.label == "a" => Elem(v.prefix, v.label, fixAttrs("href", v.attributes, true), v.scope, e.minimizeEmpty, _fixHtml(contextPath, v.child): _*)
          case e: Elem if e.label == "link" => Elem(v.prefix, v.label, fixAttrs("href", v.attributes, false), v.scope, e.minimizeEmpty, _fixHtml(contextPath, v.child): _*)
          case e: Elem => Elem(v.prefix, v.label, fixAttrs("src", v.attributes, true), v.scope, e.minimizeEmpty, _fixHtml(contextPath, v.child): _*)
          case _ => v
        }
      }
    }
    _fixHtml(contextPath, in)
  }

  private[liftweb] def defaultCreateNotFound(in: Req) =
  XhtmlResponse((<html> <body>The Requested URL {in.contextPath + in.uri} was not found on this server</body> </html>),
                LiftRules.htmlProperties.vend(in).docType, List("Content-Type" -> "text/html; charset=utf-8"), Nil, 404, S.legacyIeCompatibilityMode)

  def unapply(in: Req): Option[(List[String], String, RequestType)] = Some((in.path.partPath, in.path.suffix, in.requestType))
}

/**
 * Holds either the body or the request input stream, depending on which was requested first
 */
final case class BodyOrInputStream(is: InputStream) {
  private var calc = false
  lazy val body: Box[Array[Byte]] = synchronized {
    if (calc) Empty
    else {
      calc = true
      tryo(readWholeStream(is))
    }
  }

  lazy val stream: Box[InputStream] = synchronized {
    if (calc) Empty
    else {
      calc = true
      Full(is)
    }
  }
}

final case class ParamCalcInfo(paramNames: List[String],
                               params: Map[String, List[String]],
                               uploadedFiles: List[FileParamHolder],
                               body: Box[BodyOrInputStream])


/**
 * Holds information about the content type and subtype including
 * the q parameter and extension information.
 */
final case class ContentType(theType: String, 
                             subtype: String, 
                             order: Int,
                             q: Box[Double], 
                             extension: List[(String, String)]) extends Ordered[ContentType]
  {
    /**
     * Compares this to another ContentType instance based on the q
     * and if the q matches, compare based on specialization (* vs.
     * explicit and then order.
     */
    def compare(that: ContentType): Int = ((that.q openOr 1d) compare (q openOr 1d)) match {
      case 0 => 
        def doDefault = {
          order compare that.order
        }

        (theType, that.theType, subtype, that.subtype) match {
          case ("*", "*", _, _) => doDefault
          case ("*", _, _, _) => 1
          case (_, "*", _, _) => -1
          case (_, _, "*", "*") => doDefault
          case (_, _, "*", _) => 1
          case (_, _, _, "*") => -1
          case _ => doDefault
        }
      case x => x
    }

    /**
     * Does this ContentType match the String including
     * wildcard support
     */
    def matches(contentType: (String, String)): Boolean =
      (theType == "*" || (theType == contentType._1)) &&
    (subtype == "*" || subtype == contentType._2)
    
    /**
     * Is it a wildcard
     */
    def wildCard_? = theType == "*" && subtype == "*"
  }

/**
 * The ContentType companion object that has helper methods
 * for parsing Accept headers and other things that
 * contain multiple ContentType information.
 */
object ContentType {
  /**
   * Parse the String into a series of ContentType instances,
   * returning the multiple ContentType instances
   */
  def parse(str: String): List[ContentType] = 
    (for {
      (part, index) <- str.charSplit(',').
      map(_.trim).zipWithIndex // split at comma
      content <- parseIt(part, index)
    } yield content).sortWith(_ < _)

  private object TwoType {
    def unapply(in: String): Option[(String, String)] = 
      in.charSplit('/') match {
        case a :: b :: Nil => Some(a -> b)
        case _ => None
      }
  }


  private object EqualsSplit {
    private def removeQuotes(s: String) = 
      if (s.startsWith("\"") && s.endsWith("\"")) s.substring(1, s.length - 1)
      else s

    def unapply(in: String): Option[(String, String)] = in.roboSplit("=") match {
      case a :: b :: Nil => Some(a -> removeQuotes(b))
      case _ => None
    }
  }

  private def parseIt(content: String, index: Int): Box[ContentType] = content.roboSplit(";") match {
    case TwoType(typ, subType) :: xs => {
      val kv = xs.flatMap(EqualsSplit.unapply) // get the key/value pairs
      val q: Box[Double] = first(kv){
        case (k, v) if k == "q" => Helpers.asDouble(v)
        case _ => Empty
      }
      Full(ContentType(typ, subType, index, q, kv.filter{_._1 != "q"}))
    }

    case _ => Empty
  }
}



/**
 * Contains request information
 */
class Req(val path: ParsePath,
          val contextPath: String,
          val requestType: RequestType,
          val contentType: Box[String],
          val request: HTTPRequest,
          val nanoStart: Long,
          val nanoEnd: Long,
          _stateless_? : Boolean,
          private[http] val paramCalculator: () => ParamCalcInfo,
          private[http] val addlParams: Map[String, String]) extends HasParams with UserAgentCalculator
{
  override def toString = "Req(" + paramNames + ", " + params + ", " + path +
  ", " + contextPath + ", " + requestType + ", " + contentType + ")"

  def this(_path: ParsePath,
           _contextPath: String,
          _requestType: RequestType,
          _contentType: Box[String],
          _request: HTTPRequest,
          _nanoStart: Long,
          _nanoEnd: Long,
          _paramCalculator: () => ParamCalcInfo,
          _addlParams: Map[String, String]) = this(_path,
                                                   _contextPath,
                                                   _requestType,
                                                   _contentType,
                                                   _request,
                                                   _nanoStart,
                                                   _nanoEnd,
                                                   false,
                                                   _paramCalculator,
                                                   _addlParams)

  /**
   * Build a new Req, the same except with a different path.
   * Useful for creating Reqs with sub-paths.
   */
  def withNewPath(newPath: ParsePath): Req = {
    val outer = this

    new Req(newPath,
            contextPath,
            requestType,
            contentType,
            request,
            nanoStart,
            nanoEnd,
            _stateless_?,
            paramCalculator,
            addlParams) {
      override lazy val json: Box[JsonAST.JValue] = outer.json

      override lazy val xml: Box[Elem] = outer.xml

      override lazy val location: Box[Loc[_]] = outer.location

      override lazy val buildMenu: CompleteMenu = outer.buildMenu

      /**
       * the accept header
       */
      override lazy val accepts: Box[String] = outer.accepts
    
      /**
       * What is the content type in order of preference by the requester
       * calculated via the Accept header
       */
      override lazy val weightedAccept: List[ContentType] = 
        outer.weightedAccept

      /**
       * Returns true if the request accepts XML
       */
      override lazy val acceptsXml_? = outer.acceptsXml_?

      /**
       * Returns true if the request accepts JSON
       */
      override lazy val acceptsJson_? = outer.acceptsJson_?

      /**
       * Is the Accepts Header * / *
       */
      override lazy val acceptsStarStar : Boolean = outer.acceptsStarStar

      /**
       * Returns true if the request accepts JavaScript
       */
      override lazy val acceptsJavaScript_? = 
        outer.acceptsJavaScript_?
    }
  }

  /**
   * Should the request be treated as stateless (no session created for it)?
   */
  lazy val stateless_? = {
    val ret = _stateless_? || (location.map(_.stateless_?) openOr false)
    ret
  }

  /**
   * Returns true if the content-type is text/xml or application/xml
   */
  def xml_? = contentType != null && contentType.dmap(false){
    _.toLowerCase match {
      case x if x.startsWith("text/xml") => true
      case x if x.startsWith("application/xml") => true
      case _ => false
    }
  }

  /**
   * Returns true if the content-type is text/json or application/json
   */
  def json_? = contentType != null && contentType.dmap(false){
    _.toLowerCase match {
      case x if x.startsWith("text/json") => true
      case x if x.startsWith("application/json") => true
      case _ => false
    }
  }

  /**
   * Returns true if the X-Requested-With header is set to XMLHttpRequest.
   *
   * Most ajax frameworks, including jQuery and Prototype, set this header
   * when doing any ajax request.
   */
  def ajax_? =
    request.headers.toList.exists { header =>
      (header.name equalsIgnoreCase "x-requested-with") &&
      (header.values.exists(_ equalsIgnoreCase "xmlhttprequest"))
    }

  /**
   * A request that is neither Ajax or Comet
   */
  lazy val standardRequest_? : Boolean = path.partPath match {
    case x :: _ if x == LiftRules.liftContextRelativePath => false
    case _ => true
  }

  /**
   * Make the servlet session go away
   */
  def destroyServletSession() {
    for {
      httpReq <- Box !! request
    } httpReq.destroyServletSession()
  }

  /**
   * A snapshot of the request that can be passed off the current thread
   *
   * @return a copy of the Req
   */
  def snapshot: Req = {
    val paramCalc = paramCalculator()
    paramCalc.body.map(_.body) // make sure we grab the body
    new Req(path,
            contextPath,
            requestType,
            contentType,
            request.snapshot,
            nanoStart,
            nanoEnd,
            stateless_?,
            () => paramCalc,
            addlParams)
  }
  val section = path(0) match {case null => "default"; case s => s}
  val view = path(1) match {case null => "index"; case s@_ => s}

  val id = pathParam(0)

  def pathParam(n: Int) = path.wholePath.drop(n + 2).headOption getOrElse ""

  def path(n: Int): String = path.wholePath.drop(n).headOption getOrElse ""

  def param(n: String): Box[String] =
    params.get(n) match {
    case Some(s :: _) => Full(s)
    case _ => Empty
  }

  lazy val headers: List[(String, String)] =
  for (h <- request.headers;
       p <- h.values
  ) yield (h.name, p)


  def headers(name: String): List[String] = headers.filter(_._1.equalsIgnoreCase(name)).map(_._2)

  def header(name: String): Box[String] = headers(name) match {
    case x :: _ => Full(x)
    case _ => Empty
  }

  /**
   * Get the name of the params
   */
  def paramNames: List[String] = _paramNames

  /**
   * the raw parameters, use params
   */
  def _params: Map[String, List[String]] = __params

  /**
   * The uploaded files
   */
  def uploadedFiles: List[FileParamHolder] = _uploadedFiles

  /**
   * The POST or PUT body.  This will be empty if the content
   * type is application/x-www-form-urlencoded or a multipart mime.
   * It will also be empty if rawInputStream is accessed
   */
  def body: Box[Array[Byte]] = _body.flatMap(_.body)

  /**
   * The raw input stream of a POST or PUT that is not
   * application/x-www-form-urlencoded or a multipart mime
   * and if this method is called before the body method.
   * Remember to close the stream when done.
   */
  def rawInputStream: Box[InputStream] = _body.flatMap(_.stream)

  private lazy val ParamCalcInfo(_paramNames /*: List[String]*/,
            __params /*: Map[String, List[String]]*/,
            _uploadedFiles /*: List[FileParamHolder]*/,
            _body /*: Box[BodyOrInputStream]*/) = {
    val ret = paramCalculator()
    ret
  }

  lazy val params: Map[String, List[String]] = addlParams.foldLeft(_params){
    case (map, (key, value)) => map + (key -> (value :: map.getOrElse(key, Nil)))
  }

  lazy val cookies = request.cookies match {
    case null => Nil
    case ca => ca.toList
  }

  /**
   * Get the session ID if there is one without creating on
   */
  def sessionId: Box[String] =
    for {
      httpRequest <- Box !! request
      sid <- httpRequest.sessionId
    } yield sid

  /**
   * The JValue representation of this Req's body, if the body is JSON-parsable
   * AND the content-type of the request is JSON. Returns a Failure if
   * the request is not considered a JSON request (see json_?), or if
   * there was an error parsing the JSON.
   *
   * If you want to forcibly evaluate the request body as JSON, ignoring
   * content type, see `forcedBodyAsJson`.
   */
  lazy val json: Box[JsonAST.JValue] = {
    if (!json_?) {
      Failure("Cannot parse non-JSON request as JSON; please check content-type.")
    } else {
      forcedBodyAsJson
    }
  }

  /**
   * Forcibly tries to parse the request body as JSON. Does not perform any
   * content type checks, unlike the json method.
   */
  lazy val forcedBodyAsJson: Box[JsonAST.JValue] = {
    try {
      import java.io._

      def r = """; *charset=(.*)""".r
      def r2 = """[^=]*$""".r

      def charSet: String = contentType.flatMap(ct => r.findFirstIn(ct).flatMap(r2.findFirstIn)).getOrElse("UTF-8")
      
      body.map(b => 
        JsonParser.parse(new 
                         InputStreamReader(new 
                                           ByteArrayInputStream(b), charSet)))
    } catch {
      case e: LiftFlowOfControlException => throw e
      case e: Exception => Failure(e.getMessage, Full(e), Empty)
    }
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
      case ("http", 80) if r.header("X-SSL").isDefined => "https://" + r.serverName + contextPath
      case ("http", 80) => "http://" + r.serverName + contextPath
      case ("https", 443) => "https://" + r.serverName + contextPath
      case (sch, port) => sch + "://" + r.serverName + ":" + port + contextPath
    }) openOr ""

  /**
   * The Elem representation of this Req's body, if the body is XML-parsable
   * AND the content-type of the request is XML. Returns a Failure if
   * the request is not considered a XML request (see xml_?), or if
   * there was an error parsing the XML.
   *
   * If you want to forcibly evaluate the request body as XML, ignoring
   * content type, see `forcedBodyAsXml`.
   */
  lazy val xml: Box[Elem] = {
    if (!xml_?) {
      Failure("Cannot parse non-XML request as XML; please check content-type.")
    } else  {
      forcedBodyAsXml
    }
  }

  /**
   * Forcibly tries to parse the request body as XML. Does not perform any
   * content type checks, unlike the xml method.
   */
  lazy val forcedBodyAsXml: Box[Elem] = {
    try {
      import java.io._
      body.map(b => XML.load(new ByteArrayInputStream(b)))
    } catch {
      case e: LiftFlowOfControlException => throw e
      case e: Exception => Failure(e.getMessage, Full(e), Empty)
    }
  }

  /**
   * The SiteMap Loc associated with this Req
   */
  lazy val location: Box[Loc[_]] = LiftRules.siteMap.flatMap(_.findLoc(this))

  /**
   * Test the current SiteMap Loc for access control to insure
   * that this Req is allowed to access the page
   */
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

  private def initIfUnitted[T](f: T): T = S.session match {
    case Full(_) => f
    case _ => S.statelessInit(this)(f)
  }


  /**
   * Compute the Not Found via a Template
   */
  private def notFoundViaTemplate(path: ParsePath): LiftResponse = {
    this.initIfUnitted {
      (for {
        session <- S.session
        template =  Templates(path.partPath)
        resp <- session.processTemplate(template, this, path, 404)
      } yield resp) match {
        case Full(resp) => resp
        case _ => Req.defaultCreateNotFound(this)
      }
    }
  }

  def createNotFound: LiftResponse = 
    NamedPF((this, Empty), LiftRules.uriNotFound.toList) match {
      case DefaultNotFound => Req.defaultCreateNotFound(this)
      case NotFoundAsTemplate(path) => notFoundViaTemplate(path)
      case NotFoundAsResponse(resp) => resp
      case NotFoundAsNode(node) => LiftRules.convertResponse((node, 404),
        S.getResponseHeaders(LiftRules.defaultHeaders((node, this))),
        S.responseCookies,
        this)
    }

  def createNotFound(f: Failure): LiftResponse = 
    NamedPF((this, Full(f)), LiftRules.uriNotFound.toList) match {
      case DefaultNotFound => Req.defaultCreateNotFound(this)
      case NotFoundAsTemplate(path) => notFoundViaTemplate(path)
      case NotFoundAsResponse(resp) => resp
      case NotFoundAsNode(node) => LiftRules.convertResponse((node, 404),
        S.getResponseHeaders(LiftRules.defaultHeaders((node, this))),
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
                              true,
                              this.paramCalculator, 
                              this.addlParams)
         S.withReq(newReq) {
          f(path)
         }
      case NotFoundAsNode(node) => Full(LiftRules.convertResponse((node, 404),
        S.getResponseHeaders(LiftRules.defaultHeaders((node, this))),
        S.responseCookies,
        this))
    }
  
  val post_? = requestType.post_?

  val get_? = requestType.get_?

  val put_? = requestType.put_?

  val options_? = requestType.options_?

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


  /**
   * the accept header
   */
  lazy val accepts: Box[String] = {
    request.headers.toList.
    filter(_.name equalsIgnoreCase "accept").flatMap(_.values) match {
      case Nil => Empty
      case xs => Full(xs.mkString(", "))
    }
  }
    
  /**
   * What is the content type in order of preference by the requester
   * calculated via the Accept header
   */
  lazy val weightedAccept: List[ContentType] = accepts match {
    case Full(a) => ContentType.parse(a)
    case _ => Nil
  }

  /**
   * Returns true if the request accepts XML
   */
  lazy val acceptsXml_? =
    (weightedAccept.find(_.matches("text" -> "xml")) orElse
     weightedAccept.find(_.matches("application" -> "xml"))).isDefined

  /**
   * Returns true if the request accepts JSON
   */
  lazy val acceptsJson_? =
    (weightedAccept.find(_.matches("text" -> "json")) orElse
     weightedAccept.find(_.matches("application" -> "json"))).isDefined

  /**
   * Is the Accepts Header * / *
   */
  lazy val acceptsStarStar : Boolean = accepts.map(_ == "*/*") openOr false

  /**
   * Returns true if the request accepts JavaScript
   */
  lazy val acceptsJavaScript_? = 
    (weightedAccept.find(_.matches("text" -> "javascript")) orElse
     weightedAccept.find(_.matches("application" -> "javascript"))).
    isDefined

  def updateWithContextPath(uri: String): String = if (uri.startsWith("/")) contextPath + uri else uri
}

/**
 * This case class is used for pattern matching.  See LiftRules.statelessRewrite and LiftRules.statefulRewrite
 */
final case class RewriteRequest(path: ParsePath, requestType: RequestType, httpRequest: HTTPRequest)

/**
 * The representation of an URI path
 */
case class ParsePath(partPath: List[String], suffix: String, absolute: Boolean, endSlash: Boolean) {
  def drop(cnt: Int) = ParsePath(partPath.drop(cnt), suffix, absolute, endSlash)

  lazy val wholePath = 
    if (suffix.length > 0) {
      partPath.dropRight(1) ::: List((partPath match {
        case Nil => ""
        case xs => xs.last}) + "." + suffix)
    } else {
      partPath
    }
}

final case class RewriteResponse(path: ParsePath, 
                                 params: Map[String, String],
                                 stopRewriting: Boolean)

/**
 * Maintains the context of resolving the URL when cookies are disabled from container. It maintains
 * low coupling such as code within request processing is not aware of the actual response that
 * encodes the URL.
 */
object RewriteResponse {
  def apply(path: List[String], params: Map[String, String]) = new RewriteResponse(ParsePath(path, "", true, false), params, false)

  def apply(path: List[String]) = new RewriteResponse(ParsePath(path, "", true, false), Map.empty, false)

  def apply(path: List[String], stopRewriting: Boolean) = 
    new RewriteResponse(ParsePath(path, "", true, false),
                        Map.empty, stopRewriting)

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

  def rewriteFunc: Box[(String) => String] = Box.legacyNullTest(funcHolder.value)
}
