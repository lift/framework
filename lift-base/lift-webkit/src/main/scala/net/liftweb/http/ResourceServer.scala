/*
 * Copyright 2007-2008 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */
package net.liftweb.http

import _root_.net.liftweb.common.{Box, Full, Empty}
import _root_.net.liftweb.util._
import Helpers._
import _root_.java.net.{URLConnection, JarURLConnection}

object ResourceServer {
  var allowedPaths: PartialFunction[List[String], Boolean] = {
    case "jquery.js" :: Nil => true
    case "yui" :: _ => true
    case "liftYUI.js" :: Nil => true
    case "json2.js" :: Nil => true
    case "json.js" :: Nil => true
    case "jlift.js" :: Nil => true
    case bp@("blueprint" :: _) if bp.last.endsWith(".css") || bp.last.endsWith(".png") => true
    case "jquery-autocomplete" :: "jquery.autocomplete.js" :: Nil => true
    case "jquery-autocomplete" :: "jquery.autocomplete.css" :: Nil => true
    case "jquery-autocomplete" :: "indicator.gif" :: Nil => true
  }

  var pathRewriter: PartialFunction[List[String], List[String]] = {
    //case "jquery.js" :: Nil =>  List("jquery-1.3.2.js") // List("jquery-1.3.2-min.js")
    //case "json.js" :: Nil => List( "json2.js") // List( "json2-min.js")
    //case "json2.js" :: Nil => List( "json2.js") // List( "json2-min.js")
    case "jquery.js" :: Nil => List("jquery-1.3.2-min.js")
    case "json.js" :: Nil => List("json2-min.js")
    case "json2.js" :: Nil => List("json2-min.js")
    case xs => xs
  }

  /**
   * The base package for serving resources.  This way, resource names can't be spoofed
   */
  var baseResourceLocation = "toserve"

  def calcLastModified(in: URLConnection): Long = in.getLastModified match {
    case 0L => in match {
      case jc: JarURLConnection => jc.getJarEntry() match {
        case null => 0L
        case e => e.getTime()
      }
      case _ => 0L
    }
    case x => x
  }


  def findResourceInClasspath(request: Req, uri: List[String])(): Box[LiftResponse] =
    for{
      auri <- Full(uri.filter(!_.startsWith("."))) if isAllowed(auri)
      rw = baseResourceLocation :: pathRewriter(auri)
      path = rw.mkString("/", "/", "")
      url <- LiftRules.getResource(path)
      uc <- tryo(url.openConnection)
      lastModified = calcLastModified(uc)
    } yield
      request.testFor304(lastModified, "Expires" -> toInternetDate(millis + 30.days)) openOr {
        val stream = url.openStream
        StreamingResponse(stream, () => stream.close, uc.getContentLength,
          (if (lastModified == 0L) Nil else
            List(("Last-Modified", toInternetDate(lastModified)))) :::
                  List(("Expires", toInternetDate(millis + 30.days)),
                    ("Content-Type", detectContentType(rw.last))), Nil,
          200)
      }


  /**
   * detect the Content-Type of file (path) with context-defined content-types
   * (application's web.xml or container's configuration), and fall
   * back to system or JVM-defined (FileNameMap) content types.
   * if no content-type found, then return "application/octet-stream"
   *
   * @param path Resource name to be analyzed to detect MIME type
   *
   * @see HTTPContext # mimeType ( String )
   * @see URLConnection # getFileNameMap ( )
   */
  def detectContentType(path: String): String = {
    // Configure response with content type of resource
    (LiftRules.context.mimeType(path) or
            (Box !! URLConnection.getFileNameMap().getContentTypeFor(path))) openOr
            "application/octet-stream"
  }

  private def isAllowed(path: List[String]) = allowedPaths.isDefinedAt(path) && allowedPaths(path)

  def allow(path: PartialFunction[List[String], Boolean]) {
    allowedPaths = path orElse allowedPaths
  }

  def rewrite(rw: PartialFunction[List[String], List[String]]) {
    pathRewriter = rw orElse pathRewriter
  }
}
