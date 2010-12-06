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

import _root_.net.liftweb.common.{Box, Full, Empty}
import _root_.net.liftweb.util._
import Helpers._
import java.net.{URL, URLConnection, JarURLConnection}
import java.util.concurrent.{ConcurrentHashMap => CHash}

object ResourceServer {
  var allowedPaths: PartialFunction[List[String], Boolean] = {
    case "jquery.js" :: Nil => true
    case "yui" :: _ => true
    case "liftYUI.js" :: Nil => true
    case "extcore" :: _ => true
    case "liftExtCore.js" :: Nil => true
    case "json2.js" :: Nil => true
    case "json.js" :: Nil => true
    case "jlift.js" :: Nil => true
    case bp@("blueprint" :: _) if bp.last.endsWith(".css") || bp.last.endsWith(".png") => true
    case bp@("blueprint_10" :: _) if bp.last.endsWith(".css") || bp.last.endsWith(".png") => true
    case "jquery-autocomplete" :: "jquery.autocomplete.js" :: Nil => true
    case "jquery-autocomplete" :: "jquery.autocomplete.css" :: Nil => true
    case "jquery-autocomplete" :: "indicator.gif" :: Nil => true
  }

  private def rewriter = new PartialFunction[List[String], List[String]]{
    def isDefinedAt(in: List[String]) = LiftRules.jsArtifacts.pathRewriter.isDefinedAt(in)

    def apply(in: List[String]): List[String] = LiftRules.jsArtifacts.pathRewriter(in)
  }

  @volatile var pathRewriter: PartialFunction[List[String], List[String]] = rewriter orElse {
    case "json.js" :: Nil => List("json2-min.js")
    case "json2.js" :: Nil => List("json2-min.js")
    case xs => xs
  }

  /**
   * The base package for serving resources.  This way, resource names can't be spoofed
   */
  var baseResourceLocation = "toserve"

  private val lastModCache: CHash[String, Long] = new CHash()

  def calcLastModified(in: URL): Long = {
    val str = in.toString
    if (!Props.devMode && lastModCache.containsKey(str)) lastModCache.get(str)
    else {
      val ret: Long =
      (for{
        uc <- tryo(in.openConnection)
      } yield {
          uc.getLastModified match {
            case 0L => uc match {
              case jc: JarURLConnection => jc.getJarEntry() match {
                case null => 0L
                case e => e.getTime()
              }
              case _ => 0L
            }
            case x => x
          }
        }) openOr 0L
      lastModCache.put(str, ret)
      ret
    }
  }


  def findResourceInClasspath(request: Req, uri: List[String])(): Box[LiftResponse] =
    for{
      auri <- Full(uri.filter(!_.startsWith("."))).filter(auri => isAllowed(auri))
      rw = baseResourceLocation :: pathRewriter(auri)
      path = rw.mkString("/", "/", "")
      url <- LiftRules.getResource(path)
      lastModified = calcLastModified(url)
    } yield
      request.testFor304(lastModified, "Expires" -> toInternetDate(millis + 30.days)) openOr {
        val stream = url.openStream
        val uc = url.openConnection
        StreamingResponse(stream, () => stream.close, uc.getContentLength,
          (if (lastModified == 0L) Nil else
            List("Last-Modified" -> toInternetDate(lastModified))) :::
                  List("Expires" -> toInternetDate(millis + 30.days),
                       "Date" -> Helpers.nowAsInternetDate,
                       "Pragma" -> "",
                       "Cache-Control" -> "",
                       "Content-Type" -> detectContentType(rw.last)), Nil,
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

}
}
