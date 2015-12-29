/*
 * Copyright 2016 WorldWide Conferencing, LLC
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

import common._
import net.liftweb.util._
import Helpers._

import java.net.{URL, URLConnection, JarURLConnection}
import java.util.concurrent.{ConcurrentHashMap => CHash}

trait ResourceCache {

  private[this] val lastModCache: CHash[String, Long] = new CHash()

  protected def calcLastModified(in: URL): Long = {
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
  protected def detectContentType(path: String): String = {
    // Configure response with content type of resource
    (LiftRules.context.mimeType(path) or
            (Box !! URLConnection.getFileNameMap().getContentTypeFor(path))) openOr
            "application/octet-stream"
  }
}
