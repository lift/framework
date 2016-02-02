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

import scala.collection.JavaConverters._

import net.liftweb.common._
import net.liftweb.util._
import Helpers._

import org.webjars.WebJarAssetLocator

/**
 * Helper object for `org.webjars.WebJarAssetLocator`.
 */
object WebJarLocator extends Loggable {

  private lazy val locator = new WebJarAssetLocator()

  /**
   * Add 'min' to path in productionMode, but only if there isn't already a 'min'
   * as the second to last part of the path.
   */
  private[http] def calcPath(src: String, mode: Props.RunModes.RunMode = Props.mode): String = {
    val parts = src.split('.')

    if (parts.length >= 2 && parts(parts.length - 2) != "min" && Props.isProductionMode(mode)) {
      val ext = parts.last
      val main = parts.dropRight(1).mkString(".")
      s"${main}.min.${ext}"
    } else {
      src
    }
  }

  /**
   * Get the full path of an asset that will be used as a URI.
   *
   * If there is an error, it will be logged and the `src` param will be returned unchanged.
   *
   * @param src the asset to locate
   * @param checkForMin should we check for a minified version
   */
  def locateAsset(src: String, checkForMin: Boolean = true): String = {
    try {
      val p = if (checkForMin) calcPath(src) else src
      locator.getFullPath(p).replace("META-INF/resources", "")
    } catch {
      case e: IllegalArgumentException =>
        logger.error(e.getMessage)
        val available = availableAssets(src)
        if (available.nonEmpty) {
          logger.error(s"""Available paths:\n${available.mkString("\n")}""")
        }
        src
    }
  }

  private def availableAssets(src: String): Seq[String] = {
    locator.getFullPathIndex.asScala
      .filterKeys(_.startsWith(src))
      .values
      .map(_.replace("META-INF/resources", ""))
      .toSeq
  }
}
