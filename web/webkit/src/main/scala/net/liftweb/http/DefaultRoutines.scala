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
package http {

import _root_.java.util.{ResourceBundle, Locale}
import common._
import util._

/**
 * Many routines in Lift can be customized in LiftRules,
 * but have default behaviors.  This singleton contains
 * the default routines.  <b>**DO NOT**</b> call these methods
 * directly.  Use LiftRules to call them.  So, why make them
 * public?  So, we can document the default behaviors.
 */
object DefaultRoutines {
  private val resourceMap: LRUMap[(String, List[String]),
                                  Box[ResourceBundle]] = 
                                    new LRUMap(2000)

  private def rawResBundle(loc: Locale, path: List[String]): Box[ResourceBundle] = {
    val realPath = path match {
      case Nil => List("_resources")
      case x => x
    }

    for {
      xml <- TemplateFinder.findAnyTemplate(realPath, loc) or
      TemplateFinder.findAnyTemplate("templates-hidden" :: realPath, loc) or
      TemplateFinder.findAnyTemplate(realPath.dropRight(1) :::
                                     ("resources-hidden" ::
                                      realPath.takeRight(1)), loc)

      bundle <- BundleBuilder.convert(xml, loc)
    } yield bundle
  }

  private def resBundleFor(loc: Locale, path: List[String]): Box[ResourceBundle] =
    resourceMap.synchronized {
      val key = loc.toString -> path
      resourceMap.get(key) match {
        case Full(x) => x
        case _ => {
          val res = rawResBundle(loc, path)
          if (!Props.devMode) resourceMap(key) = res
          res
        }
          
      }
    }
    
  /**
   * Returns the resources for the current request.  In development
   * mode, these resources will be reloaded on each request.  In
   * production mode, the resources will be cached (up to 2000
   * resource bundles will be cached). <br/><br/>
   *
   * The resource bundles will be loaded using TemplateFinder and converted
   * via BundleBuilder.  The routine will search for resources given the
   * current Locale (see S.locale).  If the current path is /foo/bar,
   * the files /foo/_resources_bar, /templates-hidden/foo/_resources_bar,
   * and /foo/resources-hidden/_resources_bar will be searched.  The
   * search will be based on the TemplateFinder locale search rules. <br/><br/>
   *
   * In addition to page-specific resources, there are global resources searched
   * in /_resources, /templates-hidden/_resources, and
   * /resources-hidden/_resources.  <br/><br/>
   *
   * This resource loading mechanism offers global and per-page
   * localization.  It's based on the template loading mechanism
   * so that localization is stored in UTF-8 and as XML so there's no
   * wacky encoding or compilation as is necessary with standard Java
   * resource bundles.  Further, the per-page resources are available
   * right next to the pages themselves in the source tree, so it's
   * easier to remember to update the localization.
   *
   * @see S.locale
   * @see TemplateFinder.findAnyTemplate
   * @see BundleBuilder.convert
   * 
   */
  def resourceForCurrentReq(): List[ResourceBundle] = {
    val loc = S.locale
    val cb =
      for {
        req <- S.request
        path = req.path.partPath.dropRight(1) :::
        req.path.partPath.takeRight(1).map(s => "_resources_"+s)
        bundle <- resBundleFor(loc, path)
      } yield bundle

    cb.toList ::: resBundleFor(loc, Nil).toList
  }
}

}
}
