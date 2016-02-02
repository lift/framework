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
package builtin
package snippet

import net.liftweb.http.WebJarLocator

import net.liftweb.common._
import net.liftweb.http.S
import net.liftweb.util._
import Helpers._

/**
 * A snippet that tries to locate a WebJar asset and then replaces either
 * the `src` or `href` attribute with the full path to the asset.
 *
 * It will then be served either via a Servlet 3 container, or [[net.liftweb.http.WebJarServer]].
 * If the app is running in a Servlet 3 container, the container will take precedence over
 * [[net.liftweb.http.WebJarServer]].
 *
 * The full unminified version will be served in Development mode and the
 * minified version will be served in Production. If a file with 'min' as the
 * second to last part of the filename is used, that will be served in all modes.
 *
 * These examples:
 *
 * {{{
 * <script data-lift="WebJars.js?src=bootstrap.js"></script>
 * <link rel="stylesheet" data-lift="WebJars.css?src=bootstrap.css" />
 * <img data-lift="WebJars.img?src=sort_asc.png"></img>
 * }}}
 *
 * will produce:
 *
 * {{{
 * <script src="/webjars/bootstrap/3.3.4/dist/js/bootstrap.js"></script>
 * <link rel="stylesheet" href="/webjars/bootstrap/3.3.4/dist/css/bootstrap.css" />
 * <img src="/webjars/datatables/1.10.10/media/images/sort_asc.png"></img>
 * }}}
 *
 * If an asset cannot be located, an error will appear in the logs:
 *
 * {{{
 * [ERROR] net.liftweb.http.WebJarLocator - bootstrapx.js could not be found. Make sure you've added the corresponding WebJar and please check for typos.
 * }}}
 *
 * If there are multiple versions of an asset found, a similar error
 * will appear in the logs. It will also list the assets that were found.
 *
 * For example, jquery:
 *
 * {{{
 * <script data-lift="WebJars.js?src=jquery.js"></script>
 * }}}
 *
 * returns:
 *
 * {{{
 * [ERROR] net.liftweb.http.WebJarLocator - Multiple matches found for jquery.js. Please provide a more specific path, for example by including a version number.
 * [ERROR] net.liftweb.http.WebJarLocator - Available paths:
 * /webjars/jquery/2.2.0/dist/jquery.js
 * /webjars/jquery/2.2.0/src/jquery.js
 * }}}
 *
 * The following solves the problem:
 *
 * {{{
 * <script data-lift="WebJars.js?src=dist/jquery.js"></script>
 * }}}
 */
object WebJars extends Loggable {

  private def findPath(checkForMin: Boolean): Box[String] = S.attr("src").map { src =>
    WebJarLocator.locateAsset(src, checkForMin)
  }

  def css = "* [href]" #> findPath(true)
  def js = "* [src]" #> findPath(true)
  def img = "* [src]" #> findPath(false)
}
