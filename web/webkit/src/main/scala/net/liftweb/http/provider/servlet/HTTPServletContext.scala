/*
 * Copyright 2009-2010 WorldWide Conferencing, LLC
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
package provider {
package servlet {

import _root_.javax.servlet.{ServletContext}
import _root_.java.net.URL
import _root_.java.io.InputStream

import _root_.net.liftweb.http.provider._
import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import Helpers._

class HTTPServletContext(val ctx: ServletContext) extends HTTPContext {
  def path: String = ctx.getContextPath

  def resource(path: String): URL = ctx getResource path

  def resourceAsStream(path: String): InputStream = ctx getResourceAsStream path

  def mimeType(path: String) = Box !! ctx.getMimeType(path)

  def initParam(name: String): Box[String] = Box !! ctx.getInitParameter(name)

  def initParams: List[(String, String)] = enumToList[String](ctx.getInitParameterNames.asInstanceOf[_root_.java.util.Enumeration[String]]).
          map(n => (n, initParam(n) openOr ""))

  def attribute(name: String): Box[Any] = Box !! ctx.getAttribute(name)

  def attributes: List[(String, Any)] = enumToList[String](ctx.getAttributeNames.asInstanceOf[_root_.java.util.Enumeration[String]]).
          map(n => (n, attribute(n) openOr ""))

  def setAttribute(name: String, value: Any) {
    ctx.setAttribute(name, value)
  }

  def removeAttribute(name: String) {
    ctx.removeAttribute(name)
  }

}

}
}
}
}
