/*
 * Copyright 2009-2011 Lift Committers and Contributors
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
package provider
package servlet

import jakarta.servlet.{ServletContext}
import java.net.URL
import java.io.InputStream

import net.liftweb.http.provider._
import net.liftweb.common._
import net.liftweb.util._
import Helpers._

class HTTPServletContext(val ctx: ServletContext) extends HTTPContext {
  def path: String = ctx.getContextPath

  def resource(path: String): URL = ctx getResource path

  def resourceAsStream(path: String): InputStream = ctx getResourceAsStream path

  def mimeType(path: String) = Box !! ctx.getMimeType(path)

  def initParam(name: String): Box[String] = Box !! ctx.getInitParameter(name)

  def initParams: List[(String, String)] = enumToList[String](ctx.getInitParameterNames.asInstanceOf[java.util.Enumeration[String]]).
          map(n => (n, initParam(n) openOr ""))

  def attribute(name: String): Box[Any] = Box !! ctx.getAttribute(name)

  def attributes: List[(String, Any)] = enumToList[String](ctx.getAttributeNames.asInstanceOf[java.util.Enumeration[String]]).
          map(n => (n, attribute(n) openOr ""))

  def setAttribute(name: String, value: Any): Unit = {
    ctx.setAttribute(name, value)
  }

  def removeAttribute(name: String): Unit = {
    ctx.removeAttribute(name)
  }

}

