package net.liftweb.http.provider.servlet

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
