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
package webapptest

import java.net.URL

import org.eclipse.jetty.server.Server
import org.eclipse.jetty.webapp.WebAppContext

import org.htmlunit.WebClient
import org.htmlunit.html.HtmlPage

import common.{Box, Full}


final class JettyTestServer(baseUrlBox: Box[URL]) {

  def baseUrl = baseUrlBox getOrElse new URL("http://127.0.0.1:8080")

  private val (server_, context_) = {
    val server = new Server(baseUrl.getPort)
    val context = new WebAppContext()
    context.setServer(server)
    context.setContextPath("/")
    val dir = System.getProperty("net.liftweb.webapptest.src.test.webapp", "src/test/webapp")
    context.setWar(dir)
    server.setHandler(context)
    server.setStopTimeout(100)
    server.setStopAtShutdown(true)
    (server, context)
  }

  def urlFor(path: String) = baseUrl + path

  def start() {
    server_.start()
    // Lift is a filter; getParts() on a filter-dispatched request requires
    // multipart configuration on the servlet that owns the request (the
    // container's default servlet here). commons-fileupload bypassed the
    // container and needed none; getParts() does — this is the migration
    // requirement flagged in docs/migration/javax-to-jakarta.adoc. The
    // default servlet is registered when the context parses web.xml at
    // start, so the configuration is applied after start.
    Option(context_.getServletHandler.getServlet("default")).foreach { holder =>
      holder.getRegistration.setMultipartConfig(new jakarta.servlet.MultipartConfigElement(""))
    }
  }

  def stop() {
    context_.stop()
    server_.stop()
    server_.join()
  }

  def running = server_.isRunning

  /**
   * Browse to `startPath` with HtmlUnit and run `f` on the parsed page.
   * Scripting is disabled: these tests assert on server-rendered markup,
   * not on client-side behavior. The web client is closed after each call.
   */
  def browseHtmlUnit[A](startPath: String, f: HtmlPage => A): A = {
    val client = new WebClient()
    client.getOptions.setJavaScriptEnabled(false)
    client.getOptions.setCssEnabled(false)
    try {
      val page = client.getPage[HtmlPage](urlFor(startPath))
      f(page)
    } finally {
      client.close()
    }
  }

}

