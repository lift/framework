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
package webapptest {

import _root_.org.mortbay.jetty.Server
//import _root_.org.mortbay.jetty.servlet.Context
import _root_.org.mortbay.jetty.servlet.ServletHolder
import _root_.org.mortbay.jetty.webapp.WebAppContext

import _root_.net.sourceforge.jwebunit.junit.WebTester
import _root_.junit.framework.AssertionFailedError

object JettyTestServer {
  private val serverPort_ = System.getProperty("SERVLET_PORT", "8989").toInt
  private var baseUrl_ = "http://localhost:" + serverPort_

  def baseUrl = baseUrl_

  private val server_ : Server = {
    val server = new Server(serverPort_)
    val context = new WebAppContext()
    context.setServer(server)
    context.setContextPath("/")
    val dir = System.getProperties().getProperty("net.liftweb.webapptest.src.test.webapp", "src/test/webapp")
    context.setWar(dir)
    //val context = new Context(_server, "/", Context.SESSIONS)
    //context.addFilter(new FilterHolder(new LiftFilter()), "/");
    server.addHandler(context)
    server
  }

  def urlFor(path: String) = baseUrl_ + path

  def start() = server_.start()

  def stop() = {
    server_.stop()
    server_.join()
  }

  def browse(startPath: String, f:(WebTester) => Unit) = {
    val wc = new WebTester()
    try {
      wc.setScriptingEnabled(false)
      wc.beginAt(JettyTestServer.urlFor(startPath))
      f(wc)
    } catch {
      case exc: AssertionFailedError => {
        System.err.println("serveur response: ", wc.getServeurResponse())
        throw exc
      }
    } finally {
      wc.closeBrowser()
    }
  }

}

}
}
