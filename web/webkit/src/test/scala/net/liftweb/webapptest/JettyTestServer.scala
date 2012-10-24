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

import org.mortbay.jetty.Server
import org.mortbay.jetty.webapp.WebAppContext

import net.sourceforge.jwebunit.junit.WebTester
import junit.framework.AssertionFailedError

import common.Box


final class JettyTestServer(baseUrlBox: Box[URL]) {

  def baseUrl = baseUrlBox getOrElse new URL("http://127.0.0.1:8080")

  private val (server_, context_) = {
    val server = new Server(baseUrl.getPort)
    val context = new WebAppContext()
    context.setServer(server)
    context.setContextPath("/")
    val dir = System.getProperty("net.liftweb.webapptest.src.test.webapp", "src/test/webapp")
    context.setWar(dir)
    //val context = new Context(_server, "/", Context.SESSIONS)
    //context.addFilter(new FilterHolder(new LiftFilter()), "/");
    server.addHandler(context)
    server.setGracefulShutdown(100)
    server.setStopAtShutdown(true)
    (server, context)
  }

  def urlFor(path: String) = baseUrl + path

  def start() {
    server_.start()
  }

  def stop() {
    context_.setShutdown(true)
    server_.stop()
    server_.join()
  }

  def running = server_.isRunning

  def browse[A](startPath: String, f:(WebTester) => A): A = {
    val wc = new WebTester()
    try {
      wc.setScriptingEnabled(false)
      wc.beginAt(urlFor(startPath))
      f(wc)
    } catch {
      case exc: AssertionFailedError => {
        System.err.println("server response: ", wc.getServerResponse)
        throw exc
      }
    } finally {
      wc.closeBrowser()
    }
  }

}

