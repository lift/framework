/*
 * Copyright 2007-2011 Lift Committers and Contributors
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

import net.liftweb.common._
import net.liftweb.util._
import java.util.{Locale, ResourceBundle}
import Helpers._

/**
 * Implement this trait in order to integrate Lift with other underlaying web containers. Not necessarily JEE containers.
 */
trait HTTPProvider {
  private var actualServlet: LiftServlet = _

  def liftServlet = actualServlet

  // Logger needs to be lazy to delay creation of logger until after boot. User can have changed the logging config
  private lazy val logger = Logger(classOf[HTTPProvider])

  protected def context: HTTPContext

  /**
   * Call this from your implementation when the application terminates.
   */
  protected def terminate: Unit = {
    if (actualServlet != null) {
      actualServlet.destroy
      actualServlet = null
    }
  }

  /**
   * Call this function in order for Lift to process this request
   * @param req - the request object
   * @param resp - the response object
   * @param chain - function to be executed in case this request is supposed to not be processed by Lift
   */
  protected def service(req: HTTPRequest, resp: HTTPResponse)(chain: => Unit) = {
    tryo {
      LiftRules.early.toList.foreach(_(req))
    }

    CurrentHTTPReqResp.doWith(req -> resp) {
      val newReq = Req(req, LiftRules.statelessRewrite.toList,
        Nil,
        LiftRules.statelessReqTest.toList,
        System.nanoTime)

      CurrentReq.doWith(newReq) {
        URLRewriter.doWith(url =>
          NamedPF.applyBox(resp.encodeUrl(url),
            LiftRules.urlDecorate.toList) openOr
            resp.encodeUrl(url)) {
          if (!(isLiftRequest_?(newReq) &&
            actualServlet.service(newReq, resp))) {
            chain
          }
        }
      }
    }
  }

  /**
   * Executes Lift's Boot and makes necessary initializations
   */
  protected def bootLift(loader: Box[String]): Unit = {
      try
      {
        val b: Bootable = loader.map(b => Class.forName(b).getDeclaredConstructor().newInstance().asInstanceOf[Bootable]) openOr DefaultBootstrap
        preBoot()
        b.boot()
      } catch {
        // The UnavailableException is the idiomatic way to tell a Java application container that
        // the boot process has gone horribly, horribly wrong. That _must_ bubble to the application
        // container that is invoking the app. See https://github.com/lift/framework/issues/1843
        case unavailableException: jakarta.servlet.UnavailableException =>
          logger.error("------------------------------------------------------------------")
          logger.error("------------------------------------------------------------------")
          logger.error("------------------------------------------------------------------")
          logger.error("------------------------------------------------------------------")
          logger.error("********** Failed to Boot! An UnavailableException was thrown and all futher boot activities are aborted", unavailableException);
          logger.error("------------------------------------------------------------------")
          logger.error("------------------------------------------------------------------")
          logger.error("------------------------------------------------------------------")
          logger.error("------------------------------------------------------------------")
          logger.error("------------------------------------------------------------------")

          throw unavailableException

        case e: Exception =>
          logger.error("------------------------------------------------------------------")
          logger.error("------------------------------------------------------------------")
          logger.error("------------------------------------------------------------------")
          logger.error("------------------------------------------------------------------")
          logger.error("********** Failed to Boot! Your application may not run properly", e);
          logger.error("------------------------------------------------------------------")
          logger.error("------------------------------------------------------------------")
          logger.error("------------------------------------------------------------------")
          logger.error("------------------------------------------------------------------")
          logger.error("------------------------------------------------------------------")
      } finally {
        postBoot

        actualServlet = new LiftServlet(context)
        actualServlet.init
      }
    }

  private def preBoot(): Unit = {
    // do this stateless
    LiftRules.statelessDispatch.prepend(NamedPF("Classpath service") {
      case r@Req(mainPath :: subPath, suffx, _) if (mainPath == LiftRules.resourceServerPath) =>
        ResourceServer.findResourceInClasspath(r, r.path.wholePath.drop(1))
    })
  }

  private def postBoot: Unit = {
    if (!LiftRules.logServiceRequestTiming) {
      LiftRules.installServiceRequestTimer(NoOpServiceTimer)
    }
    try {
      ResourceBundle getBundle (LiftRules.liftCoreResourceName)
    } catch {
      case _: Exception => logger.error("LiftWeb core resource bundle for locale " + Locale.getDefault() + ", was not found ! ")
    } finally {
      LiftRules.bootFinished()
    }
  }


  private def liftHandled(in: String): Boolean = (in.indexOf(".") == -1) || in.endsWith(".html") || in.endsWith(".xhtml") ||
          in.endsWith(".htm") ||
          in.endsWith(".xml") || in.endsWith(".liftjs") || in.endsWith(".liftcss")

  /**
   * Tests if a request should be handled by Lift or passed to the container to be executed by other potential filters or servlets.
   */
  protected def isLiftRequest_?(session: Req): Boolean = {
    NamedPF.applyBox(session, LiftRules.liftRequest.toList) match {
      case Full(b) => b
      case _ => session.path.endSlash ||
              (session.path.wholePath.takeRight(1) match {
                case Nil => true
                case x :: xs => liftHandled(x)
              }) ||
              context.resource(session.uri) == null
    }
  }
}
