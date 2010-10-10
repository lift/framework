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
package http {
package provider {

import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import _root_.java.util.{Locale, ResourceBundle}
import Helpers._

/**
 * Implement this trait in order to integrate Lift with other underlaying web containers. Not necessarily JEE containers.
 */
trait HTTPProvider {
  private var actualServlet: LiftServlet = _

  // Logger needs to be lazy to delay creation of logger until after boot. User can have changed the logging config
  private lazy val logger = Logger(classOf[HTTPProvider])

  protected def context: HTTPContext

  /**
   * Call this from your implementation when the application terminates.
   */
  protected def terminate {
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

    val newReq = Req(req, LiftRules.statelessRewrite.toList,
                     LiftRules.statelessTest.toList, System.nanoTime)

    URLRewriter.doWith(url => NamedPF.applyBox(resp.encodeUrl(url), LiftRules.urlDecorate.toList) openOr resp.encodeUrl(url)) {
      if (!(isLiftRequest_?(newReq) && actualServlet.service(newReq, resp))) {
        chain
      }
    }
  }

  /**
   * Executes Lift's Boot and makes necessary initializations
   */
  protected def bootLift(loader: Box[String]): Unit = {
      try
      {
        val b: Bootable = loader.map(b => Class.forName(b).newInstance.asInstanceOf[Bootable]) openOr DefaultBootstrap
        preBoot
        b.boot
      } catch {
        case e =>
            logger.error("Failed to Boot! Your application may not run properly", e);
      } finally {
        postBoot

        actualServlet = new LiftServlet(context)
        actualServlet.init
      }
    }

  private def preBoot() {
    // do this stateless
    LiftRules.statelessDispatchTable.prepend(NamedPF("Classpath service") {
      case r@Req(mainPath :: subPath, suffx, _) if (mainPath == LiftRules.resourceServerPath) =>
        ResourceServer.findResourceInClasspath(r, r.path.wholePath.drop(1))
    })
  }

  private def postBoot {
    try {
      ResourceBundle getBundle (LiftRules.liftCoreResourceName)

      if (Props.productionMode && LiftRules.templateCache.isEmpty) {
        // Since we're in productin mode and user did not explicitely set any template caching, we're setting it
        LiftRules.templateCache = Full(InMemoryCache(500))
      }
    } catch {
      case _ => logger.error("LiftWeb core resource bundle for locale " + Locale.getDefault() + ", was not found ! ")
    } finally {
      LiftRules.doneBoot = true;
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
              (session.path.wholePath.takeRight(1) match
              {
                case Nil => true case x :: xs => liftHandled(x)
              }) ||
              context.resource(session.uri) == null
    }
  }

}

}
}
}
