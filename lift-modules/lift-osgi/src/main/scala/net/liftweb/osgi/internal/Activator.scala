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
package osgi {
package internal {

import common.{ Box, Full, Logger }
import http.LiftFilter
import util.{ ClassHelpers, Slf4jLogBoot }

import java.util.{List => JList}
import java.util.concurrent.atomic.AtomicReference
import javax.servlet.http.{ HttpServletRequest, HttpServletResponse }
import org.ops4j.pax.swissbox.extender._
import org.ops4j.pax.web.service.WebContainer
import org.osgi.framework.{ Bundle, BundleActivator, BundleContext }
import org.osgi.service.http.HttpContext
import org.scalamodules.core.{ Adding, Modified, Removed }
import org.scalamodules.core.Preamble._

/**
 * <p>This activator enables OSGi support for Lift.</p>
 * <p>First logging is switched to SLF4J, because Log4j does not work in
 * OSGi space. Second a Lift extender is created watching Lift-powered
 * bundles (<i>Lift-Config</i> manifest header). Third a Lift HttpContext is
 * created delegating requests for resources to the Lift-powered bundles.
 * Finally a modified LiftFilter (no booting) is registered.</p>
 */
class Activator extends BundleActivator with Logger {

  override def start(context: BundleContext) {
    // Log4j is a bad OSGi citizen!
    Slf4jLogBoot.enable()

    // Create and start Lift extender
    bundleWatcher = new BundleWatcher[ManifestEntry](
      context,
      new BundleManifestScanner(new RegexKeyManifestFilter("Lift-Config")),
      LiftBundleObserver)
    bundleWatcher.start()
    debug("Lift extender started.")

    // Register resources and LiftFilter
    context track classOf[WebContainer] on {
      case Adding(webContainer, _) =>
        if ((webContainerHolder getAndSet webContainer) == null) {
          val liftHttpContext = LiftHttpContext(webContainer.createDefaultHttpContext)
          webContainer.registerResources("/", "/", liftHttpContext)
          webContainer.registerFilter(OsgiLiftFilter, Array("/*"), null, null, liftHttpContext)
          debug("WebContainer service showed up => LiftFilter and resources registered.")
        }
      case Removed(webContainer, _) =>
        webContainerHolder.compareAndSet(webContainer, null)
        debug("WebContainer service disappeared.")
    }
  }

  override def stop(context: BundleContext) {
    // Unregister resources and LiftFilter
    webContainerHolder.get match {
      case null         => // Nothing!
      case webContainer => {
        webContainer unregisterFilter OsgiLiftFilter
        webContainer unregister "/"
        debug("LiftFilter and resources unregistered from WebContainer.")
      }
    }

    // Stop Lift extender
    bundleWatcher.stop()
    debug("Lift extender stopped.")
  }

  private val webContainerHolder = new AtomicReference[WebContainer]

  private var bundleWatcher: BundleWatcher[ManifestEntry] = _
}

/**
 * Observer for a Lift-powered bundle.
 */
private object LiftBundleObserver extends BundleObserver[ManifestEntry] with Logger {

  type LiftBundles = Map[Bundle, LiftBundleConfig]

  val liftBundles = new AtomicReference[LiftBundles](Map.empty)

  override def addingEntries(bundle: Bundle, entries: JList[ManifestEntry]) {
    assert(1 == entries.size, "Expecting exactly one manifest entry!")

    // Create config
    val config = LiftBundleConfig(entries get 0)

    // Boot
    val clazz = bundle loadClass "bootstrap.liftweb.Boot"
    val invoker = ClassHelpers.createInvoker("boot", clazz.newInstance.asInstanceOf[AnyRef])
    invoker map { f => f() }
    debug("Lift-powered bundle " + bundle.getSymbolicName + " booted.")

    // Add config
    update { liftBundle => liftBundle + (bundle -> config) }
    info("Lift-powered bundle " + bundle.getSymbolicName + " added.")
  }

  override def removingEntries(bundle: Bundle, entries: JList[ManifestEntry]) {
    assert(1 == entries.size, "Expecting exactly one manifest entry!")

    // Remove config
    update { liftBundle => liftBundle - bundle }
    info("Lift-powered bundle " + bundle.getSymbolicName + " removed.")

    // Unboot
    // TODO: Unboot!
  }

  private def update[T](change: LiftBundles => LiftBundles) {
    val old = liftBundles.get
    if (!liftBundles.compareAndSet(old, change(old))) update(change)
  }
}

/**
 * Special LiftFilter for lift-osgi bundle: Set OsgiBootable.
 */
private object OsgiLiftFilter extends LiftFilter {
  override def bootLift(loader : Box[String]) {
    super.bootLift(Full(classOf[OsgiBootable].getName))
  }
}

/**
 * Configuration of a Lift-powered bundle.
 */
private case class LiftBundleConfig(manifestEntry: ManifestEntry) {
  assert(manifestEntry != null, "ManifestEntry must not be null!")

  // TODO: Parse manifest entry and initialize webapp
  def mapResource(s: String) = ("/webapp/" + s).replaceAll("//", "/")
}

/**
 * Special HttpContext that delegates resource lookups to observerd
 * Lift-powered bundles and other methods to wrapped HttpContext.
 */
private case class LiftHttpContext(context: HttpContext) extends HttpContext with Logger {
  assert(context != null, "HttpContext must not be null!")

  override def getMimeType(s: String) = context getMimeType s

  override def getResource(s: String) = {
    debug("""Asked for resource "%s".""" format s)
    val liftBundles = LiftBundleObserver.liftBundles.get.toSeq.projection
    // TODO: The following probably could be done better!
    liftBundles flatMap {
      liftBundle => liftBundle._1 getResource (liftBundle._2 mapResource s) match {
        case null => None
        case res  =>
          debug("""Lift-powerer bundle "%s" answered for resource "%s".""".format(liftBundle._1.getSymbolicName, s))
          Some(res)
      }
    } headOption match {
      case None      => null
      case Some(res) => res
    }
  }

  override def handleSecurity(req: HttpServletRequest, res: HttpServletResponse) =
    context.handleSecurity(req, res)
}

}
}
}
