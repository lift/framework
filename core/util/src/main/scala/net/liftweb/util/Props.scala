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
package util

import java.net.InetAddress
import java.util.Properties
import java.io.InputStream
import Helpers._
import common._

/**
 * Configuration management utilities.
 *
 * If you want to provide a configuration file for a subset of your application
 * or for a specific environment, Lift expects configuration files to be named
 * in a manner relating to the context in which they are being used. The standard
 * name format is:
 *
 * <pre>
 *   modeName.userName.hostName.props
 *
 *   examples:
 *   dpp.yak.props
 *   test.dpp.yak.props
 *   production.moose.props
 *   staging.dpp.props
 *   test.default.props
 *   default.props
 * </pre>
 *
 * with hostName and userName being optional, and modeName being one of
 * "test", "staging", "production", "pilot", "profile", or "default".
 * The standard Lift properties file extension is "props".
 */
object Props extends Logger {
  /**
   * Get the configuration property value for the specified key.
   * @param name key for the property to get
   * @return the value of the property if defined
   */
  def get(name: String): Box[String] = Box(props.get(name))

  // def apply(name: String): String = props(name)

  def getInt(name: String): Box[Int] = get(name).map(toInt) // toInt(props.get(name))
  def getInt(name: String, defVal: Int): Int = getInt(name) openOr defVal // props.get(name).map(toInt(_)) getOrElse defVal
  def getLong(name: String): Box[Long] = props.get(name).flatMap(asLong)
  def getLong(name: String, defVal: Long): Long = getLong(name) openOr defVal // props.get(name).map(toLong(_)) getOrElse defVal
  def getBool(name: String): Box[Boolean] = props.get(name).map(toBoolean) // (props.get(name))
  def getBool(name: String, defVal: Boolean): Boolean = getBool(name) openOr defVal // props.get(name).map(toBoolean(_)) getOrElse defVal
  def get(name: String, defVal: String) = props.get(name) getOrElse defVal

  /**
   * Determine whether the specified properties exist.
   * @param what the properties to test
   * @return the subset of strings in 'what' that do not correspond to
   * keys for available properties.
   */
  def require(what: String*) = what.filter(!props.contains(_))

  /**
   * Ensure that all of the specified properties exist; throw an exception if
   * any of the specified values are not keys for available properties.
   */
  def requireOrDie(what: String*) {
    require(what :_*).toList match {
      case Nil =>
      case bad => throw new Exception("The following required properties are not defined: "+bad.mkString(","))
    }
  }

  /**
   * Enumeration of available run modes.
   */
  object RunModes extends Enumeration {
    val Development = Value(1, "Development")
    val Test = Value(2, "Test")
    val Staging = Value(3, "Staging")
    val Production = Value(4, "Production")
    val Pilot = Value(5, "Pilot")
    val Profile = Value(6, "Profile")
  }

  import RunModes._

  val propFileName = "lift.props"

  val fileName = "lift.props"

  /**
   * The mode for which to retrieve properties, retrieved by System.getProperty("run.mode").
   * Recognized modes are "development", "test", "profile", "pilot", "staging" and "production"
   * with the default run mode being development.
   */
  lazy val mode: RunModes.Value = {
    runModeInitialised = true
    Box.legacyNullTest((System.getProperty("run.mode"))).map(_.toLowerCase) match {
      case Full("test") => Test
      case Full("production") => Production
      case Full("staging") => Staging
      case Full("pilot") => Pilot
      case Full("profile") => Profile
      case Full("development") => Development
      case _ => (autoDetectRunModeFn.get)()
    }
  }

  @volatile private[util] var runModeInitialised: Boolean = false

  /**
   * Exposes a property affecting run-mode determination, for customisation. If the property is modified
   * after the run-mode is realised, then it will have no effect and will instead log a warning indicating thus.
   *
   * @param name The property name (used to make logging messages clearer, no functional impact).
   */
  class RunModeProperty[T](name: String, initialValue: T) extends Logger {
    @volatile private[this] var value = initialValue

    def get = value

    /**
     * Attempts to set the property to a new value.
     *
     * @return Whether the new property was installed. `false` means modification is no longer allowed.
     */
    def set(newValue: T): Boolean =
      if (allowModification) {
        value = newValue
        true
      } else {
        onModificationProhibited()
        false
      }

    def allowModification = !runModeInitialised

    def onModificationProhibited() {
      warn("Setting property " + name + " has no effect. Run mode already initialised to " + mode + ".")
    }
  }

  /**
   * The default run-mode auto-detection routine uses this function to infer whether Lift is being run in a test.
   *
   * This routine can be customised by calling `set` before the run-mode is referenced. (An attempt to customise this
   * after the run-mode is realised will have no effect and will instead log a warning.)
   */
  val doesStackTraceContainKnownTestRunner = new RunModeProperty[Array[StackTraceElement] => Boolean]("doesStackTraceContainKnownTestRunner",
    (st: Array[StackTraceElement]) => {
      val names = List(
        "org.apache.maven.surefire.booter.SurefireBooter",
        "sbt.TestRunner",
        "org.jetbrains.plugins.scala.testingSupport.scalaTest.ScalaTestRunner",
        "org.scalatest.tools.Runner",
        "org.scalatest.tools.ScalaTestFramework$ScalaTestRunner",
        "org.scalatools.testing.Runner",
        "org.scalatools.testing.Runner2",
        "org.specs2.runner.TestInterfaceRunner", // sometimes specs2 runs tests on another thread
        "org.specs2.runner.TestInterfaceConsoleReporter",
        "org.specs2.specification.FragmentExecution"
      )
      st.exists(e => names.exists(e.getClassName.startsWith))
    })

  /**
   * When the `run.mode` environment variable isn't set or recognised, this function is invoked to determine the
   * appropriate mode to use.
   *
   * This logic can be customised by calling `set` before the run-mode is referenced. (An attempt to customise this
   * after the run-mode is realised will have no effect and will instead log a warning.)
   */
  val autoDetectRunModeFn = new RunModeProperty[() => RunModes.Value]("autoDetectRunModeFn", () => {
    val st = Thread.currentThread.getStackTrace
    if ((doesStackTraceContainKnownTestRunner.get)(st))
      Test
    else
      Development
  })

  /**
   * Is the system running in production mode (apply full optimizations)
   */
  lazy val productionMode: Boolean = mode == RunModes.Production ||
  mode == RunModes.Pilot || mode == RunModes.Staging

  /**
   * Is the system running in development mode
   */
  lazy val devMode: Boolean = mode == RunModes.Development

  /**
   * Is the system running in test mode
   */
  lazy val testMode: Boolean = mode == RunModes.Test

  /**
   * The resource path segment corresponding to the current mode.
   */
  lazy val modeName = mode match {
    case Test => "test"
    case Staging => "staging"
    case Production => "production"
    case Pilot => "pilot"
    case Profile => "profile"
    case _ => ""
  }

  private lazy val _modeName = dotLen(modeName)

  private def dotLen(in: String): String = in match {
    case null | "" => in
    case x => x+"."
  }

  /**
   * The resource path segment corresponding to the current system user
   * (from System.getProperty("user.name"))
   */
  lazy val userName = System.getProperty("user.name")

  private lazy val _userName = dotLen(userName)

  /**
   * Is the app running in the Google App engine (the System property in.gae.j is set)
   */
  lazy val inGAE: Boolean = System.getProperty("in.gae.j") != null

  /**
   * The resource path segment corresponding to the system hostname.
   */
  lazy val hostName: String = (if (inGAE) "GAE" else Helpers.tryo(InetAddress.getLocalHost.getHostName).openOr("localhost"))

  private lazy val _hostName = dotLen(hostName)

  /**
   * The list of paths to search for property file resources.
   * Properties files may be found at either the classpath root or
   * in /props
   */
  lazy val toTry: List[() => String] = List(
    () => "/props/" + _modeName + _userName + _hostName,
      () => "/props/" + _modeName + _userName,
      () => "/props/" + _modeName + _hostName,
      () => "/props/" + _modeName + "default.",
      () => "/" + _modeName + _userName + _hostName,
      () => "/" + _modeName + _userName,
      () => "/" + _modeName + _hostName,
      () => "/" + _modeName + "default.")

  /**
   * This is a function that returns the first places to look for a props file.
   * The function returns a List of String -> () => Box[InputStream].
   * So, if you want to consult System.getProperties to look for a properties file or
   * some such, you can set the whereToLook function in your Boot.scala file
   * <b>before</b> you call anything else in Props.
   */
  @volatile var whereToLook: () => List[(String, () => Box[InputStream])] = () => Nil


  /**
   * The map of key/value pairs retrieved from the property file.
   */
  lazy val props: Map[String, String] = {
    import java.io.{ByteArrayInputStream}
    import java.util.InvalidPropertiesFormatException
    import java.util.{Map => JMap}

    var tried: List[String] = Nil

    trace("Loading properties. Active run.mode is %s".format(if (modeName=="") "(Development)" else modeName))

    def vendStreams: List[(String, () => Box[InputStream])] = whereToLook() :::
    toTry.map{
      f => {
        val name = f() + "props"
        name -> {() =>
          val res = tryo{getClass.getResourceAsStream(name)}.filter(_ ne null)
          trace("Trying to open resource %s. Result=%s".format(name, res))
          res
        }
      }
    }

    // find the first property file that is available
    first(vendStreams){
      case (str, streamBox) =>
        tried ::= str
        for {
          stream <- streamBox()
        } yield {
          val ret = new Properties
          val ba = Helpers.readWholeStream(stream)
          try {
            ret.loadFromXML(new ByteArrayInputStream(ba))
            debug("Loaded XML properties from resource %s".format(str))
          } catch {
            case _: InvalidPropertiesFormatException =>
              ret.load(new ByteArrayInputStream(ba))
              debug("Loaded key/value properties from resource %s".format(str))
          }
          ret
        }
    } match {
      // if we've got a propety file, create name/value pairs and turn them into a Map
      case Full(prop) =>
        Map(prop.entrySet.toArray.flatMap{
          case s: JMap.Entry[_, _] => List((s.getKey.toString, s.getValue.toString))
          case _ => Nil
        } :_*)

      case _ =>
        error("Failed to find a properties file (but properties were accessed).  Searched: "+tried.reverse.mkString(", "))
        Map()
    }
  }
}

