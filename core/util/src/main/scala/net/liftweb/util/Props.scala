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
package util {

import _root_.java.net.InetAddress
import _root_.java.util.Properties
import _root_.java.io.InputStream
import Helpers._
import common._

/**
 * Configuration management utilities.
 *
 * If you want to provide a configuration file for a subset of your application
 * or for a specifig environment, Lift expects configuration files to be named
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
  lazy val mode = {
    Box.legacyNullTest((System.getProperty("run.mode"))).map(_.toLowerCase) match {
      case Full("test") => Test
      case Full("production") => Production
      case Full("staging") => Staging
      case Full("pilot") => Pilot
      case Full("profile") => Profile
      case Full("development") => Development
      case _ => {
        val exp = new Exception
        exp.fillInStackTrace
        if (exp.getStackTrace.find(st => st.getClassName.indexOf("SurefireBooter") >= 0).isDefined) Test
        else if (exp.getStackTrace.find(st => st.getClassName.indexOf("sbt.TestRunner") >= 0).isDefined) Test
        else Development
      }
    }
  }

  /**
   * Is the system in production mode (apply full optimizations)
   */
  lazy val productionMode: Boolean = mode == RunModes.Production ||
  mode == RunModes.Pilot || mode == RunModes.Staging

  /**
   * Is the system in production mode (apply full optimizations)
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
  lazy val hostName: String = (if (inGAE) "GAE" else InetAddress.getLocalHost.getHostName)

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
  import _root_.java.io.{ByteArrayInputStream}
  import _root_.java.util.InvalidPropertiesFormatException
  import _root_.java.util.{Map => JMap}

  var tried: List[String] = Nil

  def vendStreams: List[(String, () => Box[InputStream])] = whereToLook() :::
    toTry.map{
      f => {
        val name = f() + "props"
        name -> (() => tryo{getClass.getResourceAsStream(name)}.filter(_ ne null))
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
      } catch {
        case _: InvalidPropertiesFormatException =>
          ret.load(new ByteArrayInputStream(ba))
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

}
}
