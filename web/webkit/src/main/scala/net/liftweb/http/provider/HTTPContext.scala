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

import _root_.java.io.{InputStream}
import _root_.java.net.{URL}
import _root_.net.liftweb.common._
import _root_.net.liftweb.util._

/**
 * Represents the service context information. Similar with servlet context.
 */
trait HTTPContext {

  /**
   * @return - the context path. It always comes first in a request URI. It is
   *           the URI part that represent to context of the request.
   */
  def path: String

  /**
   * Returns the URL representation of a resource that is mapped by a fully qualified path.
   * The path is considered relative to the root path and it always starts with '/'.
   *
   * @param path - the resource path
   * @return - the URL object of the path
   */
  def resource(path: String): URL

  /**
   * Same as <i>resource</i> but returns an InputStream to read the resource.
   * @param path - the resource path
   * @return InputStream
   */
  def resourceAsStream(path: String): InputStream

  /**
   * @param path
   * @return - the mime type mapped to resource determined by this path.
   */
  def mimeType(path: String): Box[String]

  /**
   * @param name
   * @return - the value of the init parameter identified by then provided name. Note
   *           that this is not typesfe and you need to explicitely do the casting
   *           when reading this attribute. Returns Empty if this parameter does not exist.
   */
  def initParam(name: String): Box[String]

  /**
   * @return - a List of Tuple2 consisting of name and value pair of the init parameters
   */
  def initParams: List[(String, String)]

  /**
   * @param name
   * @return - the value of the context attribute identified by then provided name.
   *           Returns Empty if this parameter does not exist.
   */
  def attribute(name: String): Box[Any]

  /**
   * @return - a List of Tuple2 consisting of name and value pair of the attributes
   */
  def attributes: List[(String, Any)]

  /**
   * @param - name
   * @param - value. Any reference. Note that this is not typesfe and you need to explicitely do
   *          the casting when reading this attribute.
   */
  def setAttribute(name: String, value: Any): Unit

  /**
   * @param - name. The name ofthe parameter that needs to be removed.
   */
  def removeAttribute(name: String): Unit
}

}
}
}
