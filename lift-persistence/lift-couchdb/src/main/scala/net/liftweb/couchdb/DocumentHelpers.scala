/*
 * Copyright 2010 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.liftweb {
package couchdb {

import _root_.scala.reflect.Manifest
import _root_.dispatch.{Http, StatusCode}
import _root_.net.liftweb.common.{Box, Failure, Full}
import Box.option2Box
import _root_.net.liftweb.json.Implicits.string2jvalue
import _root_.net.liftweb.json.JsonAST.{JArray, JDouble, JField, JInt, JObject, JString, JValue}
import _root_.net.liftweb.util.ControlHelpers.tryo

object DocumentHelpers {
  /** Force a document into the database, clobbering any existing version. Usually good for design documents. */
  def forceStore(http: Http, database: Database, doc: JObject): Box[JObject] =
    tryo(http(database(doc) fetch)) match {
      case Full(existingDoc) =>
        tryo(http(database store updateIdAndRev(doc, existingDoc._id.open_!, existingDoc._rev.open_!))).flatMap(b => b)

      case Failure(_, Full(StatusCode(404, _)), _) =>
        tryo(http(database store doc)).flatMap(b => b)

      case failure => failure.asA[JObject]
    }

  /** Strip _id and _rev from an object */
  def stripIdAndRev(in: JObject): JObject =
    JObject(in.obj.filter { case JField("_id"|"_rev", _) => false; case _ => true })
  
  /** Update a JObject with new _id and _rev fields */
  def updateIdAndRev(in: JObject, id: String, rev: String): JObject =
    JObject(JField("_id", id) :: JField("_rev", rev) ::
        in.obj.filter { case JField("_id"|"_rev", _) => false; case _ => true })

  /** Implicitly extend JObjects */
  implicit def jobjectToJObjectExtension(obj: JObject): JObjectExtension =
    new JObjectExtension(obj)

  /** Extension of JObject that has field accessing functions */
  class JObjectExtension(obj: JObject) {
    /** Full(_id) from the named field _id if present, Empty or Failure if not present */
    def _id: Box[String] = get[JString]("_id").map(_.s) 

    /** Full(_rev) from the named field _rev if present, Empty or Failure if not present */
    def _rev: Box[String] = get[JString]("_rev").map(_.s)

    /** Full(type) from the named field type if present, Empty or Failure if not present */
    def `type`: Box[String] = get[JString]("type").map(_.s)

    /** <code>true</code> iff the type field is present and equal to the given predicate value */
    def isA(s: String) = `type`.map(_ == s) openOr false

    /** Retrieve a named field from a JObject of the given (AST) value type */
    def get[A <: JValue](name: String)(implicit m: Manifest[A]): Box[A] =
      for {
        field <- obj.obj.find((field: JField) => field match {
          case JField(n, _) if n == name => true
          case _             => false
        }) ?~ ("No such field " + name)
        value <- Full(field.value).asA[A] ?~ ("Field " + field + " is not a " + m)
      } yield value

    /** Get a field as a JString and project the inner String out of it */
    def getString(name: String): Box[String] = get[JString](name).map(_.s)

    /** Get a field as a JObject and project the inner list of JFields out of it */
    def getObject(name: String): Box[List[JField]] = get[JObject](name).map(_.obj)

    /** Get a field as a JInt and project the inner BigInt out of it */
    def getInt(name: String): Box[BigInt] = get[JInt](name).map(_.num)

    /** Get a field as a JDouble and project the inner String out of it */
    def getDouble(name: String): Box[Double] = get[JDouble](name).map(_.num)

    /** Get a field as a JArray and project the inner list of JValues out of it */
    def getArray(name: String): Box[List[JValue]] = get[JArray](name).map(_.arr)

    /** Construct a version of the input JObject with the given field removed (if present) */
    def remove(field: String): JObject =
      JObject(obj.obj.filter { case JField(key, _) if key != field => true; case _ => false })

    /** Construct a version of the input JObject with all the given fields removed (if present) */
    def remove(fields: String*): JObject =
      JObject(obj.obj.filter { case JField(key, _) if !(fields contains key) => true; case _ => false })
    
    /** Clean out the usual couch fields of "_id", "_rev", and "type", if present. */
    def clean: JObject = remove("_id", "_rev", "type")
  }
}

}
}
