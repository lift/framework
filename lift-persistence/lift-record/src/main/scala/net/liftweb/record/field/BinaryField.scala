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
package record {
package field {

import _root_.scala.xml._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http.{S}
import _root_.net.liftweb.http.js._
import Helpers._
import S._
import JE._


class BinaryField[OwnerType <: Record[OwnerType]](rec: OwnerType) extends Field[Array[Byte], OwnerType] {
  def owner = rec

  def this(rec: OwnerType, value: Array[Byte]) = {
    this(rec)
    set(value)
  }

  def this(rec: OwnerType, value: Box[Array[Byte]]) = {
    this(rec)
    setBox(value)
  }

  def setFromAny(in: Any): Box[Array[Byte]] = genericSetFromAny(in)

  def setFromString(s: String): Box[Array[Byte]] = s match {
    case "" if optional_? => setBox(Empty)
    case _                => setBox(tryo(s.getBytes("UTF-8")))
  }

  def toForm = NodeSeq.Empty

  def asXHtml: NodeSeq = NodeSeq.Empty

  def defaultValue = Array(0)

  def asJs = valueBox.map(v => Str(hexEncode(v))) openOr JsNull

}

import _root_.java.sql.{ResultSet, Types}
import _root_.net.liftweb.mapper.{DriverType}

/**
 * An int field holding DB related logic
 */
abstract class DBBinaryField[OwnerType <: DBRecord[OwnerType]](rec: OwnerType) extends BinaryField[OwnerType](rec)
  with JDBCFieldFlavor[Array[Byte]] {

  def targetSQLType = Types.BINARY

  /**
   * Given the driver type, return the string required to create the column in the database
   */
  def fieldCreatorString(dbType: DriverType, colName: String): String = colName + " " + dbType.enumColumnType

  def jdbcFriendly(field : String) : Array[Byte] = value

}

}
}
}
