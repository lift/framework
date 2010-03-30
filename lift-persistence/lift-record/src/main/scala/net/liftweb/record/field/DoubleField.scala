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
import _root_.net.liftweb.common._
import _root_.net.liftweb.http.{S}
import _root_.net.liftweb.json.JsonAST.{JDouble, JNothing, JNull, JValue}
import _root_.net.liftweb.util._
import Helpers._
import S._

class DoubleField[OwnerType <: Record[OwnerType]](rec: OwnerType) extends NumericField[Double, OwnerType] {

  def this(rec: OwnerType, value: Double) = {
    this(rec)
    set(value)
  }

  def this(rec: OwnerType, value: Box[Double]) = {
    this(rec)
    setBox(value)
  }

  def owner = rec

  def setFromAny(in: Any): Box[Double] = setNumericFromAny(in, _.doubleValue)

  def setFromString(s: String): Box[Double] = setBox(tryo(java.lang.Double.parseDouble(s)))

  def defaultValue = 0.0

  def asJValue = valueBox.map(JDouble) openOr (JNothing: JValue)
  def setFromJValue(jvalue: JValue) = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case JDouble(d)                   => setBox(Full(d))
    case other                        => setBox(FieldHelpers.expectedA("JDouble", other))
  }
}

import _root_.java.sql.{ResultSet, Types}
import _root_.net.liftweb.mapper.{DriverType}

/**
 * An int field holding DB related logic
 */
abstract class DBDoubleField[OwnerType <: DBRecord[OwnerType]](rec: OwnerType) extends DoubleField[OwnerType](rec)
  with JDBCFieldFlavor[Double]{

  def targetSQLType = Types.DOUBLE

  /**
   * Given the driver type, return the string required to create the column in the database
   */
  def fieldCreatorString(dbType: DriverType, colName: String): String = colName + " " + dbType.enumColumnType

  def jdbcFriendly(field : String) : Double = value

}

}
}
}
