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
package record
package field

import net.liftweb.common._
import net.liftweb.http.{S}
import json._
import net.liftweb.util._
import Helpers._

trait DoubleTypedField extends NumericTypedField[Double] {
  
  def setFromAny(in: Any): Box[Double] = setNumericFromAny(in, _.doubleValue)

  def setFromString(s: String): Box[Double] = 
    if(s == null || s.isEmpty) {
      if(optional_?)
    	  setBox(Empty)
       else
          setBox(Failure(notOptionalErrorMessage))
    } else {
      setBox(tryo(java.lang.Double.parseDouble(s)))
    }

  def defaultValue = 0.0

  def asJValue: JValue = valueBox.map(JDouble) openOr (JNothing: JValue)
  
  def setFromJValue(jvalue: JValue) = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case JDouble(d)                   => setBox(Full(d))
    case JInt(i)                      => setBox(Full(i.toDouble))
    case other                        => setBox(FieldHelpers.expectedA("JDouble", other))
  }
}

@scala.annotation.nowarn("msg=The parameter name should be a String, not a symbol.")
class DoubleField[OwnerType <: Record[OwnerType]](@deprecatedName(Symbol("rec")) val owner: OwnerType)
  extends Field[Double, OwnerType] with MandatoryTypedField[Double] with DoubleTypedField {

  def this(@deprecatedName(Symbol("rec")) owner: OwnerType, value: Double) = {
    this(owner)
    set(value)
  }
}

@scala.annotation.nowarn("msg=The parameter name should be a String, not a symbol.")
class OptionalDoubleField[OwnerType <: Record[OwnerType]](@deprecatedName(Symbol("rec")) val owner: OwnerType)
  extends Field[Double, OwnerType] with OptionalTypedField[Double] with DoubleTypedField {

  def this(@deprecatedName(Symbol("rec")) owner: OwnerType, value: Box[Double]) = {
    this(owner)
    setBox(value)
  }
}

