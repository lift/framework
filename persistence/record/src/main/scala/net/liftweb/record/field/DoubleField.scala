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

import scala.xml._
import net.liftweb.common._
import net.liftweb.http.{S}
import net.liftweb.json.JsonAST.{JDouble, JNothing, JNull, JValue}
import net.liftweb.util._
import Helpers._
import S._

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

  def asJValue = valueBox.map(JDouble) openOr (JNothing: JValue)
  
  def setFromJValue(jvalue: JValue) = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case JDouble(d)                   => setBox(Full(d))
    case other                        => setBox(FieldHelpers.expectedA("JDouble", other))
  }
}

class DoubleField[OwnerType <: Record[OwnerType]](rec: OwnerType)
  extends Field[Double, OwnerType] with MandatoryTypedField[Double] with DoubleTypedField {

  def this(rec: OwnerType, value: Double) = {
    this(rec)
    set(value)
  }

  def owner = rec
}

class OptionalDoubleField[OwnerType <: Record[OwnerType]](rec: OwnerType)
  extends Field[Double, OwnerType] with OptionalTypedField[Double] with DoubleTypedField {

  def this(rec: OwnerType, value: Box[Double]) = {
    this(rec)
    setBox(value)
  }

  def owner = rec
}

