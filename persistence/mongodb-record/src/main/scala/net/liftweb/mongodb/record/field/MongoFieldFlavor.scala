/*
 * Copyright 2010-2020 WorldWide Conferencing, LLC
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
package mongodb
package record
package field

import net.liftweb.common.{Box, Empty, Failure, Full}
import net.liftweb.http.js.JE.{JsNull, JsRaw}
import net.liftweb.json._
import com.mongodb.DBObject

/**
* Describes common aspects related to Mongo fields
*/
@deprecated("Please use 'BsonableField' instead.", "3.4.2")
trait MongoFieldFlavor[MyType] {

  /*
  * convert this field's value into a DBObject so it can be stored in Mongo.
  */
  @deprecated("This was replaced with the functions from 'BsonableField'.", "3.4.2")
  def asDBObject: DBObject

  // set this field's value using a DBObject returned from Mongo.
  @deprecated("This was replaced with the functions from 'BsonableField'.", "3.4.2")
  def setFromDBObject(obj: DBObject): Box[MyType]

  /**
  * Returns the field's value as a valid JavaScript expression
  */
  def asJs = asJValue match {
    case JNothing => JsNull
    case jv => JsRaw(compactRender(jv))
  }

  /** Encode the field value into a JValue */
  def asJValue: JValue

}
