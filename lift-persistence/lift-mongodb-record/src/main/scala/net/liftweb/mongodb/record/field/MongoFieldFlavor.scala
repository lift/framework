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
package mongodb {
package record {
package field {

import scala.xml.{Node, NodeSeq, Text}

import net.liftweb.common.{Box, Empty, Failure, Full}
import net.liftweb.http.{S}
import net.liftweb.http.js.JE.Str
import net.liftweb.json.JsonAST.JObject
import net.liftweb.json.JsonParser
import net.liftweb.record.{Field, Record}
import net.liftweb.util.{FieldError, Log}

import com.mongodb._
import com.mongodb.util.{JSON, JSONParseException}

/**
* Describes common aspects related to Mongo fields
*/
trait MongoFieldFlavor[MyType] {

  import com.mongodb.util.JSON

  /*
  * convert this field's value into a DBObject so it can be stored in Mongo.
  */
  def asDBObject: DBObject

  // set this field's value using a DBObject returned from Mongo.
  def setFromDBObject(obj: DBObject): Box[MyType]
  
  // assume string is json
  def setFromString(in: String): Box[MyType] = {
    // use Mongo parser to convert to DBObject
    setFromDBObject(JSON.parse(in).asInstanceOf[DBObject])
  }

}

}
}
}
}
