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

import json.Formats
import json.JsonAST.JObject

import scala.reflect.Manifest

import org.bson.types.ObjectId

/*
* These traits provide lift-json related conveniece methods for case classes
* and their companion objects. Used by MongoDocument, JsonObjectField, and
* MongoJsonObjectListField
*/
trait JsonObject[BaseDocument] {
  self: BaseDocument =>

  def meta: JsonObjectMeta[BaseDocument]

  // convert class to a json value
  def asJObject()(implicit formats: Formats): JObject = meta.toJObject(this)

}

class JsonObjectMeta[BaseDocument](implicit mf: Manifest[BaseDocument]) {

  import net.liftweb.json.Extraction._

  // create an instance of BaseDocument from a JObject
  def create(in: JObject)(implicit formats: Formats): BaseDocument =
    extract(in)(formats, mf)

  // convert class to a JObject
  def toJObject(in: BaseDocument)(implicit formats: Formats): JObject =
    decompose(in)(formats).asInstanceOf[JObject]
}

/*
* Case class for a db reference (foreign key). To be used in a JsonObject
* ref = collection name, id is the value of the reference
* Only works with ObjectIds.
*/
case class MongoRef(ref: String, id: ObjectId)


}
}