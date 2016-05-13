/**
  * Copyright 2014 WorldWide Conferencing, LLC
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

package net.liftweb
package mongodb

import util.{ConnectionIdentifier, SimpleInjector}
import util.Helpers._

object MongoRules extends SimpleInjector {
  private def defaultCollectionNameFunc(conn: ConnectionIdentifier, name: String): String = {
    charSplit(name, '.').last.toLowerCase+"s"
  }

  /**
    * Calculate the name of a collection based on the full
    * class name of the MongoDocument/MongoRecord. Must be
    * set in Boot before any code that touches the
    * MongoDocumentMeta/MongoMetaRecord.
    *
    * To get snake_case, use this
    *
    *  RecordRules.collectionName.default.set((_,name) => StringHelpers.snakify(name))
    */
  val collectionName = new Inject[(ConnectionIdentifier, String) => String](defaultCollectionNameFunc _) {}
}
