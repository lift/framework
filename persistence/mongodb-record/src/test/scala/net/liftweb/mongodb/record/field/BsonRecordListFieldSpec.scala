/*
 * Copyright 2010-2014 WorldWide Conferencing, LLC
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

import org.specs2.mutable.Specification
import net.liftweb.common._
import net.liftweb.record.field.StringField

package bsonlistfieldspecs {
  class BookShelf extends MongoRecord[BookShelf] with ObjectIdPk[BookShelf] {
    def meta = BookShelf

    object books extends BsonRecordListField(this, Book)
  }
  object BookShelf extends BookShelf with MongoMetaRecord[BookShelf] {
    override def collectionName = "bookshelf"
  }

  class Book extends BsonRecord[Book] {
    override def meta = Book

    object title extends StringField(this, 512)
  }
  object Book extends Book with BsonMetaRecord[Book]
}

class BsonRecordListFieldSpec extends Specification {
  "BsonRecordListField Specification".title

  import bsonlistfieldspecs._

  "BsonRecordListFieldSpec" should {

    "fail validation if at least one of its elements fails validation" in {
      val scalaBook = Book.createRecord.title("Programming in Scala")
      val liftBook = Book.createRecord
      liftBook.title.setBox(Failure("Bad format"))
      val shelf = BookShelf.createRecord.books(scalaBook :: liftBook :: Nil)

      shelf.validate must have size(1)
    }

    "pass validation if all of its elements pass validation" in {
      val scalaBook = Book.createRecord.title("Programming in Scala")
      val liftBook = Book.createRecord.title("Simply Lift")
      val shelf = BookShelf.createRecord.books(scalaBook :: liftBook :: Nil)

      shelf.validate must be empty
    }

  }
}
