/*
 * Copyright 2010 WorldWide Conferencing, LLC
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
package mongodb {

import java.util.Date

import org.bson.types.ObjectId
import org.specs.Specification
import org.specs.runner.JUnit4

import net.liftweb.common._
import net.liftweb.json.JsonAST._
import net.liftweb.json.ext.JsonBoxSerializer

import com.mongodb._

class MongoDocumentSpecsTest extends JUnit4(MongoDocumentSpecs)

package mongodocumentspecs {
  case class Primitives(
    _id: String,
    s: String,
    i: Int,
    l: Long,
    d: Double,
    f: Float,
    b: Byte,
    bi: BigInt,
    bool: Boolean,
    sh: Short,
    jli: java.lang.Integer,
    jll: java.lang.Long,
    jld: java.lang.Double,
    jlf: java.lang.Float,
    jlb: java.lang.Byte,
    jlbool: java.lang.Boolean,
    jlsh: java.lang.Short
  ) extends MongoDocument[Primitives] {

    def meta = Primitives
  }
  object Primitives extends MongoDocumentMeta[Primitives]

  case class NullTestEmbed(nul: String)
  case class NullTestDoc(_id: String, nul: String, ent: NullTestEmbed)
  extends MongoDocument[NullTestDoc] {
    def meta = NullTestDoc
  }
  object NullTestDoc extends MongoDocumentMeta[NullTestDoc]

  case class OptionTestDoc(_id: String, optNone: Option[String],
    optSome: Option[String])
  extends MongoDocument[OptionTestDoc] {
    def meta = OptionTestDoc
  }
  object OptionTestDoc extends MongoDocumentMeta[OptionTestDoc]

  case class BoxTestDoc(_id: String, boxEmpty: Box[String],
    boxFull: Box[String], boxFail: Box[String])
  extends MongoDocument[BoxTestDoc] {
    def meta = BoxTestDoc
  }
  object BoxTestDoc extends MongoDocumentMeta[BoxTestDoc] {
    override def formats = super.formats + new JsonBoxSerializer
  }
}

object MongoDocumentSpecs extends Specification with MongoTestKit {

  "MongoDocument" should {

    "handle primitives" in {
      checkMongoIsRunning
      import mongodocumentspecs._

      val primitives = Primitives(
        "1",
        "This is a String",
        123,
        124L,
        (125.5).toDouble,
        (126.5).toFloat,
        (127).toByte,
        BigInt(128999),
        true,
        (129).toShort,
        new java.lang.Integer(130),
        new java.lang.Long(131L),
        new java.lang.Double(132.5),
        new java.lang.Float(133.5),
        new java.lang.Byte("12"),
        java.lang.Boolean.TRUE,
        new java.lang.Short("135")
      )

      primitives.save

      val pFromDb = Primitives.find(primitives._id)

      pFromDb.isDefined must_== true

      pFromDb.get must_== primitives
    }

    "handle null" in {
      checkMongoIsRunning
      import mongodocumentspecs._

      val ntd = NullTestDoc("1", null, NullTestEmbed(null))

      ntd.save

      val ntdFromDb = NullTestDoc.find(ntd._id)

      ntdFromDb.isDefined must_== true

      ntdFromDb.get must_== ntd
    }

    "handle Option" in {
      checkMongoIsRunning
      import mongodocumentspecs._

      val otd = OptionTestDoc("1", None, Some("Some String"))

      otd.save

      val otdFromDb = OptionTestDoc.find(otd._id)

      otdFromDb.isDefined must_== true

      otdFromDb.get must_== otd
    }

    "handle Box using JsonBoxSerializer" in {
      checkMongoIsRunning
      import mongodocumentspecs._

      val btd = BoxTestDoc("1", Empty, Full("Full String"), Failure("This is a failure"))

      btd.save

      val btdFromDb = BoxTestDoc.find(btd._id)

      btdFromDb.isDefined must_== true

      btdFromDb.get must_== btd
    }
  }
}

}
}
