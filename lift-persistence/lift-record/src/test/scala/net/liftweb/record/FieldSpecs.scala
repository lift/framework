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
package record {

import _root_.net.liftweb.common.{Box, Empty, Full}
import _root_.net.liftweb.http.S
import _root_.net.liftweb.record.field.PasswordField
import _root_.net.liftweb.util.FieldError
import _root_.org.specs._
import _root_.org.specs.runner.{ConsoleRunner, JUnit3}
import _root_.scala.xml.{Node, Text}


class FieldSpecsAsTest extends JUnit3(FieldSpecs)
object FieldSpecsRunner extends ConsoleRunner(FieldSpecs)

package fieldspecs {
  class PasswordTestRecord extends Record[PasswordTestRecord] {
    def meta = PasswordTestRecord

    object password extends PasswordField(this) {
      override def validators = PasswordTestRecord.validateNonEmptyPassword _ :: super.validators
    }
  }
  
  object PasswordTestRecord extends PasswordTestRecord with MetaRecord[PasswordTestRecord] {
    def createRecord = new PasswordTestRecord

    private[fieldspecs] def validateNonEmptyPassword(b: Box[String]): Box[Node] = 
      b flatMap {
        case s if s == "testvalue" => Full(Text("no way!"))
        case _ => Empty
      }
  }
}

object FieldSpecs extends Specification {
  "PasswordField" should {
    "require a nonempty password" in {
      import fieldspecs.PasswordTestRecord
      val rec = new PasswordTestRecord().password("")

      rec.validate must_== (
        FieldError(rec.password, Text(S.??("password.must.be.set"))) ::
        Nil
      )
    }

    "validate the unencrypted value" in {
      import fieldspecs.PasswordTestRecord
      val rec = new PasswordTestRecord().password("testvalue")

      rec.validate must_== (
        FieldError(rec.password, Text("no way!")) ::
        Nil
      )
    }
  }
}

}
}
