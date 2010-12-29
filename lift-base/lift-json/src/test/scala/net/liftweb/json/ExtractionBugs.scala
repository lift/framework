/*
 * Copyright 2009-2010 WorldWide Conferencing, LLC
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
package json {

import org.specs.Specification
import org.specs.runner.{Runner, JUnit}

class ExtractionBugsTest extends Runner(ExtractionBugs) with JUnit
object ExtractionBugs extends Specification {  
  implicit val formats = DefaultFormats
  
  "ClassCastException (BigInt) regression 2 must pass" in {
    val opt = OptionOfInt(Some(39))    
    Extraction.decompose(opt).extract[OptionOfInt].opt.get mustEqual 39
  }

  "Extraction should not fail when Maps values are Lists" in {
    val m = PMap(Map("a" -> List("b"), "c" -> List("d")))
    Extraction.decompose(m).extract[PMap] mustEqual m
  }

  case class OptionOfInt(opt: Option[Int])

  case class PMap(m: Map[String, List[String]])
}

}
}
