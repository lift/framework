/*
 * Copyright 2010-2012 WorldWide Conferencing, LLC
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
package http

import org.specs2.matcher.XmlMatchers
import org.specs2.mutable.Specification
import util._
import Helpers._
import net.liftweb.mockweb.MockWeb._

object SHtmlSpec extends Specification with XmlMatchers {
  "NamedCometPerTabSpec Specification".title

  val html1= <span><input id="number"></input></span>

  val inputField1= testS("/")( ("#number" #> SHtml.number(0, println(_), 0, 100)).apply(html1)  )
  val inputField2= testS("/")( ("#number" #> SHtml.number(0, println(_: Double), 0, 100, 0.1)).apply(html1)  )
  val inputField3= testS("/")( ("#number" #> SHtml.number(0, println(_: Double), 0, 100, 1)).apply(html1)  )

  "SHtml" should {
    "create a number input field" in {
      inputField1 must \("input", "type" -> "number")
    }
    "create a number input field with min='0'" in {
      inputField1 must \("input", "min" -> "0")
    }
    "create a number input field with max='100'" in {
      inputField1 must \("input", "max" -> "100")
    }
    "create a number input field with step='0.1'" in {
      inputField2 must \("input", "step" -> "0.1")
    }
    "create a number input field with step='1.0'" in {
      inputField3 must \("input", "step" -> "1.0")
    }

  }
}
