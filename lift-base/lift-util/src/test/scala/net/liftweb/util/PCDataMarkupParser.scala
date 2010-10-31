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
package util {

import _root_.org.specs._
import _root_.org.specs.runner._
import _root_.scala.xml._
import common._

class PCDataMarkupTest extends JUnit4(PCDataMarkupSpecs)
object PCDataMarkupSpecs extends Specification {
val data1 = """


<html>dude</html>


"""

val data2 = """

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html>dude</html>


"""


   "PCDataMarkupParser" should {
     "Parse a document with whitespace" in {
       PCDataXmlParser(data1).open_! must ==/ (<html>dude</html>)
     }

     "Parse a document with whitespace" in {
       PCDataXmlParser(data2).open_! must ==/ (<html>dude</html>)
     }
   }

}

}
}
