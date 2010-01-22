/*
 * Copyright 2007-2010 WorldWide Conferencing, LLC
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

package webapptest {
package snippet {

import _root_.scala.xml.{NodeSeq, Text}
import _root_.net.liftweb.http._
import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import _root_.net.liftweb.widgets.autocomplete._

object posted extends RequestVar[Box[String]](Empty)

class AutoCompleteDemo {

  def render(xhtml: NodeSeq) :NodeSeq = {
    AutoComplete("", (current, limit) => {
      println("current = " + current)
      (1 to limit).map(v => "Value_" + v)
    }, s =>  posted(Full(s))) ++ (posted.map(t => <p>{"Submitted " + t}</p>) openOr Text(""))
  }

}

}
}
