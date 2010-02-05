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
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.http.{S, RequestVar}

object posted extends RequestVar[Box[String]](Empty)

class AutoCompleteDemo {

  def display(xhtml: NodeSeq) :NodeSeq = {
    
    object value1 extends RequestVar("")
    object value2 extends RequestVar("")
    
    def autoCompleteData(current: String, limit: Int) :Seq[String] = {
      println("current = " + current)
      (1 to limit).map(v => "Value_" + v)
    }
    
    def autocompleteSubmit(in: String): Unit = {
      value2(in)
    }
    
    val myOptions :List[(String,String)] = ("'width'","300") :: Nil
    bind("autocomplete",xhtml,
         "1" -> AutoComplete("", autoCompleteData(_,_), in => value1(in)), 
         "2" -> AutoComplete("", autoCompleteData(_,_), autocompleteSubmit(_), myOptions),
         "val1" -> Text(value1.is),
         "val2" -> Text(value2.is))
    
    
  }

}

}
}
