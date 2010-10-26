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
import _root_.net.liftweb.http.SHtml._
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.widgets.tablesorter.{TableSorter, DisableSorting, Sorting, Sorter}

class TableSorterDemo {
  
  val headers = (0, DisableSorting()) :: (3,Sorter("currency")) :: Nil
  val sortList = (3,Sorting.DSC) :: Nil
  
  val options = TableSorter.options(headers,sortList)
  
  def demo1(xhtml: NodeSeq) :NodeSeq = {
    TableSorter("#myTable")
  }
  
  def demo2(xhtml: NodeSeq) :NodeSeq = {
    TableSorter("#myTable2", options)
  }
  
  def demo3(xhtml: NodeSeq) :NodeSeq = {
    bind("demo3", xhtml, 
      "btn" -> a( () => TableSorter.jsRender("#myTable3", options).cmd, Text("TableSorter that table!") ))
  }
  
}

}
}
