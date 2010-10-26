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

package net.liftweb {
package widgets {
package tablesorter {

import _root_.net.liftweb.http.ResourceServer
import _root_.scala.xml.NodeSeq
import _root_.net.liftweb.http.{LiftRules}
import _root_.net.liftweb.http.js._
import JsCmds._
import JE._
import jquery.JqJE._

// types to ease the creation of a JsObj to configure the TableSorter plugin. 
trait TableSorterOption[T] {
  
  def toJsObj: JsObj
  
}

// Because of the whacky js api we have to deal with sorter options
// being either a string specifying the name of the sorter or the boolean value
// false in the case you want to disable the sorting. 

trait SorterOption[T] extends TableSorterOption[T] {
  val sorter: T    
}

/**
* This is used to create the special case of SorterOption where sorting is 
* disabled
*/
case class DisableSorting() extends SorterOption[Boolean]{
  
  val sorter = false
  
  def toJsObj = JsObj("sorter" -> sorter)
}

// Allows the user to type stuff like Sorting.DSC to specify that a column should be sorted 
// decening instead of just a plain integer that the js api expects. 
object Sorting extends Enumeration {
  type Sorting = Value
  val ASC, DSC = Value
}

/**
* Sorter is used to define how the TableSorter should sort the columns. The possible list 
* of build in sorters are: shortDate, usLongDate, percent, isoDate, url, ipAddress, currency, 
* digit, text, shortDate, time, metadata
* 
*/
case class Sorter(sorter: String) extends SorterOption[String] {
  def toJsObj = JsObj("sorter" -> sorter)
}

object TableSorter {

  private val emptyJsObj = new JsObj {  
    def props = Nil  
  }


  def apply(selector: String): NodeSeq = renderOnLoad(selector, TableSorter.options())
  def apply(selector: String, options: JsObj): NodeSeq = renderOnLoad(selector, options)

  /**
   * Initializes the widget. You have to call this in boot for the widget to work.
   */
  def init() {
    ResourceServer.allow({
      case "tablesorter" :: tail => true
    })
  }
  
  import Sorting._
  
  /**
  * Use this method to create a type-safe configuration JsObj for the 
  * TableSorter. See http://tablesorter.com/docs/ for more information
  * about the possible configurations of the jQuery plugin. 
  * 
  * Example usage: TableSorter.options(
  *   headers = (0, DisableSorting()) :: (3,Sorter("currency")) :: Nil,
  *   sortList = (3,Sorting.DSC) :: Nil)
  * 
  * @param  headers   A list of tuples of int * sorter options. the int is the column number.
  *                   The column number is indexed starting from 0
  * @param  sortList  A list of tuples of int * Sorting. the int is the column number.
  *                   Use DisableSorting to disable sorting
  * @return           A JsObj with valid properties to configure the jQuery TableSorter plugin
  */
  def options(headers: List[(Int, TableSorterOption[_])], 
              widgets: List[String], 
              sortList: List[(Int, Sorting)]): JsObj = {
            
    val jsHeaders = headers
      .map{ case (index, header) => JsObj(index.toString -> header.toJsObj)}
      .foldLeft[JsObj](emptyJsObj)(_ +* _)
    val jSSortList = JsArray( sortList.map{ case (index, sorting) => JsArray(index,sorting.id)}:_*)  
    val widgetsArr = JsArray ( widgets.map( Str(_) ):_* )
      
    JsObj("sortList" -> jSSortList, "headers" -> jsHeaders, "widgets" -> widgetsArr)
  }
  
  //Convenience versions of the options method that provide default values for all the arguments.
  def options(): JsObj = options(Nil, List("zebra"), List((0, ASC)))
  
  def options(headers: List[(Int, TableSorterOption[_])], 
              sortList: List[(Int, Sorting)]): JsObj = options(headers, List("zebra"), sortList)

  /**
  * Transforms a regular table into a tablesorter when page is loaded
  * 
  * @param  selector  A CSS selector for the table you want to transform
  * @param  options   A JsObject configuring the tablesorter. You can use the connivance method
  *                   TableSorter.options to create the JsObj in a type-safe manner. Check out 
                      http://tablesorter.com/docs/ for more info about the possible configurations.
  */
  def renderOnLoad(selector: String, options: JsObj): NodeSeq = {
    val onLoad = """jQuery(document).ready(function(){
        jQuery('"""+selector+"""').tablesorter("""+options.toJsCmd+""");
      });"""

    <head>
      <link rel="stylesheet" href={"/" + LiftRules.resourceServerPath + "/tablesorter/themes/blue/style.css"} type="text/css" id="" media="print, projection, screen" />
      <script type="text/javascript" src={"/" + LiftRules.resourceServerPath + "/tablesorter/jquery.tablesorter.js"}/>
      {Script(JsRaw(onLoad))}
    </head>
  }

  /**
   * Transforms a regular table into a tablesorter
   */
  def jsRender(selector: String, options: JsObj): JsExp = 
    JqId(selector) ~> new JsRaw("tablesorter("+options.toJsCmd+");") with JsMember
  
  def jsRender(selector: String): JsExp = jsRender(selector,TableSorter.options())

}

}
}
}
