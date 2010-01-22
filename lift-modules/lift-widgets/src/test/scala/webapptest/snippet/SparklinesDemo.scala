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

import _root_.scala.xml._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.S._
import _root_.net.liftweb.http.SHtml._
import _root_.net.liftweb.http.{RequestVar, LiftResponse, BasicResponse, JsonResponse}
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.common.Box._
import _root_.net.liftweb.http.js._
import JE._
import JsCmds._

import _root_.net.liftweb.widgets.sparklines.{Sparklines, SparklineStyle}

class SparklinesDemo {

  def renderOnLoad(html: NodeSeq): NodeSeq = {
    val data = JsArray(100,500,300,200,400,500,400,400,100,200, 345, 412, 111, 234, 490);

    val opts = JsObj(("percentage_lines" -> JsArray(0.5, 0.75)),
                     ("fill_between_percentage_lines" -> true),
                     ("extend_markings" -> false));

    Sparklines.onLoad("bar", SparklineStyle.BAR, data, opts);
  }
}

}
}
