/*
 * Copyright 2006-2010 WorldWide Conferencing, LLC
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
package mapper {

import _root_.scala.xml.{NodeSeq, Elem}
import _root_.net.liftweb.http.S
import _root_.net.liftweb.http.S._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._

abstract class MappedTextarea[T<:Mapper[T]](owner : T, maxLen: Int) extends MappedString[T](owner, maxLen) {
  /**
   * Create an input field for the item
   */
  override def _toForm: Box[Elem] = {
    S.fmapFunc({s: List[String] => this.setFromAny(s)}){funcName =>
    Full(<textarea name={funcName}
	 rows={textareaRows.toString}
	 cols={textareaCols.toString} id={fieldId}>{
	   is match {
	     case null => ""
	     case s => s}}</textarea>)}
  }

  override def toString = {
    val v = is
    if (v == null || v.length < 100) super.toString
    else v.substring(0,40)+" ... "+v.substring(v.length - 40)
  }

  def textareaRows  = 8

  def textareaCols = 20

}

}
}
