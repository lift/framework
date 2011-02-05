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
package mapper {
package view {

import scala.xml.{Group, Elem, NodeSeq}
import scala.collection.mutable.{Map => mMap}

import net.liftweb.http.SHtml
import net.liftweb.util.{Helpers}
import net.liftweb.common.{Full, Empty}


/**
 * This class can be used to act on all submitted values in one block of code.
 * This enables one to use try/catch when setting values etc.
 * @author nafg
 */
abstract class FormProcessor(prefix: String) {

  def text(attrs: SHtml.ElemAttr*)(init: String, action: String=>Unit) = SHtml.text(init, action, attrs:_*)
  def text: (String,String=>Unit)=>NodeSeq  = text()_
  def checkbox(attrs: SHtml.ElemAttr*)(init: Boolean, action: Boolean=>Unit) = SHtml.checkbox(init, action, (attrs: Seq[SHtml.ElemAttr]):_*)
  def checkbox: (Boolean, Boolean=>Unit)=>NodeSeq = checkbox()_

  val stringValues: mMap[String, String] = mMap.empty[String, String]
  val strings = mMap.empty[String, (String,String=>Unit)=>NodeSeq]

  val booleanValues = mMap.empty[String, Boolean]
  val booleans = mMap.empty[String, (Boolean,Boolean=>Unit)=>NodeSeq]




  def bind(xhtml: NodeSeq) = {
    def transform(node: NodeSeq): NodeSeq = {
      put
      node match {
        case Elem(`prefix`, label, _, _, _*) if strings.keys.toIterator contains label =>
          strings(label)(stringValues(label), stringValues(label) = _)
        case Elem(`prefix`, label, _, _, _*) if booleans.keys.toIterator contains label =>
          booleans(label)(booleanValues(label), booleanValues(label) = _)
       case other => other
      }
    }
    Helpers.bind(prefix, Full(transform _), Empty, xhtml) ++
      Seq(SHtml.hidden(()=>get))

  }


  def put: Unit
  def get: Unit
}

}
}
}
