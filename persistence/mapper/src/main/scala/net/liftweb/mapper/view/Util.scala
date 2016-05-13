/*
 * Copyright 2009-2011 WorldWide Conferencing, LLC
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
package mapper 
package view 

import net.liftweb.mapper.{Mapper,
                           MappedField
}

import net.liftweb.common.{Full, Box}
import net.liftweb.util._
  import Helpers._


import scala.xml.{NodeSeq, Elem}


/**
 * Provides a number of methods that make complex Mapper-based view snippets
 * easier to build.
 * @author nafg
 */
object Util {
  /**
   * Binds all nodes whose names are names of fields on the specified mapper.
   * This makes it unnecessary to write repetitious bindings like
   *    "field1" -> field1.toForm,
   *    "field2" -> field2.toform
   * Instead it automates such bindings but you have to pass it a function
   * that will generate a NodeSeq from the field, e.g.,
   *    (f: MappedField[_,_]) => f.toForm
   * Usage: Pass as a Full Box to the bind overload that takes a nodeFailureXform
   * argument.
   */
  def bindFields[T <: Mapper[T]](mapper: T, nsfn: MappedField[_,T]=>NodeSeq): NodeSeq=>NodeSeq = {
    case xml.Elem(_, name, _, _, _*) => 
      mapper.fieldByName(name) match {
        case Full(field) => nsfn(field)
        case _ => NodeSeq.Empty
      }
    case ns => ns
  }
  
  /**
   * Iterates over the fields of the specified mapper. If the node currently being processed by bind
   * has an attribute "fields" then it is taken as a whitespace-delimited list of fields to iterate
   * over; otherwise all form fields are used. The specified function returns a BindParam for doing
   * processing specific to that field.
   * Returns a bind function (NodeSeq=>NodeSeq) that can be used to bind an xml node that should be
   * repeated for each field.
   * Usage: if you want to repeat xml markup for each field, the view should use the "field:" prefix
   * for field-specific nodes. The snippet should bind the containing (repeating) node to the function
   * returned by this method, passing this method the mapper instance whose fields should be used and
   * a function that returns BindParams to process the "field:" prefixed nodes.
   * This method takes an additional filter function to restrict certain fields from being
   * displayed. There is an overload without it too. 
   */
  def eachField[T<:net.liftweb.mapper.Mapper[T]](
    mapper: T,
    fn:MappedField[_,T]=>CssSel,
    filter: MappedField[_,T]=>Boolean
  ): NodeSeq=>NodeSeq = {
    def fieldBindIfWanted(fieldName: String) = {
      mapper.fieldByName(fieldName).filter(filter) match {
        case Full(field) =>
          Some(fn(field))
        case _ =>
          None
      }
    }

    "^" #> { ns: NodeSeq =>
      val fieldsAttribute = (ns \ "@fields")

      val bind: Seq[CssSel] =
        if (fieldsAttribute.nonEmpty) {
          for {
            fieldName <- fieldsAttribute.text.split("\\s+")
            // the following hackery is brought to you by the Scala compiler not
            // properly typing MapperField[_, T] in the context of the for
            // comprehension
            fieldBind <- fieldBindIfWanted(fieldName)
          } yield {
            ".field" #> fieldBind
          }
        } else {
          mapper.formFields.filter(filter).map {
            case field: MappedField[_, T] =>
              ".field" #> fn(field)
          }
        }

      bind.map(_(ns))
    }
  }
  def eachField[T<:net.liftweb.mapper.Mapper[T]](
    mapper: T,
    fn:MappedField[_,T]=>CssSel
  ): NodeSeq=>NodeSeq = eachField(mapper, fn, (f:MappedField[_,T])=>true)

  
}

