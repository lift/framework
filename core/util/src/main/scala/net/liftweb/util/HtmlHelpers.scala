/*
 * Copyright 2007-2013 WorldWide Conferencing, LLC
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
package util

import scala.xml._

import common._

/**
 * This trait is used to identify an object that is representable as a
 * {@link NodeSeq}.
 */
trait Bindable {
  def asHtml: NodeSeq
}

trait HtmlHelpers {
  /**
   * Remove an attribute from the specified element.
   *
   * @param name the name of the attribute to remove
   * @param elem the element
   * @return the element sans the named attribute
   */
  def removeAttribute(name: String, element: Elem): Elem = {
    element.copy(attributes = removeAttribute(name, element.attributes))
  }

  /**
   * Remove an attribute from the specified list of existing attributes.
   *
   * @param name the name of the attribute to remove
   * @param existingAttributes a list of existing attributes
   * @return the attributes list sans the named attribute
   */
  def removeAttribute(name: String, existingAttributes: MetaData): MetaData = {
    existingAttributes.filter {
      case up: UnprefixedAttribute => up.key != name
      case _ => true
    }   
  }

  /**
   * Adds the specified <code>cssClass</code> to the existing class
   * attribute of an Elem or create the class attribute with that
   * class if it does not exist.
   *
   * If <code>cssClass</code> is not <code>Full</code>, returns the
   * passed Elem unchanged.
   */
  def addCssClass(cssClass: Box[String], elem: Elem): Elem = {
    cssClass match {
      case Full(css) => addCssClass(css, elem)
      case _ => elem
    }
  }

  /**
   * Adds the specified <code>cssClass</code> to the existing class
   * attribute of an Elem or creates the class attribute with that class
   * if it does not exist.
   */
  def addCssClass(cssClass: String, elem: Elem): Elem = {
    elem.attribute("class") match {
      case Some(existingClasses) =>
        def attributesWithUpdatedClass(existingAttributes: MetaData) = {
          new UnprefixedAttribute(
            "class",
            existingClasses.text.trim + " " + cssClass.trim, 
            removeAttribute("class", existingAttributes)
          )
        }

        elem.copy(attributes = attributesWithUpdatedClass(elem.attributes))

      case _ => elem % new UnprefixedAttribute("class", cssClass, Null)
    }
  }

  /**
   * For a list of NodeSeq, ensure that the the id of the root Elems
   * are unique.  If there's a duplicate, that Elem will be returned
   * without an id
   */
  def ensureUniqueId(in: Seq[NodeSeq]): Seq[NodeSeq] = {
    var ids: Set[String] = Set()

    def stripDuplicateId(element: Elem): Elem = {
      element.attribute("id") match {
        case Some(id) => {
          if (ids.contains(id.text)) {
            element.copy(attributes =
              element.attributes.filter {
                case attribute: UnprefixedAttribute => attribute.key != "id"
                case _ => true
              }
            )
          } else {
            ids += id.text
            element
          }
        }

        case _ => element
      }
    }

    in.map {
      case e: Elem => stripDuplicateId(e)
      case x => x.map {
        case e: Elem => stripDuplicateId(e)
        case x => x
      }
    }
  }

  def errorDiv(body: NodeSeq): Box[NodeSeq] = {
    Props.mode match {
      case Props.RunModes.Development | Props.RunModes.Test =>
        Full(<div class="snippeterror" style="display: block; padding: 4px; margin: 8px; border: 2px solid red">
             {body}
          <i>note: this error is displayed in the browser because
          your application is running in "development" or "test" mode.If you
          set the system property run.mode=production, this error will not
          be displayed, but there will be errors in the output logs.
          </i>
          </div>)

        case _ => Empty
    }
  }
}
