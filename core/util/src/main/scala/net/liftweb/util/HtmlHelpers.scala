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

import scala.language.higherKinds

import scala.xml._

import common._

/**
 * This trait is used to identify an object that is representable as a
 * {@link NodeSeq}.
 */
trait Bindable {
  def asHtml: NodeSeq
}

/**
 * A common function-like interface for accessing information about
 * attributes, based on the two core <code>findAttr</code> and
 * <code>convert</code> methods.
 *
 * Extenders can be fairly flexible. The value of an attribute is
 * specified by the extender as type <code>Info</code>. Possibly-missing
 * attributes are returned in type Holder, which should be
 * parametrizable.  For example, you could create an AttrHelper that
 * deals in String attribute values (<code>type Info = String</code>)
 * and returns <code>Option</code>s in cases where the attribute may not
 * be found.
 *
 * Note that you can invoke an <code>AttrHelper</code> with conversion
 * functions to turn an <code>Info</code> into an arbitrary type.
 *
 * A sample implementation:
 * {{{
 * case class HtmlAttributes(in: Elem) extends AttrHelper[Box] {
 *   type Info = String
 *
 *   def findAttr(key: String): Option[Info] = {
 *     in.attribute(key).map(_.text)
 *   }
 *
 *   def findAttr(prefix: String, key: String): Option[Info] = {
 *     in.attribute(prefix, key).map(_.text)
 *   }
 *
 *   def convert[T](in: Option[T]): Box[T] = Box(in)
 * }
 * }}}
 *
 * The helper above takes a scala <code>Elem</code> and provides a
 * series of ways to access the values of its elements. For example:
 *
 * {{{
 * val attributes = HtmlAttributes(elem)
 *
 * attributes("class") // class attribute as String, Empty if absent
 * attributes("lift", "bind") // as above with lift:bind attribute
 * attributes("class", "nothing") // class attribute, "nothing" if absent
 * attributes("lift", "bind", "nothing") // as above with lift:bind attribute
 * attributes("class", _.split(" ")) // class attribute as array, Empty if absent
 * attributes("lift", "bind", _.split(" ")) // as above with lift:bind attribute
 * attributes("class", _.split(" "), Array()) // class attribute as array, Array() if absent
 * attributes("lift", "bind", _.split(" "), Array()) // as above with lift:bind attribute
 * }}}
 */
trait AttrHelper[+Holder[X]] {
  // FIXME do we really need this Holder stuff?

  type Info

  def apply(key: String): Holder[Info] = convert(findAttr(key))
  def apply(prefix: String, key: String): Holder[Info] =
    convert(findAttr(prefix, key))

  def apply(key: String, default: => Info): Info =
    findAttr(key) getOrElse default

  def apply(prefix: String, key: String, default: => Info): Info =
    findAttr(prefix, key) getOrElse default

  def apply[T](key: String, f: Info => T): Holder[T] =
    convert(findAttr(key).map(f))

  def apply[T](prefix: String, key: String, f: Info => T): Holder[T] =
    convert(findAttr(prefix, key).map(f))

  def apply[T](key: String, f: Info => T, default: => T): T =
    findAttr(key).map(f) getOrElse default

  def apply[T](prefix: String, key: String, f: Info => T, default: => T): T =
    findAttr(prefix, key).map(f) getOrElse default

  protected def findAttr(key: String): Option[Info]
  protected def findAttr(prefix: String, key: String): Option[Info]
  protected def convert[T](in: Option[T]): Holder[T]
}

trait HtmlHelpers extends CssBindImplicits {
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
