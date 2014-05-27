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
  // Finding things

  /**
   * Given a NodeSeq and a function that returns a Box[T],
   * return the first value found in which the function evaluates
   * to Full
   */
  def findBox[T](nodes: Seq[Node])(f: Elem => Box[T]): Box[T] = {
    nodes.view.flatMap {
      case Group(g) => findBox(g)(f)
      case e: Elem => f(e) or findBox(e.child)(f)
      case _ => Empty
    }.headOption
  }

  /**
   * Given a NodeSeq and a function that returns an Option[T],
   * return the first value found in which the function evaluates
   * to Some
   */
  def findOption[T](nodes: Seq[Node])(f: Elem => Option[T]): Option[T] = {
    nodes.view.flatMap {
      case Group(g) => findOption(g)(f)
      case e: Elem => f(e) orElse findOption(e.child)(f)
      case _ => None
    }.headOption
  }

  /**
   * Given an id value, find the Elem with the specified id
   */
  def findId(nodes: Seq[Node], id: String): Option[Elem] = {
    findOption(nodes) {
      e => e.attribute("id").filter(_.text == id).map(i => e)
    }
  }

  /**
   * Finds the first `Elem` in the NodeSeq (or any children)
   * that has an ID attribute and return the value of that attribute.
   */
  def findId(ns: NodeSeq): Box[String] = {
    findBox(ns)(_.attribute("id").map(_.text))
  }

  // Modifying things

  /**
   * Remove all the <head> elements, just leaving their child elements.
   */
  def stripHead(in: NodeSeq): NodeSeq = {
    ("head" #> ((ns: NodeSeq) => ns.asInstanceOf[Elem].child)).apply(in)
  }

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

  // Ensures unique ids, but provides a way to selectively recurse through
  // subelements or not by invoking a `processElement` function when an
  // element is found. `processElement` is given the element that was
  // found as well as the duplicate id stripping function, and the
  // caller can decide whether to ensure uniqueness at only one level or
  // whether to recurse through children.
  private def ensureUniqueIdHelper(in: Seq[NodeSeq], processElement: (Elem, (Node)=>Node)=>Elem): Seq[NodeSeq] = {
    var ids: Set[String] = Set()

    def stripDuplicateId(node: Node): Node = node match {
      case Group(ns) => Group(ns.map(stripDuplicateId))
      case element: Elem =>
        element.attribute("id") match {
          case Some(id) => {
            if (ids.contains(id.text)) {
              processElement(
                element.copy(attributes = removeAttribute("id", element.attributes)),
                stripDuplicateId _
              )
            } else {
              ids += id.text

              processElement(element, stripDuplicateId _)
            }
          }

          case _ => element
        }

      case other => other
    }

    in.map(_.map(stripDuplicateId))
  }

  /**
   * For a list of NodeSeq, ensure that the the id of the root Elems
   * are unique.  If there's a duplicate, that Elem will be returned
   * without an id
   */
  def ensureUniqueId(in: Seq[NodeSeq]): Seq[NodeSeq] = {
    ensureUniqueIdHelper(in, (element, _) => element)
  }

  /**
   * For a list of NodeSeq, ensure that the the id of all Elems are
   * unique, recursively.  If there's a duplicate, that Elem will be
   * returned without an id.
   */
  def deepEnsureUniqueId(in: NodeSeq): NodeSeq = {
    ensureUniqueIdHelper(List(in), { (element, stripUniqueId) =>
      element.copy(child = element.child.map(stripUniqueId))
    }).head
  }

  /**
   * Ensure that the first Element has the specified ID
   */
  def ensureId(ns: NodeSeq, id: String): NodeSeq = {
    var found = false
    
    ns.map {
      case x if found => x
      case element: Elem => {
        val meta = removeAttribute("id", element.attributes)

        found = true

        element.copy(attributes = new UnprefixedAttribute("id", id, meta))
      }
 
      case x => x
    }
  }

  // Misc

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
