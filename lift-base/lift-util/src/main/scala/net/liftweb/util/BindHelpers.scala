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
package util {

import _root_.scala.xml._
import common._



/**
 * This trait is used to identify an object that is representable as a {@link NodeSeq}.
 */
trait Bindable {
  def asHtml: NodeSeq
}

trait AttrHelper[+Holder[X]] {
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

/**
 * BindHelpers can be used to obtain additional information while a {@link bind} call is executing.
 * This informaiton includes node attributes of the current bound node or the entire NodeSeq that is
 * to be bound. Since the context is created during bind execution and destroyed when bind terminates,
 * you can benefit of these helpers in the context of FuncBindParam or FuncAttrBindParam. You can
 * also provide your own implementation of BindParam and your BindParam#calcValue function will be called
 * in the appropriate context.
 *
 * Example:
 * <pre name="code" class="scala">
 * bind("hello", xml,
 *      "someNode" -> {node: NodeSeq => <function-body>})
 * </pre>
 *
 * In <code>function-body</code> you can safely use BindHelpers methods to obtain correctly-scoped information.
 */
object BindHelpers extends BindHelpers {

  private val _bindNodes = new ThreadGlobal[List[NodeSeq]]
  private val _currentNode = new ThreadGlobal[Elem]

  /**
   * A list of NodeSeq that preceeds the NodeSeq passed to bind. The head of the list
   * is the most recent NodeSeq. This returns Empty if it is called outside its context,
   * or Full(Nil) if there are no child nodes but the function is called within the
   * appropriate context.
   */
  def bindNodes: Box[List[NodeSeq]] = _bindNodes.box

  /**
   * A Box containing the current Elem, the children of which are passed to the bindParam
   */
  def currentNode: Box[Elem] = _currentNode.box

  /**
   * Helpers for obtaining attributes of the current Elem
   */
  object attr extends AttrHelper[Option] {
    type Info = NodeSeq

    protected def findAttr(key: String): Option[Info] =
      for { n <- _currentNode.box.toOption
            at <- n.attributes.find(at => at.key == key && !at.isPrefixed) }
      yield at.value

    protected def findAttr(prefix: String, key: String): Option[Info] =
      for { n <- _currentNode.box.toOption
            at <- n.attributes.find {
              case at: PrefixedAttribute => at.key == key && at.pre == prefix
              case _ => false
            }}
      yield at.value

    protected def convert[T](in: Option[T]): Option[T] = in

  }
}

/**
 * Helpers assocated with bindings
 */
trait BindHelpers {
  private lazy val logger = Logger(classOf[BindHelpers])

  def errorDiv(body: NodeSeq): Box[NodeSeq] = 
    Props.mode match {
      case Props.RunModes.Development | Props.RunModes.Test =>
        Full(<div class="snippeterror" style="display: block; margin: 8px; border: 2px solid red">
             {body}
             <br/>
             <br/>
          <i>note: this error is displayed in the browser because
          your application is running in "development" or "test" mode.If you
          set the system property run.mode=production, this error will not
          be displayed, but there will be errors in the output logs.
          </i>
          </div>)

        case _ => Empty
    }

  /**
   * Adds a css class to the existing class tag of an Elem or create
   * the class attribute
   */
  def addCssClass(cssClass: Box[String], elem: Elem): Elem = 
    cssClass match {
      case Full(css) => addCssClass(css, elem)
      case _ => elem
    }

  /**
   * Adds a css class to the existing class tag of an Elem or create
   * the class attribute
   */
  def addCssClass(cssClass: String, elem: Elem): Elem = {
    elem.attribute("class") match {
      case Some(clz) => {
        def fix(in: MetaData) =
          new UnprefixedAttribute("class", clz.text.trim + " " + cssClass.trim, 
                                  in.filter{
                                    case p: UnprefixedAttribute =>
                                      p.key != "class"
                                    case _ => true
                                  })

        new Elem(elem.prefix,
                 elem.label,
                 fix(elem.attributes),
                 elem.scope,
                 elem.child :_*)
      }
      case _ => elem % new UnprefixedAttribute("class", cssClass, Null)
    }
  }

  /**
   * Takes attributes from the first node of 'in' (if any) and mixes
   * them into 'out'. Curried form can be used to produce a
   * NodeSeq => NodeSeq for bind.
   *
   * @param in where to take the attributes from
   * @param out where to put the attributes
   *
   * @return 'out' element with attributes from 'in'
   */
  def mixinAttributes(out: Elem)(in: NodeSeq): NodeSeq = {
    val attributes = in.headOption.map(_.attributes).getOrElse(Null)
    out % attributes
  }

  /**
   * Finds and returns one of many templates from the children based
   * upon the namespace and tag name: for example, for prefix "choose"
   * and tag name "stuff" this would return the contents of the
   * first tag <code>&lt;choose:stuff&gt; ... &lt;/choose:stuff&gt;</code>
   * in the specified NodeSeq.
   *
   * @param prefix the prefix (e.g., "choose")
   * @param tag the tag to choose (e.g., "stuff")
   * @param xhtml the node sequence to search for the specified element
   *
   * @return the first matching node sequence
   */
  def chooseTemplate(prefix: String, tag: String, xhtml: NodeSeq): NodeSeq =
    Helpers.findElems(xhtml)(e => e.label == tag && e.prefix == prefix).toList match {
      case Nil => NodeSeq.Empty
      case x :: xs => x.child
    }

  /**
   * Similar to chooseTemplate, this returns the contents of the element in a Full Box if
   * found or an Empty Box otherwise.
   */
  def template(xhtml: NodeSeq, prefix: String, tag: String): Box[NodeSeq] =
    Helpers.findElems(xhtml)(e => e.label == tag && e.prefix == prefix).toList match {
      case Nil => Empty
      case x :: xs => Full(x.child)
    }

  /**
   * Find two of many templates from the children
   */
  def template(xhtml: NodeSeq, prefix: String, tag1: String,
               tag2: String): Box[(NodeSeq, NodeSeq)] =
    for (x1 <- template(xhtml, prefix, tag1);
         x2 <- template(xhtml, prefix, tag2)) yield (x1, x2)

  /**
   * Find three of many templates from the children
   */
  def template(xhtml: NodeSeq, prefix: String, tag1: String,
               tag2: String, tag3: String): Box[(NodeSeq, NodeSeq, NodeSeq)] =
    for (x1 <- template(xhtml, prefix, tag1);
         x2 <- template(xhtml, prefix, tag2);
         x3 <- template(xhtml, prefix, tag3)) yield (x1, x2, x3)

  /**
   * Base class for Bind parameters. A bind parameter has a name and is able to extract its value from a NodeSeq.
   */
  sealed trait BindParam {
    def name: String
    def calcValue(in: NodeSeq): Option[NodeSeq]
  }

  /**
   * A trait that indicates what the newly bound attribute name should be.
   */
  trait BindWithAttr {
    def newAttr: String
  }

  /**
   * A case class that wraps attribute-oriented BindParams to allow prefixing the resulting attribute
   */
  sealed case class PrefixedBindWithAttr(prefix : String, binding: BindParam with BindWithAttr) extends BindParam with BindWithAttr {
    val name = binding.name
    def calcValue(in : NodeSeq) = binding.calcValue(in)
    val newAttr = binding.newAttr
  }

  /**
   * Constant BindParam always returning the same value
   */
  final class TheBindParam(val name: String, value: NodeSeq)
          extends Tuple2(name, value) with BindParam {
    def calcValue(in: NodeSeq): Option[NodeSeq] = Some(value)
  }

  object TheBindParam {
    def apply(name: String, value: NodeSeq) = new TheBindParam(name, value)
  }

  /**
   * Constant BindParam always returning the same value
   */
  final class TheStrBindParam(val name: String, value: String)
          extends Tuple2(name, value) with BindParam {
    def calcValue(in: NodeSeq): Option[NodeSeq] = Some(Text(value))
  }

  object TheStrBindParam {
    def apply(name: String, value: String) = new TheStrBindParam(name, value)
  }

  /**
   * BindParam that binds a given value into a new attribute.
   * For example, given the following markup:
   *
   * <pre name="code" class="xml">
   * &lt;lift:AttrBinds &gt;
   *   &lt;div test:w="foo" /&gt;
   *   &lt;div test:x="foo" /&gt;
   *   &lt;div test:y="foo" /&gt;
   *   &lt;div test:z="foo" /&gt;
   * &lt;/lift:AttrBinds &gt;
   * </pre>
   *
   * The following snippet:
   *
   * <pre name="code" class="scala">
   * import scala.xml._
   * class AttrBinds {
   *   def render(xhtml : NodeSeq) : NodeSeq =
   *     BindHelpers.bind("test", xhtml,
   *       AttrBindParam("w", Text("fooW"), "id"),
   *       AttrBindParam("x", "fooX", "id"),
   *       AttrBindParam("y", Text("fooW"), ("lift","calcId")),
   *       AttrBindParam("z", "fooZ", ("lift", "calcId")))
   * </pre>
   *
   * produces this markup:
   *
   * <pre name="code" class="xml">
   *   &lt;div id="fooW" /&gt;
   *   &lt;div id="fooX" /&gt;
   *   &lt;div lift:calcId="fooY" /&gt;
   *   &lt;div lift:calcId="fooZ" /&gt;
   * </pre>
   *
   * @param name the name of the binding to replace
   * @param myValue the value of the new attribute
   * @param newAttr The new attribute label
   */
  final class AttrBindParam(val name: String, myValue: => NodeSeq, val newAttr: String)
          extends BindParam with BindWithAttr {
    def calcValue(in: NodeSeq): Option[NodeSeq] = Some(myValue)
  }

 
  /**
   * BindParam that binds a given value into a new attribute.
   *   
   * This object provides factory methods for convenience.
   */
  object AttrBindParam {
    /**
     * Returns an unprefixed attribute binding containing the specified NodeSeq
     *
     * @param name The name to bind against
     * @param myValue The value to place in the new attribute
     * @param newAttr The new attribute label
     */
    def apply(name: String, myValue: => NodeSeq, newAttr: String) = 
      new AttrBindParam(name, myValue, newAttr)

    /**
     * Returns an unprefixed attribute binding containing the specified String
     * wrapped in a Text() element
     *
     * @param name The name to bind against
     * @param myValue The value to place in the new attribute
     * @param newAttr The new attribute label
     */
    def apply(name: String, myValue: String, newAttr: String) = 
      new AttrBindParam(name, Text(myValue), newAttr)

    /**
     * Returns a prefixed attribute binding containing the specified NodeSeq
     * 
     * @param name The name to bind against
     * @param myValue The value to place in the new attribute
     * @param newAttr The new attribute in the form (prefix,label)
     */
    def apply(name: String, myValue: => NodeSeq, newAttr: Pair[String,String]) = 
      PrefixedBindWithAttr(newAttr._1, new AttrBindParam(name, myValue, newAttr._2))

    /**
     * Returns a prefixed attribute binding containing the specified String
     * wrapped in a Text() element
     * 
     * @param name The name to bind against
     * @param myValue The value to place in the new attribute
     * @param newAttr The new attribute in the form (prefix,label)
     */
    def apply(name: String, myValue: String, newAttr: Pair[String,String]) = 
      PrefixedBindWithAttr(newAttr._1, new AttrBindParam(name, Text(myValue), newAttr._2))
  }

  /**
   * BindParam using a function to calculate its value
   */
  final class FuncBindParam(val name: String, value: NodeSeq => NodeSeq)
          extends Tuple2(name, value) with BindParam {
    def calcValue(in: NodeSeq): Option[NodeSeq] = Some(value(in))
  }

  object FuncBindParam {
    def apply(name: String, value: NodeSeq => NodeSeq) = new FuncBindParam(name, value)
  }

  /**
   * BindParam that computes a new attribute value based on the current
   * attribute value. For example, given the following markup:
   *
   * <pre name="code" class="xml">
   * &lt;lift:AttrBinds &gt;
   *   &lt;div test:x="foo" /&gt;
   *   &lt;div test:y="foo" /&gt;
   * &lt;/lift:AttrBinds &gt;
   * </pre>
   *
   * The following snippet:
   *
   * <pre name="code" class="scala">
   * import scala.xml._
   * class AttrBinds {
   *   def render(xhtml : NodeSeq) : NodeSeq =
   *     BindHelpers.bind("test", xhtml,
   *       FuncAttrBindParam("x", { ns : NodeSeq => Text(ns.text.toUpperCase + "X")}, "id"),
   *       FuncAttrBindParam("y", { ns : NodeSeq => Text(ns.text.length + "Y")}, ("lift","calcId")))
   * </pre>
   *
   * produces this markup:
   *
   * <pre name="code" class="xml">
   *   &lt;div id="FOOX" /&gt;
   *   &lt;div lift:calcId="3Y" /&gt;
   * </pre>
   *   
   * @param name The name to bind against
   * @param value A function that takes the current attribute's value and computes
   * the new attribute value
   * @param newAttr The new attribute label
   */
  final class FuncAttrBindParam(val name: String, value: => NodeSeq => NodeSeq, val newAttr: String)
          extends BindParam with BindWithAttr {
    def calcValue(in: NodeSeq): Option[NodeSeq] = Some(value(in))
  }

  /**
   * BindParam using a function to calculate its value.
   * 
   * This object provides factory methods for convenience.
   *
   */
  object FuncAttrBindParam {
    /**
     * Returns an unprefixed attribute binding computed by the provided function
     *
     * @param name The name to bind against
     * @param value The function that will transform the original attribute value
     * into the new attribute value
     * @param newAttr The new attribute label
     */
    def apply(name: String, value: => NodeSeq => NodeSeq, newAttr: String) = new FuncAttrBindParam(name, value, newAttr)

    /**
     * Returns a prefixed attribute binding computed by the provided function
     * 
     * @param name The name to bind against
     * @param value The function that will transform the original attribute value
     * into the new attribute value
     * @param newAttr The new attribute name in the form (prefix,label)
     */
    def apply(name: String, value: => NodeSeq => NodeSeq, newAttr: Pair[String,String]) = 
      PrefixedBindWithAttr(newAttr._1, new FuncAttrBindParam(name, value, newAttr._2))
  }

  final class OptionBindParam(val name: String, value: Option[NodeSeq])
          extends Tuple2(name, value) with BindParam {
    def calcValue(in: NodeSeq): Option[NodeSeq] = value
  }

  object OptionBindParam {
    def apply(name: String, value: Option[NodeSeq]) = new OptionBindParam(name, value)
  }

  final class BoxBindParam(val name: String, value: Box[NodeSeq])
          extends Tuple2(name, value) with BindParam {
    def calcValue(in: NodeSeq): Option[NodeSeq] = value
  }

  object BoxBindParam {
    def apply(name: String, value: Box[NodeSeq]) = new BoxBindParam(name, value)
  }

  /**
   * BindParam that computes an optional new attribute value based on
   * the current attribute value. Returning None in the transform function
   * will result in the Attribute being omitted. For example, given the
   * following markup:
   *
   * <pre name="code" class="xml">
   * &lt;lift:AttrBinds>
   *   &lt;div test:x="foo">
   *   &lt;div test:y="foo">
   * &lt;/lift:AttrBinds>
   * </pre>
   *
   * The following snippet:
   *
 <pre name="code" class="scala">
 import scala.xml._
 class AttrBinds {
   def render(xhtml : NodeSeq) : NodeSeq =
     BindHelpers.bind("test", xhtml,
       FuncAttrOptionBindParam("x", { ns : NodeSeq =>
           Some(Text(ns.text.toUpperCase + "X"))
         }, ("lift","calcId")),
       FuncAttrOptionBindParam("y", { ns : NodeSeq =>
         if (ns.text.length > 10) {
           Some(Text(ns.text.length + "Y"))
         } else {
           None
         }, ("lift","calcId")))
 </pre>
   *
   * produces this markup:
   *
   * <pre name="code" class="xml">
   *   &lt;div lift:calcId="FOOX" />
   *   &lt;div />
   * </pre>
   *
   * @param name The name to bind against
   * @param value The function that will transform the original attribute value
   * into the new attribute value. Returning None will cause this attribute to
   * be omitted.
   * @param newAttr The new attribute label
   *   
   */
  final class FuncAttrOptionBindParam(val name: String, func: => NodeSeq => Option[NodeSeq], val newAttr: String)
          extends BindParam with BindWithAttr {
    def calcValue(in: NodeSeq): Option[NodeSeq] = func(in)
  }

  /**
   * BindParam that computes an optional new attribute value based on
   * the current attribute value. Returning None in the transform function
   * will result in the Attribute not being bound.
   *
   * This object provides factory methods for convenience.
   */
  object FuncAttrOptionBindParam {
    /**
     * Returns an unprefixed attribute binding computed by the provided function
     *
     * @param name The name to bind against
     * @param value The function that will transform the original attribute value
     * into the new attribute value. Returning None will cause this attribute to
     * be omitted.
     * @param newAttr The new attribute label
     */
    def apply(name: String, func: => NodeSeq => Option[NodeSeq], newAttr: String) =
      new FuncAttrOptionBindParam(name, func, newAttr)


    /**
     * Returns a prefixed attribute binding computed by the provided function
     * 
     * @param name The name to bind against
     * @param value The function that will transform the original attribute value
     * into the new attribute value. Returning None will cause this attribute to
     * be omitted.
     * @param newAttr The new attribute name in the form (prefix,label)
     */
    def apply(name: String, func: => NodeSeq => Option[NodeSeq], newAttr: Pair[String,String]) =
      PrefixedBindWithAttr(newAttr._1, new FuncAttrOptionBindParam(name, func, newAttr._2))
  }

  /**
   * BindParam that computes an optional new attribute value based on
   * the current attribute value. Returning Empty in the transform function
   * will result in the Attribute being omitted. For example, given the
   * following markup:
   *
   * <pre name="code" class="xml">
   * &lt;lift:AttrBinds>
   *   &lt;div test:x="foo">
   *   &lt;div test:y="foo">
   * &lt;/lift:AttrBinds>
   * </pre>
   *
   * The following snippet:
   *
 <pre name="code" class="scala">
 import scala.xml._
 class AttrBinds {
   def render(xhtml : NodeSeq) : NodeSeq =
     BindHelpers.bind("test", xhtml,
       FuncAttrBoxBindParam("x", { ns : NodeSeq => Full(Text(ns.text.toUpperCase + "X"))}, ("lift","calcId")),
       FuncAttrBoxBindParam("y", { ns : NodeSeq =>
         if (ns.text.length > 10) {
           Full(Text(ns.text.length + "Y"))
         } else {
           Empty
         }, ("lift","calcId")))
 </pre>
   *
   * produces this markup:
   *
   * <pre name="code" class="xml">
   *   &lt;div lift:calcId="FOOX" />
   *   &lt;div />
   * </pre>
   *   
   * @param name The name to bind against
   * @param value The function that will transform the original attribute value
   * into the new attribute value. Returning Empty will cause this attribute to
   * be omitted.
   * @param newAttr The new attribute label
   *
   */
  final class FuncAttrBoxBindParam(val name: String, func: => NodeSeq => Box[NodeSeq], val newAttr: String)
          extends BindParam with BindWithAttr {
    def calcValue(in: NodeSeq): Option[NodeSeq] = func(in)
  }

  /**
   * BindParam that computes an optional new attribute value based on
   * the current attribute value. Returning Empty in the transform function
   * will result in the Attribute being omitted.
   *
   * This object provides factory methods for convenience.
   */
  object FuncAttrBoxBindParam {
    /**
     * Returns an unprefixed attribute binding computed by the provided function
     *
     * @param name The name to bind against
     * @param value The function that will transform the original attribute value
     * into the new attribute value. Returning Empty will cause this attribute to
     * be omitted.
     * @param newAttr The new attribute label
     */
    def apply(name: String, func: => NodeSeq => Box[NodeSeq], newAttr: String) =
      new FuncAttrBoxBindParam(name, func, newAttr)

    /**
     * Returns a prefixed attribute binding computed by the provided function
     * 
     * @param name The name to bind against
     * @param value The function that will transform the original attribute value
     * into the new attribute value. Returning Empty will cause this attribute to
     * be omitted.
     * @param newAttr The new attribute name in the form (prefix,label)
     */
    def apply(name: String, func: => NodeSeq => Box[NodeSeq], newAttr: Pair[String,String]) =
      PrefixedBindWithAttr(newAttr._1, new FuncAttrBoxBindParam(name, func, newAttr._2))
  }

  final class SymbolBindParam(val name: String, value: Symbol)
          extends Tuple2(name, value) with BindParam {
    def calcValue(in: NodeSeq): Option[NodeSeq] = Some(Text(value.name))
  }

  object SymbolBindParam {
    def apply(name: String, value: Symbol) = new SymbolBindParam(name, value)
  }

  final class IntBindParam(val name: String, value: Int)
          extends Tuple2[String, Int](name, value) with BindParam {
    def calcValue(in: NodeSeq): Option[NodeSeq] = Some(Text(value.toString))
  }

  object IntBindParam {
    def apply(name: String, value: Int) = new IntBindParam(name, value)
  }

  final class LongBindParam(val name: String, value: Long)
          extends Tuple2[String, Long](name, value) with BindParam {
    def calcValue(in: NodeSeq): Option[NodeSeq] = Some(Text(value.toString))
  }

  object LongBindParam {
    def apply(name: String, value: Long) = new LongBindParam(name, value)
  }

  final class BooleanBindParam(val name: String, value: Boolean)
          extends Tuple2[String, Boolean](name, value) with BindParam {
    def calcValue(in: NodeSeq): Option[NodeSeq] = Some(Text(value.toString))
  }

  object BooleanBindParam {
    def apply(name: String, value: Boolean) = new BooleanBindParam(name, value)
  }

  final class TheBindableBindParam[T <: Bindable](val name: String, value: T)
          extends Tuple2[String, T](name, value) with BindParam {
    def calcValue(in: NodeSeq): Option[NodeSeq] = Some(value.asHtml)
  }

  object TheBindableBindParam {
    def apply[T <: Bindable](name: String, value: T) = new TheBindableBindParam(name, value)
  }

  /**
   * Remove all the <head> tags, just leaving the child tags
   */
  def stripHead(in: NodeSeq): NodeSeq = {
    import scala.xml.transform._

    val rewrite = new RewriteRule {
      override def transform(in: Node): Seq[Node] = in match {
        case e: Elem if e.label == "head" && (e.namespace eq null) => this.transform(e.child)
        case x => x
      }
    }

    (new RuleTransformer(rewrite)).transform(in)
  }

  /**
   *  transforms a Box into a Text node
   */
  object BindParamAssoc {
    implicit def canStrBoxNodeSeq(in: Box[Any]): Box[NodeSeq] = in.map(_ match {
      case null => Text("null")
      case v => Text(v.toString)
    })
  }

  /**
   * takes a NodeSeq and applies all the attributes to all the Elems at the top level of the
   * NodeSeq.  The id attribute is applied to the first-found Elem only
   */
  def addAttributes(in: NodeSeq, attributes: MetaData): NodeSeq = {
    if (attributes == Null) in
    else {
      val noId = attributes.filter(_.key != "id")
      var doneId = false
      in map {
        case e: Elem =>
          if (doneId) e % noId else {
            doneId = true
            e % attributes
          }
        case x => x
      }
    }
  }

  private def snToNs(in: Seq[Node]): NodeSeq = in

  class SuperArrowAssoc(name: String) {
    // Because JsObj is a subclass of Node, we don't want it
    // getting caught because it's not a bind param
    def ->[T <: SpecialNode](in: T with SpecialNode) = Tuple2[String, T](name, in)

    def ->(in: String) = TheStrBindParam(name, in)
    def ->(in: NodeSeq) = TheBindParam(name, in)
    def ->(in: Text) = TheBindParam(name, in)
    def ->(in: Node) = TheBindParam(name, in)
    def ->(in: Seq[Node]) = TheBindParam(name, in)
    def ->(in: NodeSeq => NodeSeq) = FuncBindParam(name, in)
    def ->(in: Box[NodeSeq]) = BoxBindParam(name, in)
    def ->(in: Option[NodeSeq]) = OptionBindParam(name, in)
    def ->(in: Symbol) = SymbolBindParam(name, in)
    def ->(in: Int) = IntBindParam(name, in)
    def ->(in: Long) = LongBindParam(name, in)
    def ->(in: Boolean) = BooleanBindParam(name, in)
    def ->[T <: Bindable](in: T with Bindable) = TheBindableBindParam[T](name, in)
    def ->[T](in: T) = Tuple2[String, T](name, in)

    def -%>(in: NodeSeq) = FuncBindParam(name, old => addAttributes(in , (BindHelpers.currentNode.map(_.attributes) openOr Null)))
    def -%>(in: Box[NodeSeq]) = FuncBindParam(name, 
                                              old => in.map(a => addAttributes(a, 
                                                                               (BindHelpers.currentNode.map(_.attributes) openOr Null))) openOr
                                              NodeSeq.Empty)
    
    def -%>(in: Option[NodeSeq]) = FuncBindParam(name, old => in.map(a => addAttributes(a,
                                                                                        (BindHelpers.currentNode.map(_.attributes) openOr
                                                                                         Null))) getOrElse NodeSeq.Empty)
                                                 
    def -%>(in: NodeSeq => NodeSeq) = FuncBindParam(name, old => addAttributes(in(old),
                                                                               (BindHelpers.currentNode.map(_.attributes) openOr Null)))

    def _id_>(in: Elem) = FuncBindParam(name, _ => in % new UnprefixedAttribute("id", name, Null))
    def _id_>(in: Box[Elem]) = FuncBindParam(name, _ => in.map(_ % new UnprefixedAttribute("id", name, Null)) openOr NodeSeq.Empty)
    def _id_>(in: Option[Elem]) = FuncBindParam(name, _ => in.map(_ % new UnprefixedAttribute("id", name, Null)) getOrElse NodeSeq.Empty)
    def _id_>(in: NodeSeq => Elem) = FuncBindParam(name, kids => in(kids) % new UnprefixedAttribute("id", name, Null))

  }

  implicit def strToSuperArrowAssoc(in: String): SuperArrowAssoc = new SuperArrowAssoc(in)


  /**
   * This class creates a BindParam from an input value
   *
   * @deprecated use -> instead
   */
  @deprecated
  class BindParamAssoc(val name: String) {
    def -->(value: String): BindParam = TheBindParam(name, Text(value))
    def -->(value: NodeSeq): BindParam = TheBindParam(name, value)
    def -->(value: Symbol): BindParam = TheBindParam(name, Text(value.name))
    def -->(value: Any): BindParam = TheBindParam(name, Text(if (value == null) "null" else value.toString))
    def -->(func: NodeSeq => NodeSeq): BindParam = FuncBindParam(name, func)
    def -->(value: Box[NodeSeq]): BindParam = TheBindParam(name, value.openOr(Text("Empty")))
  }

  /**
   * transforms a String to a BindParamAssoc object which can be associated to a BindParam object
   * using the --> operator.<p/>
   * Usage: <code>"David" --> "name"</code>
   *
   * @deprecated use -> instead
   */
  @deprecated
  implicit def strToBPAssoc(in: String): BindParamAssoc = new BindParamAssoc(in)

  /**
   * transforms a Symbol to a BindParamAssoc object which can be associated to a BindParam object
   * using the --> operator.<p/>
   * Usage: <code>'David --> "name"</code>
   *
   * @deprecated use -> instead
   */
  @deprecated
  implicit def symToBPAssoc(in: Symbol): BindParamAssoc = new BindParamAssoc(in.name)

  /**
   * Experimental extension to bind which passes in an additional "parameter" from the XHTML to the transform
   * function, which can be used to format the returned NodeSeq.
   *
   * @deprecated use bind instead
   */
  @deprecated
  def xbind(namespace: String, xml: NodeSeq)(transform: PartialFunction[String, NodeSeq => NodeSeq]): NodeSeq = {
    def rec_xbind(xml: NodeSeq): NodeSeq = {
      xml.flatMap {
        node => node match {
          case s: Elem if (node.prefix == namespace) =>
            if (transform.isDefinedAt(node.label))
              transform(node.label)(node)
            else
              Text("FIX"+"ME failed to bind <"+namespace+":"+node.label+" />")
          case Group(nodes) => Group(rec_xbind(nodes))
          case s: Elem => Elem(node.prefix, node.label, node.attributes, node.scope, rec_xbind(node.child): _*)
          case n => node
        }
      }
    }

    rec_xbind(xml)
  }

  /**
   * Bind a set of values to parameters and attributes in a block of XML.<p/>
   *
   * For example: <pre name="code" class="scala">
   *   bind("user", <user:hello>replace this</user:hello>, "hello" -> <h1/>)
   * </pre>
   * will return <pre><h1></h1></pre>

   * @param namespace the namespace of tags to bind
   * @param xml the NodeSeq in which to find elements to be bound.
   * @param params the list of BindParam bindings to be applied
   *
   * @return the NodeSeq that results from the specified transforms
   */
  def bind(namespace: String, xml: NodeSeq, params: BindParam*): NodeSeq =
    bind(namespace, Empty, Empty, xml, params: _*)


  /**
   * Bind a set of values to parameters and attributes in a block of XML
   * with defined transforms for unbound elements within the specified
   * namespace.<p/>
   *
   * For example:<pre name="code" class="scala">
   *   bind("user",
   *        Full(xhtml: NodeSeq => Text("Default Value")),
   *        Empty,
   *        <user:hello>replace this</user:hello><user:dflt>replace with default</user:dflt>,
   *        "hello" -> <h1/>)
   * </pre>
   * will return <pre><h1></h1>Default Value</pre>
   *
   * @param namespace the namespace of tags to bind
   * @param nodeFailureXform a box containing the function to use as the default transform
   *        for tags in the specified namespace that do not have bindings specified.
   * @param paramFailureXform a box containing the function to use as the default transform
   *        for unrecognized attributes in bound elements.
   * @param xml the NodeSeq in which to find elements to be bound.
   * @param params the list of BindParam bindings to be applied
   *
   * @return the NodeSeq that results from the specified transforms
   */
  def bind(namespace: String, nodeFailureXform: Box[NodeSeq => NodeSeq],
           paramFailureXform: Box[PrefixedAttribute => MetaData],
           xml: NodeSeq, params: BindParam*): NodeSeq =
    bind(namespace, nodeFailureXform, paramFailureXform, false, xml, params: _*)

  /**
   * Bind a set of values to parameters and attributes in a block of XML
   * with defined transforms for unbound elements within the specified
   * namespace.<p/>
   *
   * For example:<pre name="code" class="scala">
   *   bind("user",
   *        Full(xhtml: NodeSeq => Text("Default Value")),
   *        Empty,
   *        <user:hello>replace this</user:hello><user:dflt>replace with default</user:dflt>,
   *        "hello" -> <h1/>)
   * </pre>
   * will return <pre><h1></h1>Default Value</pre>
   *
   * @param namespace the namespace of tags to bind
   * @param nodeFailureXform a box containing the function to use as the default transform
   *        for tags in the specified namespace that do not have bindings specified.
   * @param paramFailureXform a box containing the function to use as the default transform
   *        for unrecognized attributes in bound elements.
   * @param preserveScope: true if the scope should be preserved, false is the normal setting
   * @param xml the NodeSeq in which to find elements to be bound.
   * @param params the list of BindParam bindings to be applied
   *
   * @return the NodeSeq that results from the specified transforms
   */
  def bind(namespace: String, nodeFailureXform: Box[NodeSeq => NodeSeq],
           paramFailureXform: Box[PrefixedAttribute => MetaData],
           preserveScope: Boolean,
           xml: NodeSeq, params: BindParam*): NodeSeq = {
    BindHelpers._bindNodes.doWith(xml :: (BindHelpers._bindNodes.box.openOr(Nil))) {
      val map: _root_.scala.collection.immutable.Map[String, BindParam] = _root_.scala.collection.immutable.HashMap.empty ++ params.map(p => (p.name, p))

      def attrBind(attr: MetaData): MetaData = attr match {
        case Null => Null
        case upa: UnprefixedAttribute => new UnprefixedAttribute(upa.key, upa.value, attrBind(upa.next))
        case pa: PrefixedAttribute if pa.pre == namespace => map.get(pa.key) match {
          case None => paramFailureXform.map(_(pa)) openOr new PrefixedAttribute(pa.pre, pa.key, Text("FIX"+"ME find to bind attribute"), attrBind(pa.next))
          case Some(PrefixedBindWithAttr(prefix,binding)) => binding.calcValue(pa.value).map(v => new PrefixedAttribute(prefix, binding.newAttr, v, attrBind(pa.next))) getOrElse attrBind(pa.next)
          case Some(abp: BindWithAttr) => abp.calcValue(pa.value).map(v => new UnprefixedAttribute(abp.newAttr, v, attrBind(pa.next))) getOrElse attrBind(pa.next)
          case Some(bp: BindParam) => bp.calcValue(pa.value).map(v => new PrefixedAttribute(pa.pre, pa.key, v, attrBind(pa.next))) getOrElse attrBind(pa.next)
        }
        case pa: PrefixedAttribute => new PrefixedAttribute(pa.pre, pa.key, pa.value, attrBind(pa.next))
      }

      def in_bind(xml: NodeSeq): NodeSeq = {
        xml.flatMap {
          case s: Elem if s.prefix == namespace => BindHelpers._currentNode.doWith(s) {
            map.get(s.label) match {
              case None =>
                nodeFailureXform.map(_(s)) openOr s

              case Some(ns) =>
                //val toRet = ns.calcValue(s.child)
                //mergeBindAttrs(toRet, namespace, s.attributes)
                ns.calcValue(s.child) getOrElse NodeSeq.Empty
            }
          }
          case s: Elem if bindByNameType(s.label) && (attrStr(s, "name").startsWith(namespace+":")) &&
                          bindByNameTag(namespace, s) != "" => BindHelpers._currentNode.doWith(s) {
            val tag = bindByNameTag(namespace, s)
            map.get(tag) match {
              case None => nodeFailureXform.map(_(s)) openOr s
              case Some(bindParam) => bindByNameMixIn(bindParam, s)
            }
          }
          case Group(nodes) => Group(in_bind(nodes))
          case s: Elem => Elem(s.prefix, s.label, attrBind(s.attributes), if (preserveScope) s.scope else TopScope,
                               in_bind(s.child): _*)
          case n => n
        }
      }

      in_bind(xml)
    }
  }

  private def setElemId(in: NodeSeq, attr: String, value: Seq[Node]): NodeSeq =
    in.map {
      case e: Elem => e % new UnprefixedAttribute(attr, value, Null)
      case v => v
    }

  /*
   private def mergeBindAttrs(in: NodeSeq, nameSpace: String, attrs: MetaData): NodeSeq = attrs match {
   case Null => in
   case p: PrefixedAttribute if p.pre == nameSpace =>
   mergeBindAttrs(setElemId(in, p.key, p.value), nameSpace, p.next)
   case m => mergeBindAttrs(in, nameSpace, m.next)
   }
   */

  /**
   * Replace the content of lift:bind nodes with the corresponding nodes found in a map,
   * according to the value of the "name" attribute.<p/>
   * Usage: <pre name="code" class="scala">
   *   bind(Map("a" -> <h1/>), <b><lift:bind name="a">change this</lift:bind></b>) must ==/(<b><h1></h1></b>)
   * </pre>
   *
   * @param vals map of name/nodes to replace
   * @param xml nodes containing lift:bind nodes
   *
   * @return the NodeSeq that results from the specified transforms
   */
  def bind(vals: Map[String, NodeSeq], xml: NodeSeq): NodeSeq = 
    bind(vals,xml,true,_root_.scala.collection.mutable.Set(vals.keySet.toSeq : _*))

  /**
   * This method exists so that we can do recursive binding with only root-node
   * error reporting.
   * 
   * Replace the content of lift:bind nodes with the corresponding nodes found in a map,
   * according to the value of the "name" attribute.<p/>
   * Usage: <pre name="code" class="scala">
   *   bind(Map("a" -> <h1/>), <b><lift:bind name="a">change this</lift:bind></b>) must ==/(<b><h1></h1></b>)
   * </pre>
   *
   * @param vals map of name/nodes to replace
   * @param xml nodes containing lift:bind nodes
   * @param reportUnused If true, report unused binding vals to the log
   * @param unusedBindings The set of unused binding values. Mutable for expediency, but it would be
   * nice to figure out a cleaner way
   *
   * @return the NodeSeq that results from the specified transforms
   */
  private def bind(vals: Map[String, NodeSeq], xml: NodeSeq, reportUnused : Boolean, unusedBindings : _root_.scala.collection.mutable.Set[String]): NodeSeq = {
    val isBind = (node: Elem) => {
      node.prefix == "lift" && node.label == "bind"
    }

    val bindResult = xml.flatMap {
      node => node match {
        case s: Elem if (isBind(s)) => {
          node.attributes.get("name") match {
            case None => {
              if (Props.devMode) {
                logger.warn("<lift:bind> tag encountered without name attribute!")
              }
              bind(vals, node.child, false, unusedBindings)
            }
            case Some(ns) => {
              vals.get(ns.text) match {
                case None => {
                  if (Props.devMode) {
                    logger.warn("No binding values match the <lift:bind> name attribute: " + ns.text)
                  }
                  bind(vals, node.child, false, unusedBindings)
                }
                case Some(nodes) => {
                  // Mark this bind as used by removing from the unused Set
                  unusedBindings -= ns.text
                  nodes
                }
              }
            }
          }
        }
        case Group(nodes) => Group(bind(vals, nodes))
        case s: Elem => Elem(node.prefix, node.label, node.attributes, node.scope, bind(vals, node.child, false, unusedBindings): _*)
        case n => node
      }
    }

    if (Props.devMode && reportUnused && unusedBindings.size > 0) {
      logger.warn("Unused binding values for <lift:bind>: " + unusedBindings.mkString(", "))
    }

    bindResult
  }

  /**
   * Bind a list of name/xml maps to a block of XML containing lift:bind nodes (see the bind(Map, NodeSeq) function)
   * @return the NodeSeq that results from the specified transforms
   */
  def bindlist(listvals: List[Map[String, NodeSeq]], xml: NodeSeq): Box[NodeSeq] = {
    def build(listvals: List[Map[String, NodeSeq]], ret: NodeSeq): NodeSeq = listvals match {
      case Nil => ret
      case vals :: rest => build(rest, ret ++ bind(vals, xml))
    }
    if (listvals.length > 0) Full(build(listvals.drop(1), bind(listvals.head, xml)))
    else Empty
  }

  /**
   * Bind parameters to XML.
   *
   * @param around XML with lift:bind elements
   * @param atWhat data to bind
   * @deprecated use the bind function instead
   */
  @deprecated
  def processBind(around: NodeSeq, atWhat: Map[String, NodeSeq]): NodeSeq = {

    /** Find element matched predicate f(x).isDefined, and return f(x) if found or None otherwise. */
    def findMap[A, B](s: Iterable[A])(f: A => Option[B]): Option[B] =
      s.projection.map(f).find(_.isDefined).getOrElse(None)

    around.flatMap {
      v =>
        v match {
          case Group(nodes) => Group(processBind(nodes, atWhat))
          case Elem("lift", "bind", attr @ _, _, kids @ _*) =>
            findMap(atWhat) {
              case (at, what) if attr("name").text == at => Some(what)
              case _ => None
            }.getOrElse(processBind(v.asInstanceOf[Elem].child, atWhat))

          case e: Elem => {Elem(e.prefix, e.label, e.attributes, e.scope, processBind(e.child, atWhat): _*)}
          case _ => {v}
        }

    }
  }

  /**
   * Finds the named attribute in specified XML element and returns
   * a Full Box containing the value of the attribute if found.
   * Empty otherwise.
   *
   * @return a Full Box containing the value of the attribute if found; Empty otherwise
   */
  def xmlParam(in: NodeSeq, param: String): Box[String] = {
    val tmp = (in \ ("@" + param))
    if (tmp.length == 0) Empty else Full(tmp.text)
  }

  /**
   * Given a NodeSeq and a function that returns an Option[T],
   * return the first value found in which the function evaluates
   * to Some
   */
  def findOption[T](nodes: Seq[Node])(f: Elem => Option[T]): Option[T] = {
    nodes.projection.flatMap {
      case Group(g) => findOption(g)(f)
      case e: Elem => f(e) orElse findOption(e.child)(f)
      case _ => None
    }.firstOption
  }

  /**
   * Given an id value, find the Elem with the specified id
   */
  def findId(nodes: Seq[Node], id: String): Option[Elem] =
    findOption(nodes) {
      e => e.attribute("id").filter(_.text == id).map(i => e)
    }

  /**
   * Given a NodeSeq and a function that returns a Box[T],
   * return the first value found in which the function evaluates
   * to Full
   */
  def findBox[T](nodes: Seq[Node])(f: Elem => Box[T]): Box[T] = {
    nodes.projection.flatMap {
      case Group(g) => findBox(g)(f)
      case e: Elem => f(e) or findBox(e.child)(f)
      case _ => Empty
    }.firstOption
  }

  /**
   * Finds and returns the first node in the specified NodeSeq and its children
   * with the same label and prefix as the specified element.
   */
  def findNode(in: Elem, nodes: NodeSeq): Box[Elem] = nodes match {
    case seq if seq.isEmpty => Empty
    case Seq(x: Elem, xs @_*)
      if x.label == in.label && x.prefix == in.prefix => Full(x)
    case Seq(x, xs @_*) => findNode(in, x.child) or findNode(in, xs)
  }

  // get the attribute string or blank string if it doesnt exist
  private def attrStr(elem: Elem, attr: String): String = elem.attributes.get(attr) match {
    case None => ""
    case Some(Nil) => "" // why is a blank string converted to a List
    case Some(x) => x.toString // get string on scala.xml.Text
  }

  // types that can be bindByName
  private def bindByNameType(b: String) = b == "input" || b == "select" || b == "button" || b == "a"

  // allow bind by name eg - <input name="namespace:tag"/>
  private def bindByNameTag(namespace: String, elem: Elem) =
    attrStr(elem, "name").replaceAll(namespace+":", "")


  // mixin what comes from xhtml with what is programatically added
  private def bindByNameMixIn(bindParam: BindParam, s: Elem): NodeSeq = {
    def mix(nodeSeq: NodeSeq): NodeSeq = nodeSeq match {
      case elem: Elem =>
        // mix in undefined attributes
        val attributes = s.attributes.filter(attr => !elem.attribute(attr.key).isDefined)
        elem % attributes
      case Seq(x1: Elem, x2: Elem) if attrStr(x2, "type") == "checkbox" =>
        x1 ++ mix(x2)

      case other =>
        other
    }
    mix(bindParam.calcValue(s) getOrElse NodeSeq.Empty)

  }

  /**
   * promote a String to a ToCssBindPromotor
   */
  implicit def strToCssBindPromoter(str: String): ToCssBindPromoter =
    new ToCssBindPromoter(Full(str), CssSelectorParser.parse(str))

  /**
   * promote a String to a ToCssBindPromotor
   */
  implicit def cssSelectorToCssBindPromoter(sel: CssSelector): ToCssBindPromoter =
    new ToCssBindPromoter(Empty, Full(sel))
}

/**
 * An intermediate class used to promote a String or a CssSelector to
 * something that can be associated with a value to apply to the selector
 */
final class ToCssBindPromoter(stringSelector: Box[String], css: Box[CssSelector]) {

  /**
   * Inserts a String constant according to the CssSelector rules
   */
  def #>(str: String): CssBind = new CssBindImpl(stringSelector, css) {
    def calculate(in: NodeSeq): Seq[NodeSeq] = List(Text(str))
  }

  /**
   * Inserts a NodeSeq constant according to the CssSelector rules
   */
  def #>(ns: NodeSeq): CssBind = new CssBindImpl(stringSelector, css) {
    def calculate(in: NodeSeq): Seq[NodeSeq] = List(ns)
  }

  /**
   * A function that transforms the content according to the CssSelector rules
   */
  def #>(nsFunc: NodeSeq => NodeSeq): CssBind = new CssBindImpl(stringSelector, css) {
    def calculate(in: NodeSeq): Seq[NodeSeq] = List(nsFunc(in))
  }
  
  /**
   * Inserts a Bindable constant according to the CssSelector rules.
   * Mapper and Record fields implement Bindable.
   */
  def #>(bindable: Bindable): CssBind = new CssBindImpl(stringSelector, css) {
    def calculate(in: NodeSeq): Seq[NodeSeq] = List(bindable.asHtml)
  }

  /**
   * Inserts a StringPromotable constant according to the CssSelector rules.
   * StringPromotable includes Int, Long, Boolean, and Symbol
   */
  def #>(strPromo: StringPromotable): CssBind = new CssBindImpl(stringSelector, css) {
    def calculate(in: NodeSeq): Seq[NodeSeq] = List(Text(strPromo.toString))
  }

  /**
   * Applies the N constants according to the CssSelector rules.
   * This allows for Seq[String], Seq[NodeSeq], Box[String],
   * Box[NodeSeq], Option[String], Option[NodeSeq]
   */
  def #>(itrConst: IterableConst): CssBind = new CssBindImpl(stringSelector, css) {
    def calculate(in: NodeSeq): Seq[NodeSeq] = itrConst.constList
  }

  /**
   * Apply the function and then apply the results account the the CssSelector
   * rules.
   * This allows for NodeSeq => Seq[String], NodeSeq =>Seq[NodeSeq],
   * NodeSeq => Box[String],
   * NodeSeq => Box[NodeSeq], NodeSeq => Option[String],
   * NodeSeq =>Option[NodeSeq]
   */
  def #>(itrFunc: IterableFunc): CssBind = new CssBindImpl(stringSelector, css) {
    def calculate(in: NodeSeq): Seq[NodeSeq] = itrFunc(in)
  }
}

/**
 * A trait that has some helpful implicit conversions from
 * Iterable[NodeSeq], Seq[String], Box[String], and Option[String]
 */
trait IterableConst {
  def constList: Seq[NodeSeq]
}


/**
 * The companion object that does the helpful promotion of common
 * collection types into an IterableConst,
 * e.g. Iterable[NodeSeq], Seq[String], Box[String], and Option[String]
 */
object IterableConst {
  /**
   * Converts anything that can be converted into an Iterable[NodeSeq]
   * into an IterableConst.  This includes Seq[NodeSeq], Option[NodeSeq],
   * and Box[NodeSeq]
   */
  implicit def itNodeSeq[C <% Iterable[NodeSeq]](it: C): IterableConst =
    new IterableConst {
      def constList: Seq[NodeSeq] = it.toSeq
    }

  implicit def itStringPromotable(it: Seq[String]): IterableConst =
    new IterableConst {
      def constList: Seq[NodeSeq] = it.map(a => Text(a))
    }


  implicit def boxStringPromotable(it: Box[String]): IterableConst =
    new IterableConst {
      def constList: Seq[NodeSeq] = it.toList.map(a => Text(a))
    }


  implicit def optionStringPromotable(it: Option[String]): IterableConst =
    new IterableConst {
      def constList: Seq[NodeSeq] = it.toList.map(a => Text(a))
    }

  implicit def itBindablePromotable(it: Seq[Bindable]): IterableConst =
    new IterableConst {
      def constList: Seq[NodeSeq] = it.map(a => a.asHtml)
    }


  implicit def boxBindablePromotable(it: Box[Bindable]): IterableConst =
    new IterableConst {
      def constList: Seq[NodeSeq] = it.toList.map(a => a.asHtml)
    }


  implicit def optionBindablePromotable(it: Option[Bindable]): IterableConst =
    new IterableConst {
      def constList: Seq[NodeSeq] = it.toList.map(a => a.asHtml)
    }
}

sealed trait IterableFunc extends Function1[NodeSeq, Seq[NodeSeq]] {
  def apply(ns: NodeSeq): Seq[NodeSeq]
}

object IterableFunc {
  implicit def itNodeSeq[C <% Iterable[NodeSeq]](it: NodeSeq => C): IterableFunc =
    new IterableFunc {
      def apply(in: NodeSeq): Seq[NodeSeq] = it(in).toSeq
    }

  implicit def itNodeSeqPromotable(it: NodeSeq => NodeSeq): IterableFunc =
    new IterableFunc {
      def apply(in: NodeSeq): Seq[NodeSeq] = List(it(in))
    }


  implicit def itStringFuncPromotable(it: NodeSeq => String): IterableFunc =
    new IterableFunc {
      def apply(in: NodeSeq): Seq[NodeSeq] = List(Text(it(in)))
    }


  implicit def itStringPromotable(it: NodeSeq => Seq[String]): IterableFunc =
    new IterableFunc {
      def apply(in: NodeSeq): Seq[NodeSeq] = it(in).map(a => Text(a))
    }

  implicit def boxStringPromotable(it: NodeSeq => Box[String]): IterableFunc =
    new IterableFunc {
      def apply(in: NodeSeq): Seq[NodeSeq] = it(in).toList.map(a => Text(a))
    }


  implicit def optionStringPromotable(it: NodeSeq => Option[String]): IterableFunc =
    new IterableFunc {
      def apply(in: NodeSeq): Seq[NodeSeq] = it(in).toList.map(a => Text(a))
    }
}


/**
 * This trait marks something that can be promoted into a String.
 * The companion object has helpful conversions from Int,
 * Symbol, Long, and Boolean
 */
trait StringPromotable

object StringPromotable {
  implicit def intToStrPromo(in: Int): StringPromotable = 
    new StringPromotable {
      override val toString = in.toString
    }

  implicit def symbolToStrPromo(in: Symbol): StringPromotable = 
    new StringPromotable {
      override val toString = in.name
    }

  implicit def longToStrPromo(in: Long): StringPromotable = 
    new StringPromotable {
      override val toString = in.toString
    }

  implicit def booleanToStrPromo(in: Boolean): StringPromotable = 
    new StringPromotable {
      override val toString = in.toString
    }
}

/**
 * This trait is both a NodeSeq => NodeSeq and has the ability
 * to chain CssBindFunc instances so that they can be applied
 * en masse to incoming NodeSeq and do the transformation.
 */
sealed trait CssBindFunc extends Function1[NodeSeq, NodeSeq] {
  def &(other: CssBindFunc): CssBindFunc = (this, other) match {
    case (AggregatedCssBindFunc(a), AggregatedCssBindFunc(b)) =>
      AggregatedCssBindFunc(a ::: b)
    case (AggregatedCssBindFunc(a), o: CssBind) =>
      AggregatedCssBindFunc(a ::: List(o))
    case (t: CssBind, AggregatedCssBindFunc(a)) =>
      AggregatedCssBindFunc(t :: a)
    case (t: CssBind, o: CssBind) => AggregatedCssBindFunc(List(t, o))
  }
}

/**
 * A passthrough function that does not change the nodes
 *
 * @tag CssFunction
 */
object PassThru extends Function1[NodeSeq, NodeSeq] {
  def apply(in: NodeSeq): NodeSeq = in
}

/**
 * Replaces the nodes with an Empty NodeSeq.  Useful
 * for removing unused nodes
 *
 * @tag CssFunction
 */
object ClearNodes extends Function1[NodeSeq, NodeSeq] {
  def apply(in: NodeSeq): NodeSeq = NodeSeq.Empty
}



private final case class AggregatedCssBindFunc(binds: List[CssBind]) extends CssBindFunc {
  private lazy val (good, bad) = binds.partition{_.css.isDefined}
  private lazy val selectorMap = new SelectorMap(good)

  def apply(in: NodeSeq): NodeSeq = bad match {
    case Nil => selectorMap(in)
    case bv => bad.flatMap(_(in)) ++ selectorMap(in)
  }
}

/**
 * This CssBind will clear all nodes marked with the class
 * clearable.  Designers can mark extra nodes in markup with
 * class="clearable" and this Bind will make them go away
 */
object ClearClearable extends CssBindImpl(Full(".clearable"), CssSelectorParser.parse(".clearable")) {
  
  def calculate(in: NodeSeq): Seq[NodeSeq] = Nil
}

private class SelectorMap(binds: List[CssBind]) extends Function1[NodeSeq, NodeSeq] {
  private val (idMap, nameMap, clzMap, attrMap)  = {
    var idMap: Map[String, CssBind] = Map()
    var nameMap: Map[String, CssBind] = Map()
    var clzMap: Map[String, CssBind] = Map()
    var attrMap: Map[String, Map[String, CssBind]] = Map()

    binds.foreach {
      case i @ CssBind(IdSelector(id, _)) => if (!idMap.isDefinedAt(id)) {
        idMap += (id -> i)
      }

      case i @ CssBind(NameSelector(name, _)) => if (!nameMap.isDefinedAt(name)) {
        nameMap += (name -> i)
      }

      case i @ CssBind(ClassSelector(clz, _)) => if (!clzMap.isDefinedAt(clz)) {
        clzMap += (clz -> i)
      }

      case i @ CssBind(AttrSelector(name, value, _)) => {
        val oldMap = attrMap.getOrElse(name, Map())
        if (!oldMap.isDefinedAt(value)) {
          attrMap += (name -> (oldMap + (value -> i)))
        }
      }

      case _ =>
    }

    (idMap, nameMap, clzMap, attrMap)
  }

  private abstract class SlurpedAttrs(val id: Box[String],val name: Box[String]) {
    def attrs: Map[String, String]
    def classes: List[String]

    def removeId(in: MetaData) = in.filter {
      case up: UnprefixedAttribute => up.key != "id"
      case _ => true
    }

    def applyRule(bind: CssBind, realE: Elem): NodeSeq = {
      def mergeAll(other: MetaData, stripId: Boolean): MetaData = {
        var oldAttrs = attrs - (if (stripId) "id" else "")

        var builtMeta: MetaData = Null
        var pos = other
        
        while (pos != Null) {
          pos match {
            case up: UnprefixedAttribute if stripId && up.key == "id" =>
              // ignore the id attribute

            case up: UnprefixedAttribute if up.key == "class" => {
              oldAttrs.get("class") match {
                case Some(ca) => {
                  oldAttrs -= "class"
                  builtMeta = new UnprefixedAttribute("class",
                                                      up.value.text + " "+
                                                      ca, builtMeta)
                }

                case _ => builtMeta = up.copy(builtMeta)
              }
            }

            case up: UnprefixedAttribute => {
              oldAttrs -= up.key
              builtMeta = up.copy(builtMeta)
            }

            case pa: PrefixedAttribute => {
              oldAttrs -= (pa.pre+":"+pa.key)
              builtMeta = pa.copy(builtMeta)
            }
            case _ =>
          }

          pos = pos.next
        }
        
        for {
          (k, v) <- oldAttrs
        } {
          import Helpers._
          k.charSplit(':') match {
            case p :: k :: _ => 
              builtMeta = new PrefixedAttribute(p, k, v, builtMeta)
            case k :: _ => builtMeta = new UnprefixedAttribute(k, v, builtMeta)
            case _ =>
          }
        }

        builtMeta
      }

      // we can do an open_! here because all the CssBind elems
      // have been vetted
      bind.css.open_!.subNodes match {
        case Full(KidsSubNode()) => {
          val calced = bind.calculate(realE.child)
          calced.length match {
            case 0 => NodeSeq.Empty
            case 1 => new Elem(realE.prefix, realE.label, 
                               realE.attributes, realE.scope, calced.first :_*)
            case _ if id.isEmpty => 
              calced.map(kids => new Elem(realE.prefix, realE.label, 
                                          realE.attributes, realE.scope,
                                          kids :_*))

            case _ => {
              val noId = removeId(realE.attributes)
              calced.toList.zipWithIndex.map {
                case (kids, 0) => 
                  new Elem(realE.prefix, realE.label, 
                           realE.attributes, realE.scope, kids :_*)
                case (kids, _) => 
                  new Elem(realE.prefix, realE.label, 
                           noId, realE.scope, kids :_*)
              }
            }
          }
        }

        case Full(AttrSubNode(attr)) => {
          val calced = bind.calculate(realE)
          val filtered = realE.attributes.filter{
            case up: UnprefixedAttribute => up.key != attr
            case _ => true
          }

          val newAttr = if (calced.isEmpty) {
            filtered
          } else {
            val flat: NodeSeq = calced.flatMap(a => a)
            new UnprefixedAttribute(attr, flat, filtered)
          }

          new Elem(realE.prefix, 
                   realE.label, newAttr,
                   realE.scope, SelectorMap.this.apply(realE.child) :_*)
        }
          
        case x: EmptyBox => {
          val calced = bind.calculate(realE)

          calced.length match {
            case 0 => NodeSeq.Empty
            case 1 => {
              calced.first match {
                case Group(g) => g
                case e: Elem => new Elem(e.prefix, 
                                         e.label, mergeAll(e.attributes, false),
                                         e.scope, e.child :_*)
                case x => x
              }
            }
            
            case n => {
              calced.toList.zipWithIndex.flatMap {
                case (Group(g), _) => g
                case (e: Elem, 0) => 
                  new Elem(e.prefix, 
                           e.label, mergeAll(e.attributes, false),
                           e.scope, e.child :_*)
                case (e: Elem, _) =>
                  new Elem(e.prefix, 
                           e.label, mergeAll(e.attributes, true),
                           e.scope, e.child :_*)
                case (x, _) => 
                  x
              }
            }
          }
        }
      }
    }


    def processId(in: Elem): Box[NodeSeq] = 
      for {
        rid <- id
        bind <- idMap.get(rid)
      } yield applyRule(bind, in)

    def processName(in: Elem): Box[NodeSeq] = 
      for {
        rid <- name
        bind <- nameMap.get(rid)
      } yield applyRule(bind, in)

    def findClass(clz: List[String]): Box[CssBind] = clz match {
      case Nil => Empty
      case x :: xs =>
        clzMap.get(x) match {
          case Some(cb) => Full(cb)
          case _ => findClass(xs)
        }
    }

    def processClass(in: Elem): Box[NodeSeq] = 
      findClass(classes) match {
        case Full(bind) => Full(applyRule(bind, in))
        case _ => Empty
      }

    def processAttr(in: Elem): Box[NodeSeq] = 
      if (attrMap.isEmpty || attrs.isEmpty) Empty
    else {
      (for {
        (key, map) <- attrMap
        v <- attrs.get(key)
        cb <- map.get(v)
      } yield applyRule(cb, in)).toSeq.firstOption
    }
  }

  private def slurpAttrs(in: MetaData): SlurpedAttrs = {
    var id: Box[String] = Empty
    var cur: MetaData = in
    var name: Box[String] = Empty
    var clzs: List[String] = Nil
    var theAttrs: Map[String, String] = Map()

    while (cur != Null) {
      cur match {
        case up: UnprefixedAttribute => {
          val key = up.key
          val value = up.value.text
          import Helpers._
          key match {
            case "id" => id = Full(value)
            case "name" => name = Full(value)
            case "class" => clzs = value.charSplit(' ')
            case _ =>
          }

          theAttrs += key -> value
        }

        case pa: PrefixedAttribute => {
          theAttrs += ((pa.pre+":"+pa.key) -> pa.value.text)
        }
        
        case _ =>
      }

      cur = cur.next
    }

    new SlurpedAttrs(id, name) {
      def attrs: Map[String, String] = theAttrs
      def classes: List[String] = clzs
    }
  }
  
  private def treatElem(e: Elem): NodeSeq = {
    val slurp = slurpAttrs(e.attributes)

    (slurp.processId(e) or
    slurp.processName(e) or
    slurp.processClass(e) or
    slurp.processAttr(e)) openOr {
      new Elem(e.prefix, e.label, 
               e.attributes, e.scope, apply(e.child) :_*)
    }
  }

  def apply(in: NodeSeq): NodeSeq = in flatMap {
    case Group(g) => apply(g)
    case e: Elem => treatElem(e)
    case x => x
  }
  
}

object CssBind {
  def unapply(in: CssBind): Option[CssSelector] = in.css
}

sealed trait CssBind extends CssBindFunc {
  def stringSelector: Box[String]
  def css: Box[CssSelector]
  
  def apply(in: NodeSeq): NodeSeq = css match {
    case Full(c) => selectorMap(in)
    case _ => Helpers.errorDiv(
      <div>
      Syntax error in CSS selector definition: {stringSelector openOr "N/A"}.
      The selector will not be applied.
      </div>) openOr NodeSeq.Empty
  }

  private lazy val selectorMap: SelectorMap = new SelectorMap(List(this))

  def calculate(in: NodeSeq): Seq[NodeSeq]
}

/**
 * An abstract implementation of CssBind.  You can instantiate
 * this class and create a custom calculate method
 */
abstract class CssBindImpl(val stringSelector: Box[String], val css: Box[CssSelector]) extends CssBind {
  def calculate(in: NodeSeq): Seq[NodeSeq]
}


}
}
// vim: set ts=2 sw=2 et:
