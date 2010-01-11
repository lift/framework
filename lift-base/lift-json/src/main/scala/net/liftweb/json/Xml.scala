package net.liftweb.json

/*
 * Copyright 2009-2010 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */

/** Functions to convert between JSON and XML.
 */
object Xml {
  import JsonAST._
  import scala.xml._

  /** Convert given XML to JSON.
   * <p>
   * Following rules are used in conversion.
   * <ul>
   *   <li>XML leaf element is converted to JSON string</li>
   *   <li>XML parent element is converted to JSON object and its children to JSON fields</li>
   *   <li>XML elements with same name at same level are converted to JSON array</li>
   *   <li>XML attributes are converted to JSON fields</li>
   * </ul>
   * <p>
   * Example:<pre>
   * scala> val xml =
   *     &lt;users&gt;
   *       &lt;user&gt;
   *         &lt;id&gt;1&lt;/id&gt;
   *         &lt;name&gt;Harry&lt;/name&gt;
   *       &lt;/user&gt;
   *       &lt;user&gt;
   *         &lt;id&gt;2&lt;/id&gt;
   *         &lt;name&gt;David&lt;/name&gt;
   *       &lt;/user&gt;
   *     &lt;/users&gt;   
   *
   * scala> val json = toJson(xml)
   * scala> pretty(render(json))
   * 
   * {
   *   "users":{
   *     "user":[{
   *       "id":"1",
   *       "name":"Harry"
   *     },{
   *       "id":"2",
   *       "name":"David"
   *     }]
   *   }
   * }
   * </pre>
   *
   * Now, the above example has two problems. First, the id is converted to String while
   * we might want it as an Int. This is easy to fix by mapping JString(s) to JInt(s.toInt).
   * The second problem is more subtle. The conversion function decides to use JSON array
   * because there's more than one user-element in XML. Therefore a structurally equivalent
   * XML document which happens to have just one user-element will generate a JSON document
   * without JSON array. This is rarely a desired outcome. These both problems can be fixed
   * by following map function.
   * <pre>
   * json map {
   *   case JField("id", JString(s)) => JField("id", JInt(s.toInt))
   *   case JField("user", x: JObject) => JField("user", JArray(x :: Nil))
   *   case x => x 
   * }
   * </pre>
   */
  def toJson(xml: NodeSeq): JValue = {
    def empty_?(node: Node) = node.child.isEmpty

    /* Checks if given node is leaf element. For instance these are considered leafs:
     * <foo>bar</foo>, <foo>{ doSomething() }</foo>, etc.
     */
    def leaf_?(node: Node) = {
      def descendant(n: Node): List[Node] = n match {
        case g: Group => g.nodes.toList.flatMap(x => x :: descendant(x))
        case _ => n.child.toList.flatMap { x => x :: descendant(x) }
      }

      !descendant(node).find(_.isInstanceOf[Elem]).isDefined
    }

    def array_?(nodeNames: Seq[String]) = nodeNames.size != 1 && nodeNames.toList.removeDuplicates.size == 1
    def directChildren(n: Node) = n.child.filter(c => c.isInstanceOf[Elem])
    def makeObj(name: String, f: => List[JValue]) = JObject(f map {
      case f: JField => f
      case x => JField(name, x)
    })
    def nameOf(n: Node) = (if (n.prefix ne null) n.prefix + ":" else "") + n.label
    def makeField(name: String, value: String) = JField(name, JString(value))
    def buildAttrs(n: Node) = n.attributes.map((a: MetaData) => makeField(a.key, a.value.text)).toList

    def build(root: NodeSeq, rootName: Option[String], argStack: List[JValue]): List[JValue] = root match {
      case n: Node =>
        if (empty_?(n)) JField(nameOf(n), JNull) :: argStack
        else if (leaf_?(n)) makeField(nameOf(n), n.text) :: argStack
        else {
          val obj = makeObj(nameOf(n), buildAttrs(n) ::: build(directChildren(n), Some(nameOf(n)), Nil))
          (rootName match {
            case Some(n) => JField(n, obj)
            case None => obj
          }) :: Nil
        }
      case nodes: NodeSeq => 
        val allLabels = nodes.map(_.label)
        if (array_?(allLabels)) {
          val arr = JArray(nodes.toList.flatMap { n => {
            if (leaf_?(n) && n.attributes == Null) JString(n.text) :: Nil 
            else build(n, None, buildAttrs(n)) }})
          JField(allLabels(0), arr) :: argStack
        } else nodes.toList.flatMap(n => build(n, Some(nameOf(n)), buildAttrs(n)))
    }
    
    (xml map { n => makeObj(nameOf(n), build(n, None, Nil)) }).toList match {
      case List(x) => x
      case x => JArray(x)
    }
  }

  /** Convert given JSON to XML.
   * <p>
   * Following rules are used in conversion.
   * <ul>
   *   <li>JSON primitives are converted to XML leaf elements</li>
   *   <li>JSON objects are converted to XML elements</li>
   *   <li>JSON arrays are recursively converted to XML elements</li>
   * </ul>
   * <p>
   * Use <code>map</code> function to preprocess JSON before conversion to adjust
   * the end result. For instance a common conversion is to encode arrays as comma
   * separated Strings since XML does not have array type.
   * <p><pre>
   * toXml(json map {
   *   case JField("nums",JArray(ns)) => JField("nums",JString(ns.map(_.values).mkString(",")))
   *   case x => x
   * })
   * </pre>
   */
  def toXml(json: JValue): NodeSeq = {
    def toXml(name: String, json: JValue): NodeSeq = json match {
      case JObject(fields) => new XmlNode(name, fields flatMap { f => toXml(f.name, f.value) })
      case JArray(xs) => xs flatMap { v => toXml(name, v) }
      case JField(n, v) => new XmlNode(name, toXml(n, v))
      case JInt(x) => new XmlElem(name, x.toString)
      case JDouble(x) => new XmlElem(name, x.toString)
      case JString(x) => new XmlElem(name, x)
      case JBool(x) => new XmlElem(name, x.toString)
      case JNull => new XmlElem(name, "null")
      case JNothing => Text("")
    }

    json match {
      case JField(n, v) => toXml(n, v)
      case JObject(fields) => fields flatMap { f => toXml(f.name, f.value) }
      case x => toXml("root", x)
    }
  }

  private[json] class XmlNode(name: String, children: Seq[Node]) extends Elem(null, name, xml.Null, TopScope, children :_*)

  private[json] class XmlElem(name: String, value: String) extends Elem(null, name, xml.Null, TopScope, Text(value))
}
