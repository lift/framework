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
package http {
package rest {

import _root_.net.liftweb.common._
import _root_.net.liftweb._
import util._
import Helpers._

import _root_.scala.xml.{NodeSeq, Text, Elem, UnprefixedAttribute, Null, Node}

/**
 * Mix this trait into your REST service provider to convert between different
 * response types and a LiftResponse. You need to define the createTag method
 * to provide a root element for your API. You may optionally override the
 * successAttrName, operationAttrName, and/or msgAttrName defs to control the
 * attributes that will be applied to your root element based on the
 * return from your API.
 *
 * For example, the following code implements a simple API that takes a comma-
 * separated string of integers and reduces them with various operations.
 *
<pre name="code" class="scala">
object CalculatorApi extends XmlApiHelper {
  // Define our root tag
  def createTag(contents : NodeSeq) : Elem = &lt;api>{contents}&lt;/api>

  // The LiftResponses here will be converted to Box[LiftResponse]
  // via the putResponseInBox implicit conversion
  def calculator : LiftRules.DispatchPF = {
    case r @ Req(List("api","sum"), _, GetRequest) => () => doSum(r)
    case r @ Req(List("api","product"), _, GetRequest) => () => doProduct(r)
    case r @ Req(List("api","max"), _, GetRequest) => () => doMax(r)
    case r @ Req(List("api","min"), _, GetRequest) => () => doMin(r)
    case Req("api" :: _, _, _) => () => BadResponse()
  }

  // Define a common handler
  def reduceOp (operation : (Int,Int) => Int)(r : Req) : Box[Elem] = tryo {
    (r.param("args").map {
      args => &lt;result>{args.split(",").map(_.toInt).reduceLeft(operation)}&lt;/result>
     }) ?~ "Missing args"
  } match {
    case Full(x) => x
    case f : Failure => f
    case Empty => Empty
  }

  // Using a return type of LiftResponse causes the canNodeToResponse
  // implicit to be invoked
  def doSum (r : Req) : LiftResponse = reduceOp(_ + _)(r)
  def doProduct (r : Req) : LiftResponse = reduceOp(_ * _)(r)
  def doMax (r : Req) : LiftResponse = reduceOp(_ max _)(r)
  def doMin (r : Req) : LiftResponse = reduceOp(_ min _)(r)
}
</pre>
  *
  * With this API, the URL <pre>http://foo.com/api/sum?args=1,2,3,4,5</pre> would
  * return
<pre name="code" class="xml">
&lt;api operation="sum" success="true">&lt;result>15&lt;/result>&lt;/api>
</pre>
  * 
 */
trait XMLApiHelper {
  /**
   * Converts a boolean into a response of a root element with
   * no contents and the "success" attribute set to the value of
   * the "in" parameter.
   */
  implicit def boolToResponse(in: Boolean): LiftResponse =
    buildResponse(in, Empty, <xml:group/>)

  /**
   * Converts a boxed boolean into a response of a root element with
   * no contents and the "success" attribute set to the value of
   * the "in" parameter. If the Box is a Failure, the "msg" attribute
   * of the root element will be set to the Failure's msg value.
   */
  implicit def canBoolToResponse(in: Box[Boolean]): LiftResponse =
    buildResponse(in openOr false, in match {
      case Failure(msg, _, _) => Full(Text(msg))
      case _ => Empty
    }, <xml:group/>)

  /**
   * Converts a boxed Seq[Node] into a response. If the Box is a Full,
   * the root element uses the contents of the Box as its contents, and
   * sets the "success" attribute to "true". If the Box is a Failure,
   * the "success" attribute is set to "false" and the "msg" attribute
   * is set to the Failure's msg value. If the Box is Empty then the root
   * element is returned with no contents and the "success" attribute set to
   * "false".
   */
  implicit def canNodeToResponse(in: Box[Seq[Node]]): LiftResponse = in match {
    case Full(n) => buildResponse(true, Empty, n)
    case Failure(msg, _, _) => buildResponse(false, Full(Text(msg)), Text(""))
    case _ => buildResponse(false, Empty, Text(""))
  }

  /**
   * Converts a Seq[Node] into a root element with the "success" attribute
   * set to "true" and the Seq[Node] as the contents.
   */
  implicit def listElemToResponse(in: Seq[Node]): LiftResponse =
    buildResponse(true, Empty, in)

  /**
   * Converts a pair of (Boolean,String) into a response of a root
   * element with no contents, the "success" attribute set to
   * the value of the first element of the pair, and the "msg"
   * attribute set to the value of the second element of the pair.
   */
  implicit def pairToResponse(in: (Boolean, String)): LiftResponse =
    buildResponse(in._1, Full(Text(in._2)), <xml:group/>)

  /**
   * Converts a given LiftResponse into a Full[LiftResponse]
   */
  implicit def putResponseInBox(in: LiftResponse): Box[LiftResponse] = Full(in)

  /**
   * Determines the value to place in the "operation" attribute of
   * the root element based on the second element of the request path.
   */
  protected def operation: Option[NodeSeq] =
    (for (req <- S.request) yield req.path.partPath match {
      case _ :: name :: _ => name
      case _ => ""
    }).map(Text)

  /**
   * The method that wraps the outer-most tag around the body. The success,
   * operation and msg attributes will be merged into the returned Elem.
   */
  def createTag(in: NodeSeq): Elem

  /**
   * The name for the success attribute
   */
  def successAttrName = "success"

  /**
   * The name for the operation attribue
   */
  def operationAttrName = "operation"

  /**
   * The name for the msg attribute
   */
  def msgAttrName = "msg"

  /**
   * Build the Response based on Success, an optional message
   * and the body
   */
  protected def buildResponse(success: Boolean, msg: Box[NodeSeq],
                              body: NodeSeq): LiftResponse =
    XmlResponse(createTag(body) % (successAttrName -> success) %
            (new UnprefixedAttribute(operationAttrName, operation, Null)) %
            (new UnprefixedAttribute(msgAttrName, msg, Null)))
}

}
}
}
