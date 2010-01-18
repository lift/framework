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

package net.liftweb.http.rest

import _root_.net.liftweb.common._
import _root_.net.liftweb._
import util._
import Helpers._

import _root_.scala.xml.{NodeSeq, Text, Elem, UnprefixedAttribute, Null, Node}

/**
 * Mix this trait into your REST service provider to convert between different
 * response and a LiftResponse
 */
trait XMLApiHelper {
  implicit def boolToResponse(in: Boolean): LiftResponse =
    buildResponse(in, Empty, <xml:group/>)

  implicit def canBoolToResponse(in: Box[Boolean]): LiftResponse =
    buildResponse(in openOr false, in match {
      case Failure(msg, _, _) => Full(Text(msg))
      case _ => Empty
    }, <xml:group/>)

  implicit def pairToResponse(in: (Boolean, String)): LiftResponse =
    buildResponse(in._1, Full(Text(in._2)), <xml:group/>)

  protected def operation: Option[NodeSeq] =
    (for (req <- S.request) yield req.path.partPath match {
      case _ :: name :: _ => name
      case _ => ""
    }).map(Text)

  implicit def nodeSeqToResponse(in: NodeSeq): LiftResponse =
    buildResponse(true, Empty, in)

  implicit def listElemToResponse(in: Seq[Node]): LiftResponse =
    buildResponse(true, Empty, in)

  implicit def canNodeToResponse(in: Box[NodeSeq]): LiftResponse = in match {
    case Full(n) => buildResponse(true, Empty, n)
    case Failure(msg, _, _) => buildResponse(false, Full(Text(msg)), Text(""))
    case _ => buildResponse(false, Empty, Text(""))
  }

  implicit def putResponseInBox(in: LiftResponse): Box[LiftResponse] = Full(in)

  /**
   * The method that wraps the outer-most tag around the body
   */
  def createTag(in: NodeSeq): Elem

  /**
   * The attribute name for success
   */
  def successAttrName = "success"

  /**
   * The attribute name for operation
   */
  def operationAttrName = "operation"

  /**
   * The attribute name for any msg attribute
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
