/*
 * Copyright 2010 WorldWide Conferencing, LLC
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

import scala.xml.{Elem,Node,NodeSeq,XML}

import common.{Box,Empty,Failure,Full}
import util.ControlHelpers.tryo

import _root_.org.specs._
import _root_.org.specs.matcher.{Matcher,Matchers}
import _root_.org.specs.runner._
import _root_.org.specs.Sugar._


//import http.{BadResponse,GetRequest,LiftResponse,LiftRules,XmlResponse}
//import http.rest.XMLApiHelper

class XMLApiSpecTest extends Runner(XMLApiSpec) with JUnit with Console
object XMLApiSpec extends Specification {
  object XMLApiExample extends XMLApiHelper {
    // Define our root tag
    def createTag(contents : NodeSeq) : Elem = <api>{contents}</api>

    // This method exists to test the non-XML implicit conversions on XMLApiHelper
    def produce (in : Any) : LiftResponse = in match {
      // Tests boolToResponse
      case "true" => true
      case "false" => false
      // Tests canBoolToResponse
      case s : String => tryo[Boolean] { s.toInt > 5 }
      // Tests pairToResponse
      case i : Int if i == 42 => (true,"But what is the question?")
      // These test the listElemToResponse conversion
      case f : Float if f == 42f => (<float>perfect</float> : Elem)
      case f : Float if f == 0f => (<float>zero</float> : Node)
      case f : Float if f > 0f => (<float>positive</float> : NodeSeq)
      case f : Float if f < 0f => (<float>negative</float> : Seq[Node])
    }

    // This method tests the XML implicit conversions on XMLApiHelper
    def calculator : LiftRules.DispatchPF = {
      case r @ Req(List("api","sum"), _, GetRequest) => () => doSum(r)
      case r @ Req(List("api","product"), _, GetRequest) => () => doProduct(r)
      case r @ Req(List("api","max"), _, GetRequest) => () => doMax(r)
      case r @ Req(List("api","min"), _, GetRequest) => () => doMin(r)
      // Tests putResponseInBox
      case Req("api" :: _, _, _) => () => BadResponse()
    }

    // ===== Handler methods =====
    def reduceOp (operation : (Int,Int) => Int)(r : Req) : Box[Elem] = tryo {
      (r.param("args").map {
        args =>  <result>{args.split(",").map(_.toInt).reduceLeft(operation)}</result>
       }) ?~ "Missing args"
    } match {
      case Full(x) => x
      case f : Failure => f
      case Empty => Empty
    }

    // We specify the LiftResponse return type to force use of the implicit
    // canNodeToResponse conversion
    def doSum (r : Req) : LiftResponse = reduceOp(_ + _)(r)
    def doProduct (r : Req) : LiftResponse = (reduceOp(_ * _)(r) : Box[Node])
    def doMax (r : Req) : LiftResponse = (reduceOp(_ max _)(r) : Box[NodeSeq])
    def doMin (r : Req) : LiftResponse = (reduceOp(_ min _)(r) : Box[Node])
    //def doMin (r : Req) : LiftResponse = (reduceOp(_ min _)(r) : Box[Seq[Node]])
  }

  // A helper to simplify the specs matching
  case class matchXmlResponse(expected : Node) extends Matcher[LiftResponse] {
    def apply (response : => LiftResponse) = response match {
      case x : XmlResponse => {
        /* For some reason, the UnprefixedAttributes that Lift uses to merge in
         * new attributes makes comparison fail. Instead, we simply stringify and
         * reparse the response contents and that seems to fix the issue. */
        val converted = XML.loadString(x.xml.toString)
        (converted == expected, 
         "%s matches %s".format(converted,expected),
         "%s does not match %s".format(converted, expected))
      }
      case other => (false,"matches","not an XmlResponse")
    }
  }

  "XMLApiHelper" should {
    import XMLApiExample.produce

    /* In all of these tests we include the <xml:group/> since that's what Lift
     * inserts for content in non-content responses.
     */

    "Convert booleans to LiftResponses" in {
      produce("true") must matchXmlResponse(<api success="true"><xml:group/></api>)
      produce("false") must matchXmlResponse(<api success="false"><xml:group/></api>)
    }

    "Convert Boxed booleans to LiftResponses" in {
      produce("42") must matchXmlResponse(<api success="true"><xml:group/></api>)
      produce("1") must matchXmlResponse(<api success="false"><xml:group/></api>)
      
      val failure = produce("invalidInt")

      failure must haveClass[XmlResponse]
      failure match {
        case x : XmlResponse => {
          x.xml.attribute("success").map(_.text) must_== Some("false")
          x.xml.attribute("msg").isDefined must_== true
        }
      }
    }

    "Convert Pairs to responses" in {
      produce(42) must matchXmlResponse(<api success="true" msg="But what is the question?"><xml:group/></api>)
    }

    "Convert various XML types to a response" in {
      produce(0f) must matchXmlResponse(<api success="true"><float>zero</float></api>)
      produce(-1f) must matchXmlResponse(<api success="true"><float>negative</float></api>)
      produce(1f) must matchXmlResponse(<api success="true"><float>positive</float></api>)
      produce(42f) must matchXmlResponse(<api success="true"><float>perfect</float></api>)
    }
  }
}

}

}} // Close packages
