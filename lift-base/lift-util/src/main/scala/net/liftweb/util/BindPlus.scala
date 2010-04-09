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

import scala.xml.{NodeSeq, PrefixedAttribute, MetaData}
import Helpers.{bind, BindParam, FuncBindParam, TheBindParam}
import common._

object BindPlus {
  
  class BindableNodeSeq(ns: NodeSeq) {
    def bind(prefix: String, bindParams: BindParam*) = Helpers.bind(prefix, ns, bindParams: _*)
    def bind(prefix: String, nodeFailureXform: Box[NodeSeq => NodeSeq],
             paramFailureXform: Box[PrefixedAttribute => MetaData], bindParams: BindParam*) =
      Helpers.bind(prefix, nodeFailureXform, paramFailureXform, ns, bindParams: _*)
      
    def bindSwitch(prefix: String, choices: Seq[String])(choice: (Int, NodeSeq=>NodeSeq)) =
      BindPlus.bindSwitch(prefix, ns, choices)(choice)
  }

  /**
   * Can be used to support a bind-chaining syntax, for use with multiple prefixes.
   * For example:
   * <pre>
   *  xhtml.bind("prefix1",
   *    bindParam1,
   *    bindParam2
   *  ).bind("prefix2",
   *    bindParam1,
   *    bindParam2
   *  )</pre>
   * Where bindParam can be the usual arrow -> syntax or any BindParam.
   * Can also be used with the bind overload that takes nodeFailureXform
   * and paramFailureXform arguments, and with bindSwitch.
   * Just import this method, or Util._
   */
  implicit def nodeSeqToBindable(ns: NodeSeq): BindableNodeSeq = new BindableNodeSeq(ns)
  
  
  /**
    Allows one to have parts of the view that are alternatives to each other.
    For example:
    <pre>
      bindSwitch("prefix", xhtml, List("concise","detailed")) {
        if(isInDetailedMode)
          1 -> {(xhtml: NodeSeq) => detailedBind(xhtml) }
        else
          0 -> {(xhtml: NodeSeq) => conciseBind(xhtml) }
      }
    </pre>
    Note that the final parameter is not a function returning a tuple but an actual tuple.
    In this example, curly braces denote an expression that evaluates to the tuple
    which is then passed to bindSwitch.
    You can then include in the view &lt;prefix:concise&gt; and lt;prefix:detailed&gt;
    and only one will be displayed and bound, while the other one will not be outputted.
    Note that this method is also available with the chaining syntax. See nodeSeqToBindable.
    @param prefix The xml prefix of the elements that are alternatives to each other
    @param xml The xml markup containing the alternate view elements
    @param choices A Seq[String] of the labels of the elements that are alternatives to each other
    @param choice A Pair/Tuple2 containing the index in the Seq[String] to use, and a NodeSeq=>NodeSeq to process that element.
  */
  def bindSwitch(prefix: String, xml: NodeSeq, choices: Seq[String])(choice: (Int, NodeSeq=>NodeSeq)) = {
    var index = 0
    bind(prefix, xml,
         choices map {c =>
           index += 1
           choice match {
             case (i, fn) if i == index-1 => FuncBindParam(c, fn)
             case _ => TheBindParam(c, NodeSeq.Empty)
           }
         } : _*
    )
  }
}

}
}
