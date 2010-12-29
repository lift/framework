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
package json {

import org.specs.Specification
import org.specs.runner.{Runner, JUnit}

class SerializationBugsTest extends Runner(SerializationBugs) with JUnit
object SerializationBugs extends Specification {
  import Serialization.{read, write => swrite}

  implicit val formats = Serialization.formats(NoTypeHints)

  "plan1.Plan can be serialized (issue 341)" in {
    import plan1._

    val game = Game(Map("a" -> Plan(Some(Action(1, None))))) 
    val ser = swrite(game)
    read[Game](ser) mustEqual game
  }

  "plan2.Plan can be serialized (issue 341)" in {
    import plan2._

    val g1 = Game(Map("a" -> Plan(Some(Action("f1", "s", Array(), None)), 
                                  Some("A"), 
                                  Some(Action("f2", "s2", Array(0, 1, 2), None)))))
    val ser = swrite(g1)
    val g2 = read[Game](ser)
    val plan = g2.buy("a")
    g2.buy.size mustEqual 1
    val leftOp = plan.leftOperand.get
    leftOp.functionName mustEqual "f1"
    leftOp.symbol mustEqual "s"
    leftOp.inParams.toList mustEqual Nil
    leftOp.subOperand mustEqual None
    plan.operator mustEqual Some("A")
    val rightOp = plan.rightOperand.get
    rightOp.functionName mustEqual "f2"
    rightOp.symbol mustEqual "s2"
    rightOp.inParams.toList mustEqual List(0, 1, 2)
    rightOp.subOperand mustEqual None
  }

  "null serialization bug" in {
    val x = new X(null) 
    val ser = swrite(x)
    read[X](ser) mustEqual x
  }

  "StackOverflowError with large Lists" in {
    val xs = LongList(List.fill(5000)(0).map(Num))
    val ser = swrite(xs)
    read[LongList](ser).xs.length mustEqual 5000
  }
}

case class LongList(xs: List[Num])
case class Num(x: Int)

case class X(yy: Y)
case class Y(ss: String)

package plan1 {
  case class Plan(plan: Option[Action])
  case class Game(game: Map[String, Plan])
  case class Action(id: Int, subAction: Option[Action])
}

package plan2 {
  case class Plan(leftOperand: Option[Action], operator: Option[String], 
                  rightOperand: Option[Action])
  case class Game(buy: Map[String, Plan])
  case class Action(functionName: String, symbol: String,
                    inParams: Array[Number], subOperand: Option[Action]) 
}

}
}
