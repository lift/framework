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
package webapptest {
package snippet {

import net.liftweb.http._
import scala.xml._

class DeferredSnippet {
  object MyNumber extends RequestVar(55)

  def first:NodeSeq = {
    MyNumber.set(44)
    <span id="first">first</span>
  }

  def secondLazy:NodeSeq = {
    val old = MyNumber.is
    MyNumber.set(99)
    <span id="second">Very lazy {old}</span>
  }

  def third:NodeSeq = {
    <span id="third">third {MyNumber.is}</span>
  }

   def stackWhack: NodeSeq = {
     val inActor: Boolean = Thread.currentThread.getStackTrace.
     find(_.getClassName.contains("net.liftweb.actor.")).isDefined

   <span id={"actor_"+inActor}>stackWhack</span>
   }
}

}
}
}
