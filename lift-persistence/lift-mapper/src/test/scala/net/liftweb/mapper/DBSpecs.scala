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
package mapper {

import _root_.org.specs._
import _root_.org.specs.runner.JUnit3
import _root_.org.specs.runner.ConsoleRunner
import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import _root_.net.liftweb.http.{S,LiftSession,LiftRules}

import Helpers._

class DBSpecsAsTest extends JUnit3(DBSpecs)
object DBSpecsRunner extends ConsoleRunner(DBSpecs)

object DBSpecs extends Specification {
  val provider = DBProviders.H2MemoryProvider
  val logF = Schemifier.infoF _
  
  def cleanup() {
    provider.setupDB
    Schemifier.destroyTables_!!(DefaultConnectionIdentifier, logF ,  User)
    Schemifier.schemify(true, logF, DefaultConnectionIdentifier, User)
  }
 
  "DB" should {
    "collect queries when queryCollector is added as logFunc" in {
      cleanup()
      DB.addLogFunc(DB.queryCollector)
      
      var statements: List[(String, Long)] = Nil
                           
      S.addAnalyzer((r,t,ss) => statements=ss)
      
      val session = new LiftSession("hello", "", Empty)
      val elwood = S.initIfUninitted(session) {
        val r = User.find(By(User.firstName, "Elwood")).open_!
        S.queryLog.size must_== 1
        r
      }
      statements.size must_==1
      elwood.firstName.is must_== "Elwood"
    }
  }
}

}
}