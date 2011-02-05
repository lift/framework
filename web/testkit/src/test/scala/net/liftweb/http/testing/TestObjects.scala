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
package testing {

import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.common.{ Box, Full, Empty, Failure}
import _root_.net.liftweb.util.{Helpers}

/*
 * The purpose of these classes is not to run actual tests,
 * but to insure that tests can be run correctly by
 * making sure they compile correctly
 */


object MyCode extends TestKit {
  val baseUrl = ""

  val l2: TestResponse = post("/foo")
  l2.foreach {
    x: HttpResponse =>
      val l3: TestResponse = x.get("ddd")
    println("Hello")
  }

  
  for {
    login <- post("/whatever")
    next <- login.get("/bla")
  } {} 
}

object MyBoxCode extends RequestKit {
  def baseUrl = ""

  val l2: Box[TheResponse] = post("/foo")
  l2.foreach {
    x: TheResponse =>
      val l3: Box[TheResponse] = x.get("ddd")
    println("Hello")
  }

  
  for {
    login: TheResponse <- post("/whatever")
    next <- login.get("/bla")
  } {} 
  
}

}
}
}
