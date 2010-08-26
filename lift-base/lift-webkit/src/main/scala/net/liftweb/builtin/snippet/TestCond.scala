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
package builtin {
package snippet {

import _root_.scala.xml._
import _root_.net.liftweb.http._

object TestCond extends DispatchSnippet {
  def dispatch : DispatchIt = {
    case "loggedin" |
    "logged_in" |
    "LoggedIn" | "loggedIn" => loggedIn _

    case "loggedout" |
    "logged_out" |
    "LoggedOut" |
    "loggedOut"
    => loggedOut _
  }

  def loggedIn(xhtml: NodeSeq): NodeSeq =
  if (S.loggedIn_?) xhtml else NodeSeq.Empty

  def loggedOut(xhtml: NodeSeq): NodeSeq =
  if (S.loggedIn_?) NodeSeq.Empty else xhtml
}

}
}
}
