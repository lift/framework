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

package bootstrap.liftweb

import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import Helpers._

/**
  * A class that's instantiated early and run.  It allows the application
  * to modify lift's environment
  */
class Boot {
  def boot {
    // where to search snippet
    LiftRules.addToPackages("net.liftweb.webapptest")

    LiftRules.dispatch.append(ContainerVarTests)
  }
}

import rest._

case class Moose(str: String)

object ContainerVarTests extends RestHelper {
  object StrVar extends ContainerVar("Hello")
  object IntVar extends ContainerVar(45)
  // object CaseVar extends ContainerVar(Moose("dog"))

  serve {
    case "cv_int" :: Nil Get _ => <int>{IntVar.is}</int>
    case "cv_int" :: AsInt(i) :: _ Get _ => {
      IntVar.set(i)
      <int>{IntVar.is}</int>
    }
  }

  serve {
    case "cv_str" :: Nil Get _ => <str>{StrVar.is}</str>
    case "cv_str" :: str :: _ Get _ => {
      StrVar.set(str)
      <str>{StrVar.is}</str>
    }
  }
}
