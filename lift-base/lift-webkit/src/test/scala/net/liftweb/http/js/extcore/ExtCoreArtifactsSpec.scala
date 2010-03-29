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
package http {
package js {
package extcore {

import _root_.org.specs._
import _root_.org.specs.runner._
import _root_.org.specs.Sugar._

class ExtCoreArtifactsSpecTest extends Runner(ExtCoreArtifactsSpec) with JUnit with Console
object ExtCoreArtifactsSpec extends Specification {
	
  "ExtCoreArtifacts.toggle" should {
    "return the correct javascript expression" in {
      ExtCoreArtifacts.toggle("id").toJsCmd must_== """Ext.fly("id").toggle()"""
    }
  }
  
  "ExtCoreArtifacts.hide" should {
    "return the correct javascript expression" in {
      ExtCoreArtifacts.hide("id").toJsCmd must_== """Ext.fly("id").hide()"""
    }
  }
  
  "ExtCoreArtifacts.show" should {
    "return the correct javascript expression" in {
      ExtCoreArtifacts.show("id").toJsCmd must_== """Ext.fly("id").show()"""
    }
  }
  
  "ExtCoreArtifacts.showAndFocus" should {
    "return the correct javascript expression" in {
      ExtCoreArtifacts.showAndFocus("id").toJsCmd must_== """Ext.fly("id").show().focus(200)"""
    }
  }
  
  "ExtCoreArtifacts.serialize" should {
    "return the correct javascript expression" in {
      ExtCoreArtifacts.serialize("id").toJsCmd must_== """Ext.Ajax.serializeForm("id")"""
    }
  }
}

}
}
}
}
