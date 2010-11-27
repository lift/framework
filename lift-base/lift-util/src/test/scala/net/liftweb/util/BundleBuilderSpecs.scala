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
package util {

import _root_.org.specs._
import _root_.org.specs.runner._
import _root_.scala.xml._
import common._
import _root_.java.util.Locale

object BundleBuilderSpec extends Specification  {
  import BindHelpers._
  
  "BundleBuilder" should {
    "Build a Bundle" in {
      val b = BundleBuilder.convert(<div>
                                    <div name="dog" lang="en">Dog</div>
                                    <div name="dog" lang="fr">Chien</div>
                                    <div name="cat"><div>hi</div></div>
                                    </div>, Locale.US).open_!

      b.getObject("dog") must_== "Dog"
      b.getObject("cat").asInstanceOf[NodeSeq] must ==/ (<div>hi</div>)
    }

    "Build a Bundle must support default" in {
      val b = BundleBuilder.convert(<div>
                                    <div name="dog" lang="zz">Dog</div>
                                    <div name="dog" lang="fr" default="true" >Chien</div>
                                    <div name="cat"><div>hi</div></div>
                                    </div>, Locale.US).open_!

      b.getObject("dog") must_== "Chien"
      b.getObject("cat").asInstanceOf[NodeSeq] must ==/ (<div>hi</div>)
    }

  }
}
class BundleBuilderSpecTest extends JUnit4(BundleBuilderSpec)

}
}
