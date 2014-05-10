/*
 * Copyright 2010-2011 WorldWide Conferencing, LLC
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

package net.liftweb
package util

import java.util.Locale

import xml.NodeSeq

import org.specs2.matcher.XmlMatchers
import org.specs2.mutable.Specification


/**
 * Systems under specification for BundleBuilder.
 */
object BundleBuilderSpec extends Specification with XmlMatchers {
  "BundleBuilder Specification".title

  "BundleBuilder" should {
    "Build a Bundle" in {
      val b = BundleBuilder.convert(<div>
                                    <div name="dog" lang="en">Dog</div>
                                    <div name="dog" lang="fr">Chien</div>
                                    <div name="cat"><div>hi</div></div>
                                    </div>, Locale.US).openOrThrowException("Test")

      b.getObject("dog") must_== "Dog"
      b.getObject("cat").asInstanceOf[NodeSeq] must ==/ (<div>hi</div>)
    }

    "Build a Bundle must support default" in {
      val b = BundleBuilder.convert(<div>
                                    <div name="dog" lang="zz">Dog</div>
                                    <div name="dog" lang="fr" default="true" >Chien</div>
                                    <div name="cat"><div>hi</div></div>
                                    </div>, Locale.US).openOrThrowException("Test")

      b.getObject("dog") must_== "Chien"
      b.getObject("cat").asInstanceOf[NodeSeq] must ==/ (<div>hi</div>)
    }

  }
}

