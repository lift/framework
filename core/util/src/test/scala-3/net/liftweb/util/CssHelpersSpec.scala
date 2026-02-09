/*
 * Copyright 2007-2015 Lift Committers and Contributors
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

import scala.xml._

import org.specs2.mutable.Specification

import common._

class CssHelpersSpec extends Specification {
  import CSSHelpers._

  "CSSParser" should {
    "leave most CSS alone" in {
      val baseCss = 
      """
      #booyan {
        text-indent: 1em;
        -moz-columns: 3;
        -webkit-text-antialiasing: grayscale;
        -magical-fake-thing: booyan;
        superfake: but-still-reasonably-css-y;
      }
      """

      CssUrlPrefixer("prefix").fixCss(baseCss) must beEqualTo(Full(baseCss))
    }
    
    "leave relative CSS urls alone" in {
      val baseCss = 
      """
      #booyan {
        background: url(boom);
        background-image: url('boom?bam,sloop#"shap%20bap');
        image-set: url("http://boom.com/magic?'bam,sloop#bam%21bap")
      }

      .bam {
        background-image: url("boom?bam,sloop#shap%20bap");
      }
      """

      CssUrlPrefixer("prefix").fixCss(baseCss) must beEqualTo(Full(baseCss))
    }

    "prefix root-relative CSS urls with the specified prefix" in {
      val baseCss =
      """
      |#booyan {
      |  background: url(/boom);
      |  background-image: url('/boom?bam,"sloop#shap%20bap');
      |  image-set: url("/boom.com/magic?bam,'sloop#bam%21bap")
      |}""".stripMargin('|')

      CssUrlPrefixer("prefix").fixCss(baseCss).openOrThrowException("test") ===
          """
          |#booyan {
          |  background: url(prefix/boom);
          |  background-image: url('prefix/boom?bam,"sloop#shap%20bap');
          |  image-set: url("prefix/boom.com/magic?bam,'sloop#bam%21bap")
          |}""".stripMargin('|')
    }

    "fail on mismatched quotes or parens and report where it failed" in {
      CssUrlPrefixer("prefix").fixCss("#boom { url('ha) }") must beLike {
        case Failure(message, _, _) =>
          message must contain("'ha")
      }

      CssUrlPrefixer("prefix").fixCss("#boom { url(\"ha) }") must beLike {
        case Failure(message, _, _) =>
          message must contain("\"ha")
      }

      CssUrlPrefixer("prefix").fixCss("#boom { url('ha' }") must beLike {
        case Failure(message, _, _) =>
          message must contain("ha' }")
      }
    }

    // Escaped quotes-in-quotes currently fail. Maybe we want to support these?
  }
}
