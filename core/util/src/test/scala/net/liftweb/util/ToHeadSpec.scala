/*
 * Copyright 2007-2011 WorldWide Conferencing, LLC
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

import xml.XML._

import org.specs2.matcher.XmlMatchers
import org.specs2.mutable.Specification

import common._
import ControlHelpers._
import HeadHelper._


/**
 * Systems under specification for ToHead.
 */
object ToHeadSpec extends Specification with XmlMatchers {
  "ToHead Specification".title

  "lift <head> merger" should {
    "merge /html/body//head into existing /html/head section" in {
      val susfiles = for {
        act <- tryo(getClass.getResource("ToHeadSpec.actual1.html")).filter(_ ne null)
        exp <- tryo(getClass.getResource("ToHeadSpec.expected1.html")).filter(_ ne null)
      } yield (act, exp)

      susfiles must beLike {
        case Full(sus) =>
          val actual = load(sus._1)
          val expected = load(sus._2)
          mergeToHtmlHead(actual).toString.replaceAll("\\s", "") must_==
          (expected.toString.replaceAll("\\s", ""))
      }
    }

    "merge <head> from real example" in {
      val susfiles = for {
        act <- tryo(getClass.getResource("ToHeadSpec.actual2.html")).filter(_ ne null)
        exp <- tryo(getClass.getResource("ToHeadSpec.expected2.html")).filter(_ ne null)
      } yield (act, exp)

      susfiles must beLike {
        case Full(sus) =>
          val actual = load(sus._1)
          val expected = load(sus._2)
          mergeToHtmlHead(actual) must ==/(expected)
      }
    }

    "merge <lift:tohead> into a new head if not previously exist" in {
      val susfiles = for {
        act <- tryo(getClass.getResource("ToHeadSpec.actual3.html")).filter(_ ne null)
        exp <- tryo(getClass.getResource("ToHeadSpec.expected3.html")).filter(_ ne null)
      } yield (act, exp)

      susfiles must beLike {
        case Full(sus) =>
          val actual = load(sus._1)
          val expected = load(sus._2)
          mergeToHtmlHead(actual).toString.replaceAll("\\s", "") must_==
          (expected.toString.replaceAll("\\s", ""))
      }
    }
   }

  /*
   "lift head cleaner" should {
     "remove duplicate title tag" >> {
       val actual = (<title>hello</title><title>hello2</title><title>hello3</title>)

       val expected = (<title>hello</title>)

       HeadHelper.cleanHead(actual) must beEqualToIgnoringSpace(expected)
     }
     "remove script tag with same id as previous script tag" >> {
       val invariant = (<script type="text/javascript" id="sc1" src="foo1.js"></script><script type="text/javascript" id="sc2" src="foo2.js"></script>)
       HeadHelper.cleanHead(invariant) must beEqualToIgnoringSpace(invariant)

       val actual = (<script type="text/javascript" id="sc1" src="foo1.js"></script><script type="text/javascript" id="sc1" src="foo2.js"></script>)
       val expected = (<script type="text/javascript" id="sc1" src="foo1.js"></script>)
       HeadHelper.cleanHead(actual) must beEqualToIgnoringSpace(expected)
     }
     "remove script tag with src attributes if src attributes are equals to previous script" >> {
       val actual = (<script type="text/javascript" id="sc1" src="foo1.js"></script><script type="text/javascript" src="foo1.js"></script>)
       val expected = (<script type="text/javascript" id="sc1" src="foo1.js"></script>)
       HeadHelper.cleanHead(actual) must beEqualToIgnoringSpace(expected)

       val actual2 = (<script type="text/javascript" id="sc1" src="foo1.js"></script><script type="text/javascript" id="sc2" src="foo1.js"></script>)
       val expected2 = (<script type="text/javascript" id="sc1" src="foo1.js"></script>)
       HeadHelper.cleanHead(actual2) must beEqualToIgnoringSpace(expected2)
     }
     "remove script tag if content are equals to previous script (need to trim each line ?)" >> {
       val actual = (<script type="text/javascript">alert("hello");</script><script type="text/javascript">alert("hello");</script>)
       val expected = (<script type="text/javascript" >alert("hello");</script>)
       HeadHelper.cleanHead(actual) must beEqualToIgnoringSpace(expected)
     }
     "remove link to css with same id as previous link tag" >> {
       val invariant = (<link id="css1" rel="stylesheet" type="text/css" href="style1.css"/><link id="css2" rel="stylesheet" type="text/css" href="style2.css"/>)
       HeadHelper.cleanHead(invariant) must beEqualToIgnoringSpace(invariant)

       val actual = (<link id="css1" rel="stylesheet" type="text/css" href="style1.css"/><link id="css1" rel="stylesheet" type="text/css" href="style2.css"/>)
       val expected = (<link id="css1" rel="stylesheet" type="text/css" href="style1.css"/>)
       HeadHelper.cleanHead(actual) must beEqualToIgnoringSpace(expected)
     }
     "remove link tag with href attributes if href attributes are equals to previous link" >> {
       val invariant = (<link rel="stylesheet" type="text/css" href="style1.css"/><link rel="stylesheet" type="text/css" href="style2.css"/>)
       HeadHelper.cleanHead(invariant) must beEqualToIgnoringSpace(invariant)

       val actual = (<link rel="stylesheet" type="text/css" href="style1.css"/><link rel="stylesheet" type="text/css" href="style1.css"/>)
       val expected = (<link rel="stylesheet" type="text/css" href="style1.css"/>)
       HeadHelper.cleanHead(actual) must beEqualToIgnoringSpace(expected)
     }
     "remove style tag with same id as previous style tag" >> {
       val invariant = (<style id="st1">.foo{{...}}</style><style id="st2">.bar{{...}}</style>)
       HeadHelper.cleanHead(invariant) must beEqualToIgnoringSpace(invariant)

       val actual = (<style id="st1">.foo{{...}}</style><style id="st1">.bar{{...}}</style>)
       val expected = (<style id="st1">.foo{{...}}</style>)
       HeadHelper.cleanHead(actual) must beEqualToIgnoringSpace(expected)
     }
     "remove style tag if content are equals to previous style (need to trim each line ?)" >> {
       val invariant = (<style>.foo{{...}}</style><style>.bar{{...}}</style>)
       HeadHelper.cleanHead(invariant) must beEqualToIgnoringSpace(invariant)

       val actual = (<style>.foo{{...}}</style><style>.foo{{...}}</style>)
       val expected = (<style>.foo{{...}}</style>)
       HeadHelper.cleanHead(actual) must beEqualToIgnoringSpace(expected)
     }
   }
*/
}

