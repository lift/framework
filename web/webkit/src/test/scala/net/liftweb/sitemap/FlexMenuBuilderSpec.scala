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

package net.liftweb.sitemap

import net.liftweb.http.{S, LiftRules}
import net.liftweb.common.{Full, Empty}
import net.liftweb.mockweb.WebSpec
import xml.{Elem, Group, NodeSeq}

object FlexMenuBuilderSpec extends WebSpec(FlexMenuBuilderSpecBoot.boot _) {
  "FlexMenuBuilder Specification".title

  val html1 = <div data-lift="MenuBuilder.builder?group=hometabsv2"></div>

  "FlexMenuBuilder" should {
    val testUrl = "http://foo.com/help"
    val testUrlPath = "http://foo.com/index1"

    "Link to Self" withSFor(testUrl) in {
      object MenuBuilder extends FlexMenuBuilder { override def linkToSelf = true}
      val linkToSelf = <ul><li><a href="/index">Home</a></li><li><a href="/help">Help</a></li><li><a href="/help2">Help2</a></li></ul>
      val actual = MenuBuilder.render
      linkToSelf must beEqualToIgnoringSpace(actual)
    }
    "expandAll" withSFor(testUrl) in {
      object MenuBuilder extends FlexMenuBuilder { override def expandAll = true}
      val expandAll: NodeSeq = <ul><li><a href="/index">Home</a></li><li><span>Help</span><ul><li><a href="/index1">Home1</a></li><li><a href="/index2">Home2</a></li></ul></li><li><a href="/help2">Help2</a><ul><li><a href="/index3">Home3</a></li><li><a href="/index4">Home4</a></li></ul></li></ul>
      val actual = MenuBuilder.render
      expandAll.toString must_== actual.toString
    }
    "Add css class to item in the path" withSFor(testUrlPath) in {
      object MenuBuilder extends FlexMenuBuilder {
        override def updateForPath(nodes: Elem, path: Boolean): Elem = {
          if (path){
            nodes % S.mapToAttrs(Map("class" -> "active"))
          } else{
            nodes
          }
        }
      }
      val itemInPath: NodeSeq = <ul><li><a href="/index">Home</a></li><li class="active"><a href="/help">Help</a><ul><li class="active"><span>Home1</span></li><li><a href="/index2">Home2</a></li></ul></li><li><a href="/help2">Help2</a></li></ul>
      val actual = MenuBuilder.render
      itemInPath.toString must_== actual.toString
    }
    "Add css class to the current item" withSFor(testUrl) in {
      object MenuBuilder extends FlexMenuBuilder {
        override def updateForCurrent(nodes: Elem, current: Boolean): Elem = {
          if (current){
            nodes % S.mapToAttrs(Map("class" -> "active"))
          } else{
            nodes
          }
        }
      }
      val itemInPath: NodeSeq = <ul><li><a href="/index">Home</a></li><li class="active"><span>Help</span></li><li><a href="/help2">Help2</a></li></ul>
      val actual = MenuBuilder.render
      itemInPath.toString must_== actual.toString
    }
  }

}

/**
 * This only exists to keep the WebSpecSpec clean. Normally,
 * you could just use "() => bootstrap.Boot.boot".
 */
object FlexMenuBuilderSpecBoot {
  def boot() {
    def siteMap = SiteMap(
      Menu.i("Home") / "index",
      Menu.i("Help") / "help" submenus (
        Menu.i("Home1") / "index1",
        Menu.i("Home2") / "index2"

      ),
      Menu.i("Help2") / "help2" submenus (
        Menu.i("Home3") / "index3",
        Menu.i("Home4") / "index4"
      )
    )
    LiftRules.setSiteMap(siteMap)
  }
}

