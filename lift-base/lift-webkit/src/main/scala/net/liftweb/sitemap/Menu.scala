/*
 * Copyright 2007-2009 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */
package net.liftweb.sitemap

import _root_.net.liftweb.http._
import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import Helpers._

case class Menu(loc: Loc[_], kids: Menu*) extends HasKids {
  private[sitemap] var _parent: Box[HasKids] = Empty
  private[sitemap] var siteMap: SiteMap = _

  private[sitemap] def init(siteMap: SiteMap) {
    this.siteMap = siteMap
    kids.foreach(_._parent = Full(this))
    kids.foreach(_.init(siteMap))
    loc.menu = this
  }

  private[sitemap] def validate {
    _parent.foreach(p => if (p.isRoot_?) throw new SiteMapException("Menu items with root location (\"/\") cannot have children"))
    kids.foreach(_.validate)
  }

  private[sitemap] def testParentAccess: Either[Boolean, Box[() => LiftResponse]] = _parent match {
    case Full(p) => p.testAccess
    case _ => Left(true)
  }

  override private[sitemap] def testAccess: Either[Boolean, Box[() => LiftResponse]] = loc.testAccess

  def findLoc(req: Req): Box[Loc[_]] =
  if (loc.doesMatch_?(req)) Full(loc)
  else first(kids)(_.findLoc(req))

  def locForGroup(group: String): Seq[Loc[_]] =
  (if (loc.inGroup_?(group)) List[Loc[_]](loc) else Nil) ++
  kids.flatMap(_.locForGroup(group))


  override def buildUpperLines(pathAt: HasKids, actual: Menu, populate: List[MenuItem]): List[MenuItem]
  = {
    val kids: List[MenuItem] =
    _parent.toList.flatMap(_.kids.toList.flatMap(m => m.loc.buildItem(if (m == this) populate else Nil, m == actual, m == pathAt)))

    _parent.toList.flatMap(p => p.buildUpperLines(p, actual, kids))
  }
  // def buildChildLine: List[MenuItem] = kids.toList.flatMap(m => m.loc.buildItem(Nil, false, false))

  def makeMenuItem(path: List[Loc[_]]): Box[MenuItem] =
  loc.buildItem(loc.buildKidMenuItems(kids), _lastInPath(path), _inPath(path))

  private def _inPath(in: List[Loc[_]]): Boolean = in match {
    case Nil => false
    case x :: xs if x eq loc => true
    case x :: xs => _inPath(xs)
  }

  private def _lastInPath(path: List[Loc[_]]): Boolean = path match {
    case Nil => false
    case xs => xs.last eq loc
  }

  def breadCrumbs: List[Loc[_]] = _parent match {
    case Full(m: Menu) => m.loc.breadCrumbs
    case _ => Nil
  }
}

