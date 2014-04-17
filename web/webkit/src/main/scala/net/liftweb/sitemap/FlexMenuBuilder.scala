/*
 * Copyright 2007-2012 WorldWide Conferencing, LLC
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

import net.liftweb.common._
import net.liftweb.http.{LiftRules, S}
import xml.{Elem, Text, NodeSeq}
import net.liftweb.util.Helpers


trait FlexMenuBuilder {
  // a hack to use structural typing to get around the private[http] on Loc.buildItem
  type StructBuildItem = {def buildItem(kids: List[MenuItem], current: Boolean, path: Boolean): Box[MenuItem]}

  /**
   * Override if you want a link to the current page
   */
  def linkToSelf = false

  /**
   * Should all the menu items be expanded?  Defaults to false
   */
  def expandAll = false

  /**
   * Should any of the menu items be expanded?
   */
  protected def expandAny = false

  /**
   * This is used to build a MenuItem for a single Loc
   */
  protected def buildItemMenu[A](loc: Loc[A], currLoc: Box[Loc[_]], expandAll: Boolean): List[MenuItem] = {
    val isInPath = currLoc.map { cur =>
      def isInPath(loc: Loc[_]): Boolean = (cur == loc) || loc.menu.kids.exists(k => isInPath(k.loc))
      isInPath(loc)
    } openOr false

    val kids: List[MenuItem] = if (expandAll) loc.buildKidMenuItems(loc.menu.kids) else Nil
    loc.buildItem(kids, currLoc == Full(loc), isInPath).toList
  }


  /**
   * Compute the MenuItems to be rendered by looking at the 'item' and 'group' attributes
   */
  def toRender: Seq[MenuItem] = {
    val res = (S.attr("item"), S.attr("group")) match {
      case (Full(item), _) =>
        for {
          sm <- LiftRules.siteMap.toList
          req <- S.request.toList
          loc <- sm.findLoc(item).toList
          item <- buildItemMenu(loc, req.location, expandAll)
        } yield item

      case (_, Full(group)) =>
        for {
          sm <- LiftRules.siteMap.toList
          loc <- sm.locForGroup(group)
          req <- S.request.toList
          item <- buildItemMenu(loc, req.location, expandAll)
        } yield item
      case _ => renderWhat(expandAll)
    }
    res

  }

  /**
   * If a group is specified and the group is empty what to display
   */
  protected def emptyGroup: NodeSeq = NodeSeq.Empty

  /**
   * If the whole menu hierarchy is empty, what to display
   */
  protected def emptyMenu: NodeSeq = Text("No Navigation Defined.")

  /**
   * What to display when the placeholder is empty (has no kids)
   */
  protected def emptyPlaceholder: NodeSeq = NodeSeq.Empty

  /**
   * Take the incoming Elem and add any attributes based on
   * path which is true if this Elem is the path to the current page
   */
  protected def updateForPath(nodes: Elem, path: Boolean): Elem = nodes

  /**
   * Take the incoming Elem and add any attributes based on
   * current which is a flag that indicates this is the currently viewed page
   */
  protected def updateForCurrent(nodes: Elem, current: Boolean): Elem = nodes

  /**
   * By default, create an li for a menu item
   */
  protected def buildInnerTag(contents: NodeSeq, path: Boolean, current: Boolean): Elem =
    updateForCurrent(updateForPath(<li>{contents}</li>, path), current)


  /**
   * Render a placeholder
   */
  protected def renderPlaceholder(item: MenuItem, renderInner: Seq[MenuItem] => NodeSeq): Elem = {
    buildInnerTag(<xml:group><span>{item.text}</span>{renderInner(item.kids)}</xml:group>,
      item.path, item.current)
  }

  /**
   * Render a link that's the current link, but the "link to self" flag is set to true
   */
  protected def renderSelfLinked(item: MenuItem, renderInner: Seq[MenuItem] => NodeSeq): Elem =
    buildInnerTag(<xml:group>{renderLink(item.uri, item.text, item.path,
      item.current)}{renderInner(item.kids)}</xml:group>, item.path, item.current)

  /**
   * Render the currently selected menu item, but with no a link back to self
   */
  protected def renderSelfNotLinked(item: MenuItem, renderInner: Seq[MenuItem] => NodeSeq): Elem =
    buildInnerTag(<xml:group>{renderSelf(item)}{renderInner(item.kids)}</xml:group>, item.path, item.current)

  /**
   * Render the currently selected menu item
   */
  protected def renderSelf(item: MenuItem): NodeSeq = <span>{item.text}</span>

  /**
   * Render a generic link
   */
  protected def renderLink(uri: NodeSeq, text: NodeSeq, path: Boolean, current: Boolean): NodeSeq =
    <a href={uri}>{text}</a>

  /**
   * Render an item in the current path
   */
  protected def renderItemInPath(item: MenuItem, renderInner: Seq[MenuItem] => NodeSeq): Elem =
    buildInnerTag(<xml:group>{renderLink(item.uri, item.text, item.path,
      item.current)}{renderInner(item.kids)}</xml:group>, item.path, item.current)

  /**
   * Render a menu item that's neither in the path nor
   */
  protected def renderItem(item: MenuItem, renderInner: Seq[MenuItem] => NodeSeq): Elem =
    buildInnerTag(<xml:group>{renderLink(item.uri, item.text, item.path,
      item.current)}{renderInner(item.kids)}</xml:group>, item.path, item.current)

  /**
   * Render the outer tag for a group of menu items
   */
  protected def renderOuterTag(inner: NodeSeq, top: Boolean): NodeSeq = <ul>{inner}</ul>

  /**
   * The default set of MenuItems to be rendered
   */
  protected def renderWhat(expandAll: Boolean): Seq[MenuItem] =
    (if (expandAll)
      for {
        sm <- LiftRules.siteMap;
        req <- S.request
      } yield sm.buildMenu(req.location).lines
    else S.request.map(_.buildMenu.lines)) openOr Nil

  def render: NodeSeq = {

    val level: Box[Int] = for (lvs <- S.attr("level"); i <- Helpers.asInt(lvs)) yield i

    val toRender: Seq[MenuItem] = this.toRender

    def ifExpandCurrent(f: => NodeSeq): NodeSeq = if (expandAny || expandAll) f else NodeSeq.Empty
    def ifExpandAll(f: => NodeSeq): NodeSeq = if (expandAll) f else NodeSeq.Empty
    toRender.toList match {
      case Nil if S.attr("group").isDefined => emptyGroup
      case Nil => emptyMenu
      case xs =>
        def buildANavItem(i: MenuItem): NodeSeq = {
          i match {
            // Per Loc.PlaceHolder, placeholder implies HideIfNoKids
            case m@MenuItem(text, uri, kids, _, _, _) if m.placeholder_? && kids.isEmpty => emptyPlaceholder
            case m@MenuItem(text, uri, kids, _, _, _) if m.placeholder_? => renderPlaceholder(m, buildLine _)
            case m@MenuItem(text, uri, kids, true, _, _) if linkToSelf   => renderSelfLinked(m, k => ifExpandCurrent(buildLine(k)))
            case m@MenuItem(text, uri, kids, true, _, _) => renderSelfNotLinked(m, k => ifExpandCurrent(buildLine(k)))
            // Not current, but on the path, so we need to expand children to show the current one
            case m@MenuItem(text, uri, kids, _, true, _) => renderItemInPath(m, buildLine _)
            case m =>renderItem(m, buildLine _)
          }
        }

        def buildLine(in: Seq[MenuItem]): NodeSeq = buildUlLine(in, false)

        def buildUlLine(in: Seq[MenuItem], top: Boolean): NodeSeq =
          if (in.isEmpty) {
            NodeSeq.Empty
          } else {
            renderOuterTag(in.flatMap(buildANavItem), top)
          }

        val realMenuItems = level match {
          case Full(lvl) if lvl > 0 =>
            def findKids(cur: Seq[MenuItem], depth: Int): Seq[MenuItem] = if (depth == 0) cur
            else findKids(cur.flatMap(mi => mi.kids), depth - 1)

            findKids(xs, lvl)

          case _ => xs
        }
        buildUlLine(realMenuItems, true)
    }
  }
}
