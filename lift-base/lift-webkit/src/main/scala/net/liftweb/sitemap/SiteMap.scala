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
package sitemap {

import _root_.net.liftweb.http._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import Helpers._

import _root_.scala.xml.{NodeSeq, Text}

class SiteMapException(msg: String) extends Exception(msg)

case class SiteMap(globalParamFuncs: List[PartialFunction[Box[Req], Loc.AnyLocParam]],
                   private val convertablekids: ConvertableToMenu*) extends HasKids  {
  import SiteMap._

  lazy val kids: Seq[Menu] = convertablekids.map(_.toMenu)

  private var locs: Map[String, Loc[_]] = Map.empty

  private var locPath: Set[List[String]] = Set()

  /**
   * Create a new SiteMap by passing the current menu items
   * to a function.  This function can add to, remove, or
   * otherwise mutate the current menu items.
   */
  def rebuild(f: List[Menu] => List[Menu]) = SiteMap(globalParamFuncs, f(kids.toList) :_*)

  kids.foreach(_._parent = Full(this))
  kids.foreach(_.init(this))
  kids.foreach(_.validate)

  private[sitemap] def addLoc(in: Loc[_]) {
    val name = in.name
    if (locs.isDefinedAt(name))
    throw new SiteMapException("Location "+name+" defined twice "+
                               locs(name)+" and "+in)
    else locs = locs + (name -> in.asInstanceOf[Loc[_]])

    if (SiteMap.enforceUniqueLinks && !in.link.external_? &&
	locPath.contains(in.link.uriList))
      throw new SiteMapException("Location "+name+
              " defines a duplicate link "+
              in.link.uriList)

    if (!in.link.external_?) locPath += in.link.uriList
  }

  def globalParams: List[Loc.AnyLocParam] = {
    val r = S.request

    globalParamFuncs.flatMap(f => if (f.isDefinedAt(r)) List(f(r)) else Nil)
  }

  def findLoc(name: String): Box[Loc[_]] = Box(locs.get(name))

  def findLoc(req: Req): Box[Loc[_]] = first(kids)(_.findLoc(req))

  /**
  * Find all the menu items for a given group.
  * This method returns a linear sequence of menu items
  */
  def locForGroup(group: String): Seq[Loc[_]] =
    kids.flatMap(_.locForGroup(group)).filter(
      _.testAccess match {
        case Left(true) => true case _ => false
      })

  /**
   * Find all the menu items for a given group.
   * This method returns menu tree
   */
  def menuForGroup(group: String): CompleteMenu = {
    CompleteMenu(kids.flatMap(_.makeMenuItem(Nil, group)))
  }

  lazy val menus: List[Menu] = locs.values.map(_.menu).toList

  /**
   * Build a menu based on the current location
   */
  def buildMenu(current: Box[Loc[_]]): CompleteMenu = {
    val path: List[Loc[_]] = current match {
      case Full(loc) => loc.breadCrumbs
      case _ => Nil
    }
    CompleteMenu(kids.flatMap(_.makeMenuItem(path)))
  }
}

object SiteMap {
  /**
   * By default, Lift enforced unique links in a SiteMap.  However, you
   * can disable this feature by setting enforceUniqueLinks to false
   */
  @volatile var enforceUniqueLinks = true

  def findLoc(name: String): Box[Loc[_]] = for (sm <- LiftRules.siteMap; loc <- sm.findLoc(name)) yield loc

  /**
   * Builds a function that successively tests the partial function against the Menu.  If the PartialFunction is
   * matched, it is applied and a new Menu is created.  This is generally used by modules to insert their menus
   * at locations in the menu hierarchy denoted by a marker Loc.LocParam.  If the function does not fire,
   * the 'or' function is applied, which allows trying alternative strategies (e.g., if the marker LocParam
   * is not found, append the menus to the root SiteMap.)  This method returns a function
   * so that the strategy can be returned from a module and chained: (module1 andThen module2 andThen module3)(baseSitemap).
   *
   * @param pf the partial function (pattern match) to test against the Menu, if it matches, apply it which causes menu mutation.
   * @param or the function to apply if none of the patterns match
   *
   * @returns a function which will apply the changes to a SiteMap
   */
  def sitemapMutator(pf: PartialFunction[Menu, List[Menu]])(or: SiteMap => SiteMap): SiteMap => SiteMap = 
    (sm: SiteMap) => {
      var fired = false

      def theFunc: Menu => List[Menu] = 
        (menu: Menu) => {
          if (fired) {
            List(menu)
          } else if (pf.isDefinedAt(menu)) {
            fired = true
            pf(menu)
          } else List(menu.rebuild(doAMenuItem _))
        }

        
      def doAMenuItem(in: List[Menu]): List[Menu] = 
        in.flatMap(theFunc)

      val ret = sm.rebuild(_.flatMap(theFunc))

      if (fired) ret else or(sm)
    }

  /**
   * Builds a function that successively tests the partial function against the Menu.  If the PartialFunction is
   * matched, it is applied and a new Menu is created.  This is generally used by modules to insert their menus
   * at locations in the menu hierarchy denoted by a marker Loc.LocParam.  If the function does not fire,
   * a copy of the original sitemap is returned.
   *
   * @param pf the partial function (pattern match) to test against the Menu, if it matches, apply it which causes menu mutation.
   *
   * @returns a function which will apply the changes to a SiteMap
   */
  def simpleSitemapMutator(pf: PartialFunction[Menu, List[Menu]]) = sitemapMutator(pf)(s => s)

  /**
   * Create a mutator that simply appends the menus to the SiteMap at the end of the sitemap.  This is a good
   * default mutator that appends the menu items at the end of the sitemap
   */
  def addMenusAtEndMutator(menus: List[Menu]): SiteMap => SiteMap =
    (sm: SiteMap) => sm.rebuild(_ ::: menus)

  /**
   * <p>
   * In the PartialFunction for sitemapMutator, you may want to look for a particular Loc in the menu to determine
   * if you want to (1) replace it, (2) add your menus after it or (3) insert your menus under it.  You can create
   * a pattern matcher via buildMenuMatcher which returns an instance of UnapplyLocMatcher.
   * </p>
   *
   * <p>
   * For example:<br/>
   * <code class="scala"><pre>
   * val MyMarkerLocParam = new Loc.LocParam[Any]
   * val MyMatcher = SiteMap.buildMenuMatcher(_ == MyMarkerLocParam)
   * </pre></code>
   * </p>
   */
  trait UnapplyLocMatcher {
    def unapply(menu: Menu): Option[Menu]
  }

  /**
   * <p>
   * Builds an UnapplyLocMatcher
   * </p>
   *
   * <p>
   * For example:<br/>
   * <code class="scala"><pre>
   * val MyMarkerLocParam = new Loc.LocParam[Any]
   * val MyMatcher = SiteMap.buildMenuMatcher(_ == MyMarkerLocParam)
   * </pre></code>
   * </p>
   */
  def buildMenuMatcher(matchFunc: Loc.LocParam[_] => Boolean): UnapplyLocMatcher = new UnapplyLocMatcher {
    def unapply(menu: Menu): Option[Menu] =
      menu.loc.params.find(matchFunc).map(ignore => menu)
  }
                       

  def findAndTestLoc(name: String): Box[Loc[_]] =
  findLoc(name).flatMap(l => l.testAccess match {
      case Left(true) => Full(l)
      case _ => Empty
    })

  def buildLink(name: String, text: NodeSeq): NodeSeq = {
    val options = for {
        loc <- findAndTestLoc(name).toList
        link <- loc.createDefaultLink
      } yield {
        val linkText = text match {
          case x if x.text.length > 0 => x
          case _ => loc.linkText openOr Text(loc.name)
        }
        <a href={link}>{linkText}</a>
    }

    options.headOption getOrElse NodeSeq.Empty
  }

  def buildLink(name: String): NodeSeq = buildLink(name, Nil)

  def apply(kids: ConvertableToMenu *) = new SiteMap(Nil, kids :_*)

  /**
   * Should the top level /index path be rendered as /  By default this value is false.
   * You may set it to true, but this may confuse some application servers when the application
   * is not running in the root context.
   */
  @volatile var rawIndex_? = false
}

trait HasKids {
  def kids: Seq[Menu]
  def buildUpperLines(pathAt: HasKids, actual: Menu, populate: List[MenuItem]): List[MenuItem] = populate

  def isRoot_? = false

  private[sitemap] def testAccess: Either[Boolean, Box[() => LiftResponse]] = Left(true)
}

}
}
