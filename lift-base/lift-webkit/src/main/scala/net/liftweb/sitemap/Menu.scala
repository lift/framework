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
import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import Helpers._

/**
 * A common trait between Menu and something that can be converted to a Menu.
 * This makes building Lists of things that can be converted to Menu instance
 * easier because there's a common trait.
 */
trait ConvertableToMenu {
  def toMenu: Menu
}

/**
 * A common trait that defines a portion of a Menu's Link URI path. This allows
 * us to constrain how people construct paths using the DSL by restricting it to
 * Strings or to the <pre>**</pre> object.
 */
sealed trait MenuPath {
  def pathItem: String
}

/**
 * This object may be appended to a Menu DSL path, with the syntax
 * <pre>Menu("Foo") / "test" / **</pre> to match anything starting with
 * a given path. For more info, see Loc.Link.matchHead_?
 *
 * @see Loc.Link
 */
object ** extends MenuPath {def pathItem = "**"}

/**
 * Defines a single path element for a Menu's Link URI. Typically users will
 * not utilize this case class, but will use the WithSlash trait's "/" method
 * that takes Strings.
 */
final case class AMenuPath(pathItem: String) extends MenuPath

/**
 * A DSL for building menus.
 */
object Menu {
  /**
   * A Menu can be created with the syntax <pre>Menu("Home") / "index"</pre>
   * The first parameter is the LinkText which calculates how Links are presented.  The
   * parameter to Menu will be treated as call-by-name such that it is re-evaluated each time
   * the menu link is needed.  That means you can do <pre>Menu(S ? "Home") / "index"</pre>
   * and the menu link will be localized for each display.
   */
  def apply(linkText: Loc.LinkText[Unit]): PreMenu = this.apply(Helpers.randomString(20), linkText)

  /**
   * A Menu can be created with the syntax <pre>Menu("home_page", "Home") / "index"</pre>
   * The first parameter is the name of the page and the second is the LinkText which calculates how Links are presented.
   * The LinkText
   * parameter to Menu will be treated as call-by-name such that it is re-evaluated each time
   * the menu link is needed.  That means you can do <pre>Menu("home_page", S ? "Home") / "index"</pre>
   * and the menu link will be localized for each display.  You can look up a Menu item by
   * name as well as using the &lt;lift:menu.item name="home_page"&gt; snippet.
   */
  def apply(name: String,linkText: Loc.LinkText[Unit]): PreMenu = new PreMenu(name, linkText)

  /**
   * A convenient way to define a Menu items that's got the same name as it does it's localized LinkText.
   * <pre>Menu.i("Home") / "index"</pre> is short-hand for <pre>Menu("Home", S ? "Home") / "index"</pre>
   */
  def i(nameAndLink: String): PreMenu = Menu.apply(nameAndLink, S ? nameAndLink)

  def param[T](name: String, linkText: Loc.LinkText[T], parser: String => Box[T],
               encoder: T => String): PreParamMenu[T] =
    new PreParamMenu[T](name, linkText, parser, encoder)


  /**
   * An intermediate class that holds the basic stuff that's needed to make a Menu item for SiteMap.
   * You must include at least one URI path element by calling the / method
   */
  class PreParamMenu[T](name: String, linkText: Loc.LinkText[T], parser: String => Box[T], encoder: T => String) {
    /**
     * The method to add a path element to the URL representing this menu item
     */
    def /(pathElement: String): ParamMenuable[T] with WithSlash = 
      new ParamMenuable[T](name, linkText, parser, encoder, pathElement :: Nil, false, Nil, Nil) with WithSlash
  }

  class ParamMenuable[T](val name: String,val linkText: Loc.LinkText[T],
                         val parser: String => Box[T],
                         val encoder: T => String,
                         val path: List[String],
                         val headMatch: Boolean,
                         val params: List[Loc.LocParam[T]],val submenus: List[ConvertableToMenu]) extends ConvertableToMenu with BaseMenuable
  {
    type BuiltType = ParamMenuable[T]

    def buildOne(newPath: List[String], newHead: Boolean): BuiltType = new ParamMenuable[T](name, linkText, parser, encoder, newPath, newHead, params, submenus)
    def buildSlashOne(newPath: List[String], newHead: Boolean): BuiltType with WithSlash = new ParamMenuable[T](name, linkText, parser, encoder, newPath, newHead, params, submenus) with WithSlash


    /**
     * Append a LocParam to the Menu item
     */
    def rule(param: Loc.LocParam[T]): ParamMenuable[T] = >>(param)

    /**
     * Append a LocParam to the Menu item
     */
    def >>(param: Loc.LocParam[T]): ParamMenuable[T] =
    new ParamMenuable[T](name, linkText, parser, encoder, path, headMatch, params ::: List(param), submenus)
    
    /**
     * Define the submenus of this menu item
     */
    def submenus(subs: ConvertableToMenu*): ParamMenuable[T] = submenus(subs.toList)

    /**
     * Define the submenus of this menu item
     */
    def submenus(subs: List[ConvertableToMenu]): ParamMenuable[T] =
      new ParamMenuable[T](name, linkText, parser, encoder, path, headMatch, params, submenus ::: subs)

    // FIXME... do the right thing so that in development mode
    // the menu and loc are recalculated when the menu is reloaded

    /**
     * Convert the Menuable into a Menu instance
     */
    lazy val toMenu: Menu = Menu(toLoc, submenus :_*)

    /**
     * Convert the ParamMenuable into a Loc so you can access
     * the well typed currentValue
     */
    lazy val toLoc: Loc[T] = new Loc[T] {
        import scala.xml._

        // the name of the page
        def name = ParamMenuable.this.name
        
        // the default parameters (used for generating the menu listing)
        def defaultValue = Empty

        // no extra parameters
        def params = ParamMenuable.this.params

        /**
         * What's the text of the link?
         */
        def text = ParamMenuable.this.linkText

        val link = new Loc.Link[T](ParamMenuable.this.path, 
                                   ParamMenuable.this.headMatch) {
          override def createLink(in: T) = 
            Full(Text(ParamMenuable.this.path.mkString("/", "/", "/")+
                      urlEncode(ParamMenuable.this.encoder(in))))
        }

        /**
         * Rewrite the request and emit the type-safe parameter
         */
        override val rewrite: LocRewrite =
          Full(NamedPF("Wiki Rewrite") {
            case RewriteRequest(ParsePath(x, _, _,_), _, _) 
            if x.dropRight(1) == ParamMenuable.this.path && 
            x.takeRight(1).headOption.flatMap(a => 
              ParamMenuable.this.parser(a)).isDefined
            =>
              (RewriteResponse(x.dropRight(1)), 
               x.takeRight(1).headOption.flatMap(a => 
                 ParamMenuable.this.parser(a)).get) 
              
          })
    }
  }

  /**
   * The companion object to Menuable that has convenience methods
   */
  object ParamMenuable {
    /**
     * Convert a Menuable into a Menu when you need a Menu.
     */
    implicit def toMenu(able: ParamMenuable[_]): Menu = able.toMenu
  }


  /**
   * An intermediate class that holds the basic stuff that's needed to make a Menu item for SiteMap.
   * You must include at least one URI path element by calling the / method
   */
  class PreMenu(name: String, linkText: Loc.LinkText[Unit]) {
    /**
     * The method to add a path element to the URL representing this menu item
     */
    def /(pathElement: String): Menuable with WithSlash = 
      new Menuable(name, linkText, pathElement :: Nil, false, Nil, Nil) with WithSlash
  }

  trait BaseMenuable {
    type BuiltType

    def path: List[String]
    def headMatch: Boolean

    def buildOne(newPath: List[String], newHead: Boolean): BuiltType
    def buildSlashOne(newPath: List[String], newHead: Boolean): BuiltType with WithSlash
  }

  trait WithSlash {
    self: BaseMenuable =>
      /**
       * The method to add a path element to the URL representing this menu item. This method is
       * typically only used to allow the <pre>**</pre> object mechanism for specifying head match.
       */
      def /(pathElement: MenuPath): BuiltType = pathElement match {
        case ** => buildOne(path, true)
        case _  => buildOne(path ::: List(pathElement.pathItem), headMatch)
      }

      /**
       * The method to add a path element to the URL representing this menu item
       */
      def /(pathElement: String): BuiltType with WithSlash = buildSlashOne(path ::: List(pathElement), headMatch)
  }

  class Menuable(val name: String,val linkText: Loc.LinkText[Unit],
                 val path: List[String],
                 val headMatch: Boolean,
                 val params: List[Loc.LocParam[Unit]],val submenus: List[ConvertableToMenu]) extends ConvertableToMenu with BaseMenuable
  {

    type BuiltType = Menuable

    def buildOne(newPath: List[String], newHead: Boolean): BuiltType = new Menuable(name, linkText, newPath, newHead, params, submenus)
    def buildSlashOne(newPath: List[String], newHead: Boolean): BuiltType with WithSlash = new Menuable(name, linkText, newPath, newHead, params, submenus) with WithSlash


    /**
     * Append a LocParam to the Menu item
     */
    def rule(param: Loc.LocParam[Unit]): Menuable = >>(param)

    /**
     * Append a LocParam to the Menu item
     */
    def >>(param: Loc.LocParam[Unit]): Menuable =
    new Menuable(name, linkText, path, headMatch, params ::: List(param), submenus)
    
    /**
     * Define the submenus of this menu item
     */
    def submenus(subs: ConvertableToMenu*): Menuable = submenus(subs.toList)

    /**
     * Define the submenus of this menu item
     */
    def submenus(subs: List[ConvertableToMenu]): Menuable =
    new Menuable(name, linkText, path, headMatch, params, submenus ::: subs)

    /**
     * Convert the Menuable into a Menu instance
     */
    def toMenu: Menu = Menuable.toMenu(this)
  }

  /**
   * The companion object to Menuable that has convenience methods
   */
  object Menuable {
    /**
     * Convert a Menuable into a Menu when you need a Menu.
     */
    implicit def toMenu(able: Menuable): Menu = Menu(Loc(able.name,
                                                         new Loc.Link(able.path, able.headMatch),
                                                         able.linkText, able.params), able.submenus :_*)
  }
}

case class Menu(loc: Loc[_], private val convertableKids: ConvertableToMenu*) extends HasKids with ConvertableToMenu {
  lazy val kids: Seq[Menu] = convertableKids.map(_.toMenu)
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

  def toMenu = this

  def findLoc(req: Req): Box[Loc[_]] =
  if (loc.doesMatch_?(req)) Full(loc)
  else first(kids)(_.findLoc(req))

  def locForGroup(group: String): Seq[Loc[_]] =
  (if (loc.inGroup_?(group)) List[Loc[_]](loc) else Nil) ++
  kids.flatMap(_.locForGroup(group))


  override def buildUpperLines(pathAt: HasKids, actual: Menu, populate: List[MenuItem]): List[MenuItem]
  = {
    val kids: List[MenuItem] =
    _parent.toList.flatMap(_.kids.toList.flatMap(m => m.loc.buildItem(if (m == this)
                                                                      populate else
                                                                      Nil, m == actual, m == pathAt)))

    _parent.toList.flatMap(p => p.buildUpperLines(p, actual, kids))
  }

  def makeMenuItem(path: List[Loc[_]]): Box[MenuItem] =
    loc.buildItem(kids.toList.flatMap(_.makeMenuItem(path)) ::: loc.supplimentalKidMenuItems, _lastInPath(path), _inPath(path))

  /**
   * Make a menu item only of the current loc is in the given group
   */
  def makeMenuItem(path: List[Loc[_]], group: String): Box[MenuItem] =
    if (loc.inGroup_?(group)) makeMenuItem(path)
    else Empty

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

}
}
