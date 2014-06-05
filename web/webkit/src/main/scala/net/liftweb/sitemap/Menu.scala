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
package sitemap 

// FIXME Needed to due to https://issues.scala-lang.org/browse/SI-6541,
// which causes existential types to be inferred for the generated
// unapply of a case class with a wildcard parameterized type.
// Ostensibly should be fixed in 2.12, which means we're a ways away
// from being able to remove this, though.
import scala.language.existentials

import scala.annotation._
import net.liftweb.http._
import net.liftweb.common._
import net.liftweb.util._
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

sealed trait LocPath {
  def pathItem: String
  def wildcard_? : Boolean
}

object LocPath {
  implicit def stringToLocPath(in: String): LocPath =
    new NormalLocPath(in)
}

case object * extends LocPath {
  def pathItem = "star"
  def wildcard_? = true
  override def toString() = "WildcardLocPath()"
}

final case class NormalLocPath(pathItem: String) extends LocPath {
  def wildcard_? = false
}


/**
 * The bridge from the Menu singleton to Java-land
 */
final class MenuJBridge {
  def menu(): MenuSingleton = Menu
}

object Menu extends MenuSingleton {
  /**
   * An intermediate class that holds the basic stuff that's needed to make a Menu item for SiteMap.
   * You must include at least one URI path element by calling the / method
   */
  class PreParamMenu[T<:AnyRef](name: String, linkText: Loc.LinkText[T], parser: String => Box[T], encoder: T => String) {
    /**
     * The method to add a path element to the URL representing this menu item
     */
    def /(pathElement: LocPath): ParamMenuable[T] with WithSlash = 
      new ParamMenuable[T](name, linkText, parser, encoder, pathElement :: Nil, false, Nil, Nil) with WithSlash

    /**
     * The Java way of building menus.  Put the path String here,
     * for example "/foo/bar" or "/foo/ * /bar"
     */
    def path(pathElement: String): ParamMenuable[T] =
      new ParamMenuable[T](name, linkText, parser, encoder, 
                           pathElement.charSplit('/').
                           drop(if (pathElement.startsWith("/")) 1
                                else 0).
                           map(_.trim).
                           filter(_ != "**").
                           map {
                             case "*" => *
                             case "" => NormalLocPath("index")
                             case str => NormalLocPath(str)
                             } match {
                               case Nil => List(NormalLocPath("index"))
                               case xs => xs
                               },
                           pathElement.endsWith("**"), Nil, Nil)
  }

  class ParamMenuable[T](val name: String,val linkText: Loc.LinkText[T],
                         val parser: String => Box[T],
                         val encoder: T => String,
                         val path: List[LocPath],
                         val headMatch: Boolean,
                         val params: List[Loc.LocParam[T]],val submenus: List[ConvertableToMenu]) extends ConvertableToMenu with BaseMenuable
  {
    type BuiltType = ParamMenuable[T]

    def buildOne(newPath: List[LocPath], newHead: Boolean): BuiltType = new ParamMenuable[T](name, linkText, parser, encoder, newPath, newHead, params, submenus)
    def buildSlashOne(newPath: List[LocPath], newHead: Boolean): BuiltType with WithSlash = new ParamMenuable[T](name, linkText, parser, encoder, newPath, newHead, params, submenus) with WithSlash


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
    lazy val toLoc: Loc[T] = new Loc[T] with ParamExtractor[String, T] {
      def headMatch: Boolean = ParamMenuable.this.headMatch
      
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

      def locPath: List[LocPath] = ParamMenuable.this.path
      
      def parser = ParamMenuable.this.parser

      def listToFrom(in: List[String]): Box[String] = in.headOption

      val link = new ParamLocLink[T](ParamMenuable.this.path,
                                     ParamMenuable.this.headMatch,
                                     t => List(encoder(t)))
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

    /**
     * Convert a Menuable into a Loc[T]
     */
    implicit def toLoc[T](able: ParamMenuable[T]): Loc[T] = 
      able.toMenu.loc.asInstanceOf[Loc[T]]

  }

  /**
   * An intermediate class that holds the basic stuff that's needed to make a Menu item for SiteMap.
   * You must include at least one URI path element by calling the / method.
   */
  class PreParamsMenu[T<:AnyRef](name: String, linkText: Loc.LinkText[T],
                         parser: List[String] => Box[T],
                         encoder: T => List[String]) {
    /**
     * The method to add a path element to the URL representing this menu item
     */
    def /(pathElement: LocPath): ParamsMenuable[T] with WithSlash = 
      new ParamsMenuable[T](name, linkText, parser, encoder, 
                            pathElement :: Nil, false, Nil, Nil) with WithSlash

    def path(pathElement: String): ParamsMenuable[T] with WithSlash = 
      new ParamsMenuable[T](name, linkText, parser, encoder, 
                            pathElement :: Nil, false, Nil, Nil) with WithSlash
  }

  class ParamsMenuable[T](val name: String,
                          val linkText: Loc.LinkText[T],
                          val parser: List[String] => Box[T],
                          val encoder: T => List[String],
                          val path: List[LocPath],
                          val headMatch: Boolean,
                          val params: List[Loc.LocParam[T]],
                          val submenus: List[ConvertableToMenu]) extends ConvertableToMenu with BaseMenuable
  {
    type BuiltType = ParamsMenuable[T]

    def buildOne(newPath: List[LocPath], newHead: Boolean): BuiltType = new ParamsMenuable[T](name, linkText, parser, encoder, newPath, newHead, params, submenus)
    def buildSlashOne(newPath: List[LocPath], newHead: Boolean): BuiltType with WithSlash = new ParamsMenuable[T](name, linkText, parser, encoder, newPath, newHead, params, submenus) with WithSlash


    /**
     * Append a LocParam to the Menu item
     */
    def rule(param: Loc.LocParam[T]): ParamsMenuable[T] = >>(param)

    /**
     * Append a LocParam to the Menu item
     */
    def >>(param: Loc.LocParam[T]): ParamsMenuable[T] =
    new ParamsMenuable[T](name, linkText, parser, encoder, path, headMatch, params ::: List(param), submenus)
    
    /**
     * Define the submenus of this menu item
     */
    def submenus(subs: ConvertableToMenu*): ParamsMenuable[T] = submenus(subs.toList)

    /**
     * Define the submenus of this menu item
     */
    def submenus(subs: List[ConvertableToMenu]): ParamsMenuable[T] =
      new ParamsMenuable[T](name, linkText, parser, encoder, path, headMatch, params, submenus ::: subs)

    // FIXME... do the right thing so that in development mode
    // the menu and loc are recalculated when the menu is reloaded

    /**
     * Convert the Menuable into a Menu instance
     */
    lazy val toMenu: Menu = Menu(toLoc, submenus :_*)

    /**
     * Convert the ParamsMenuable into a Loc so you can access
     * the well typed currentValue
     */
    lazy val toLoc: Loc[T] = new Loc[T] with ParamExtractor[List[String], T] {
      // the name of the page
      def name = ParamsMenuable.this.name
     
      def headMatch: Boolean = ParamsMenuable.this.headMatch


      // the default parameters (used for generating the menu listing)
      def defaultValue = Empty

      // no extra parameters
      def params = ParamsMenuable.this.params

      /**
       * What's the text of the link?
       */
      def text = ParamsMenuable.this.linkText

      val link = new ParamLocLink[T](ParamsMenuable.this.path,
                                     ParamsMenuable.this.headMatch,
                                     encoder)
      
      def locPath: List[LocPath] = ParamsMenuable.this.path
      
      def parser = ParamsMenuable.this.parser

      def listToFrom(in: List[String]): Box[List[String]] = Full(in)
      
      val pathLen = ParamsMenuable.this.path.length
    }
  }

  /**
   * The companion object to Menuable that has convenience methods
   */
  object ParamsMenuable {
    /**
     * Convert a Menuable into a Menu when you need a Menu.
     */
    implicit def toMenu(able: ParamsMenuable[_]): Menu = able.toMenu

    /**
     * Convert a Menuable into a Loc[T]
     */
    implicit def toLoc[T](able: ParamsMenuable[T]): Loc[T] = 
      able.toMenu.loc.asInstanceOf[Loc[T]]

  }


  /**
   * An intermediate class that holds the basic stuff that's needed to make a Menu item for SiteMap.
   * You must include at least one URI path element by calling the / method.
   */
  class PreMenu(name: String, linkText: Loc.LinkText[Unit]) {
    /**
     * The method to add a path element to the URL representing this menu item
     */
    def /(pathElement: LocPath): Menuable with WithSlash = 
      new Menuable(name, linkText, pathElement :: Nil, false, Nil, Nil) with WithSlash

    def path(pathElement: String): Menuable with WithSlash = 
      new Menuable(name, linkText, pathElement :: Nil, false, Nil, Nil) with WithSlash
  }

  /**
   * This trait contains helper method that will extract
   * parameters and convert path items based on
   * the locPath
   */
  trait ParamExtractor[ConvertFrom, ConvertTo] {
    self: Loc[ConvertTo] => 
    /**
     * What's the path we're extracting against?
     */
    def locPath: List[LocPath]

    /**
     * A function to convert the ConvertFrom (a String or
     * List[String]) to the target type
     */
    def parser: ConvertFrom => Box[ConvertTo]

    /**
     * Convert the List[String] extracted from the
     * parse params into whatever is necessary to convert
     * to a ConvertTo
     */
    def listToFrom(in: List[String]): Box[ConvertFrom]

    object ExtractSan {
      def unapply(in: List[String]): Option[(List[String], ConvertTo)] 
      = {
        for {
          (path, paramList) <- extractAndConvertPath(in)
          toConvert <- listToFrom(paramList)
          param <- parser(toConvert)
        } yield path -> param
      }
    }

    /**
     * Rewrite the request and emit the type-safe parameter
     */
    override lazy val rewrite: LocRewrite =
      Full(NamedPF(locPath.toString) {
        case RewriteRequest(ParsePath(ExtractSan(path, param),
                                      _, _,_), _, _) => {
          RewriteResponse(path, true) -> param
        }})

    def headMatch: Boolean

    /**
     * Given an incoming request path, match the path and
     * extract the parameters. If the path is matched, return
     * all the extracted parameters.
     * If the path matches, the return Full box will contain
     * the rewritten path and the extracted path parameter
     */
    def extractAndConvertPath(org: List[String]): Box[(List[String], List[String])] = {
      import scala.collection.mutable._
      val retPath = new ListBuffer[String]()
      val retParams = new ListBuffer[String]()
      var gotStar = false

      @tailrec
      def doExtract(op: List[String], mp: List[LocPath]): Boolean =
        (op, mp) match {
          case (Nil, Nil) => true
          case (o :: Nil, Nil) => {
            retParams += o 
            headMatch || !gotStar
          }

          case (op, Nil) => retParams ++= op; headMatch
          case (Nil, _) => false
          case (o :: _, NormalLocPath(str) :: _) if o != str => false
          case (o :: os, * :: ms) => {
            gotStar = true
            retParams += o
            retPath += *.pathItem
            doExtract(os, ms)
          }
          case (o :: os, _ :: ms) => {
            retPath += o
            doExtract(os, ms)
          }
        }

      if (doExtract(org, locPath)) {
        Full((retPath.toList, retParams.toList))
      } else {
        Empty
      }
    }
  }

  trait BaseMenuable {
    type BuiltType

    def path: List[LocPath]
    def headMatch: Boolean

    def buildOne(newPath: List[LocPath], newHead: Boolean): BuiltType
    def buildSlashOne(newPath: List[LocPath], newHead: Boolean): BuiltType with WithSlash
  }

  trait WithSlash {
    self: BaseMenuable =>
      /**
       * The method to add a path element to the URL representing this menu item. This method is
       * typically only used to allow the <pre>**</pre> object mechanism for specifying head match.
       */
      def /(pathElement: MenuPath): BuiltType = pathElement match {
        case ** => buildOne(path, true)
        case AMenuPath(pathItem) => 
          buildOne(path ::: List(NormalLocPath(pathItem)), headMatch)
      }


      def path(pathElement: MenuPath): BuiltType = this./(pathElement)

      /**
       * The method to add a path element to the URL representing this menu item
       */
      def /(pathElement: LocPath): BuiltType with WithSlash = buildSlashOne(path ::: List(pathElement), headMatch)
  }

  class Menuable(val name: String,val linkText: Loc.LinkText[Unit],
                 val path: List[LocPath],
                 val headMatch: Boolean,
                 val params: List[Loc.LocParam[Unit]],val submenus: List[ConvertableToMenu]) extends ConvertableToMenu with BaseMenuable
  {

    type BuiltType = Menuable

    def buildOne(newPath: List[LocPath], newHead: Boolean): BuiltType = new Menuable(name, linkText, newPath, newHead, params, submenus)
    def buildSlashOne(newPath: List[LocPath], newHead: Boolean): BuiltType with WithSlash = new Menuable(name, linkText, newPath, newHead, params, submenus) with WithSlash


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
    implicit def toMenu(able: Menuable): Menu =
      Menu(Loc(able.name,
               new ParamLocLink[Unit](able.path, able.headMatch,
                                      ignore => Nil),
               able.linkText, able.params), able.submenus :_*)
  }
}

/**
 * A DSL for building menus.
 */
sealed trait MenuSingleton {
  import Menu._

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
   * A convenient way to define a Menu item that has the same name as its localized LinkText.
   * <pre>Menu.i("Home") / "index"</pre> is short-hand for <pre>Menu("Home", S.loc("Home", Text("Home")) / "index"</pre>
   */
  def i(nameAndLink: String): PreMenu = Menu.apply(nameAndLink, S.loc(nameAndLink, scala.xml.Text(nameAndLink)))

  def param[T<:AnyRef](name: String, linkText: Loc.LinkText[T], parser: String => Box[T],
               encoder: T => String): PreParamMenu[T] =
    new PreParamMenu[T](name, linkText, parser, encoder)

  def params[T<:AnyRef](name: String, linkText: Loc.LinkText[T],
                parser: List[String] => Box[T],
                encoder: T => List[String]): PreParamsMenu[T] =
    new PreParamsMenu[T](name, linkText, parser, encoder)
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

  /**
   * Rebuild the menu by mutating the child menu items.
   * This mutation can be changing, adding or removing
   */
  def rebuild(f: List[Menu] => List[Menu]): Menu = Menu(loc, f(kids.toList) :_*)

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
    loc.buildItem(kids.toList.flatMap(_.makeMenuItem(path)) ::: loc.supplementalKidMenuItems, _lastInPath(path), _inPath(path))

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

final class ParamLocLink[T](path: List[LocPath], headMatch: Boolean, backToList: T => List[String]) extends Loc.Link[T](path.map(_.pathItem), headMatch) {

  @tailrec
  def test(toTest: List[String], path: List[LocPath]): Boolean = {
    (toTest, path) match {
      case (Nil, Nil) => true
      case (Nil, _) => false
      case (_, Nil) => matchHead_?
      case (str :: _, NormalLocPath(p) :: _) if str != p => false
      case (_ :: ts, * :: ps) => test(ts, ps)
      case (_ :: ts, _ :: ps) => test(ts, ps)
    }
  }
  
  override def isDefinedAt(req: Req): Boolean = {
    test(req.path.partPath, path)
  }

  /**
   * Override this method to modify the uriList with data from the Loc's value
   */
  override def pathList(value: T): List[String] = {
    import scala.collection.mutable.ListBuffer
    val ret = new ListBuffer[String]()
    
    @tailrec
    def merge(path: List[LocPath], params: List[String]) {
      (path, params) match {
        case (Nil, p) => ret ++= p
        case (* :: ps, Nil) => ret += "?" ; merge(ps, Nil)
        case (* :: ps, r :: rs) => ret += r ; merge(ps, rs)
        case (NormalLocPath(p) :: ps, rs) =>
          ret += p ; merge(ps, rs)
      }
    }

    merge(path, backToList(value))

    ret.toList
  }
}
