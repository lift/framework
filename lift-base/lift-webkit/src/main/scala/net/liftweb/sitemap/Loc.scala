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
import auth._

import _root_.scala.xml.{NodeSeq, Text}

/**
 * A menu location
 */
trait Loc[T] {
  def name: String

  def link: Loc.Link[T]

  def text: Loc.LinkText[T]

  def overrideValue: Box[T] = Empty

  object requestValue extends RequestVar[Box[T]](Empty) {
    override val __nameSalt = randomString(10)
  }

  /**
   * When the menu item is displayed, what CSS class do we add to the
   * node?
   */
  def cssClassForMenuItem: Box[String] = cacheCssClassForMenuItem.map(_())

  /**
   * By default, this lazy val looks for the MenuCssClass LocParam and
   * uses it.
   */
  protected lazy val cacheCssClassForMenuItem: Box[() => String] = 
    params.flatMap {
      case a: Loc.MenuCssClass => List(a)
      case _ => Nil
    }.headOption.map(_.cssClass.func)

  def defaultValue: Box[T]

  def currentValue: Box[T] = overrideValue or requestValue.is or defaultValue

  def childValues: List[T] = Nil

  def params: List[Loc.LocParam[T]]

  def allParams: List[Loc.LocParam[T]] = params ::: siteMap.globalParams

  private lazy val _placeHolder_? = allParams.contains(Loc.PlaceHolder)

  def placeHolder_? : Boolean = _placeHolder_?

  private lazy val _hideIfNoKids_? = allParams.contains(Loc.HideIfNoKids)

  def hideIfNoKids_? = placeHolder_? || _hideIfNoKids_?

  def siteMap: SiteMap = _menu.siteMap

  def createDefaultLink: Option[NodeSeq] = currentValue.flatMap(p => link.createLink(p)).toOption

  def createLink(in: T): Option[NodeSeq] = link.createLink(in).toOption

  override def toString = "Loc("+name+", "+link+", "+text+", "+params+")"

  type LocRewrite =  Box[PartialFunction[RewriteRequest, (RewriteResponse, T)]]

  def rewrite: LocRewrite = Empty

  def rewritePF: Box[LiftRules.RewritePF] = rewrite.map(
    rw =>
    new NamedPartialFunction[RewriteRequest, RewriteResponse] {
      def functionName = rw match {
        case rw: NamedPartialFunction[_, _] => rw.functionName
        case _ => "Unnamed"
      }

      def isDefinedAt(in: RewriteRequest) = rw.isDefinedAt(in)

      def apply(in: RewriteRequest): RewriteResponse = {
        val (ret, param) = rw.apply(in)
        requestValue.set(Full(param))
        ret
      }
    }
  )

  type SnippetTest = PartialFunction[(String, Box[T]), NodeSeq => NodeSeq]

  def snippets: SnippetTest = Map.empty

  /**
   * Is the Loc marked as Stateless (this will force rendering of
   * the page into stateless mode)
   */
  def stateless_? : Boolean = 
    if (Props.devMode) calcStateless()
    else _frozenStateless

  /**
   * A lazy val used to track statelessness for non-dev mode.
   * By default, it calls calcStateless().
   */
  protected lazy val _frozenStateless = calcStateless()

  /**
   * The method to calculate if this Loc is stateless.  By default
   * looks for the Loc.Stateless Param
   */
  protected def calcStateless() = allParams.find {
    case Loc.Stateless => true
    case _ => false
  }.isDefined

  lazy val calcSnippets: SnippetTest = {
    def buildPF(in: Loc.Snippet): PartialFunction[String, NodeSeq => NodeSeq] = {
      new PartialFunction[String, NodeSeq => NodeSeq] {
        def isDefinedAt(s: String) = s == in.name
        def apply(s: String): NodeSeq => NodeSeq = {
          if (isDefinedAt(s)) in.func
          else throw new MatchError()
        }
      }
    }

    val singles = (
      allParams.flatMap{ case v: Loc.Snippet => Some(v);     case _ => None }.toList.map(buildPF) :::
      allParams.flatMap{ case v: Loc.LocSnippets => Some(v); case _ => None }.toList
    )

    if (singles.isEmpty) Map.empty
    else {
      val func: PartialFunction[String, NodeSeq => NodeSeq] = singles match {
        case pf :: Nil => pf
        case pfs => pfs.reduceLeft[PartialFunction[String, NodeSeq => NodeSeq]](_ orElse _)
      }

      new SnippetTest {
        def isDefinedAt(in: (String, Box[T])): Boolean = func.isDefinedAt(in._1)
        def apply(in: (String, Box[T])): NodeSeq => NodeSeq = func.apply(in._1)
      }
    }
  }

  def snippet(name: String): Box[NodeSeq => NodeSeq] = {
    val test = (name, currentValue)

    if ((snippets orElse calcSnippets).isDefinedAt(test)) Full((snippets orElse calcSnippets)(test))
    else Empty
  }

  protected object accessTestRes extends RequestVar[Either[Boolean, Box[() => LiftResponse]]](_testAccess) {
    override val __nameSalt = randomString(10)
  }

  def testAccess: Either[Boolean, Box[() => LiftResponse]] = accessTestRes.is

  protected def _testAccess: Either[Boolean, Box[() => LiftResponse]] = {
    def testParams(what: List[Loc.LocParam[T]]): Either[Boolean, Box[() => LiftResponse]] = what match {
      case Nil => Left(true)

      case Loc.If(test, msg) :: xs => if (!test()) Right(Full(msg)) else testParams(xs)
      case Loc.IfValue(test, msg) :: xs => if (!test(currentValue)) Right(Full(msg)) else testParams(xs)

      case Loc.Unless(test, msg) :: xs => if (test()) Right(Full(msg)) else testParams(xs)
      case Loc.UnlessValue(test, msg) :: xs => if (test(currentValue)) Right(Full(msg)) else testParams(xs)

      case Loc.TestAccess(func) :: xs =>
        func() match {
          case Full(resp) => Right(Full(() => resp))
          case _ => testParams(xs)
        }

      case Loc.TestValueAccess(func) :: xs =>
        func(currentValue) match {
          case Full(resp) => Right(Full(() => resp))
          case _ => testParams(xs)
        }

      case x :: xs => testParams(xs)
    }

    testParams(allParams) match {
      case Left(true) => _menu.testParentAccess
      case x => x
    }
  }

  def earlyResponse: Box[LiftResponse] = {
    def early(what: List[Loc.LocParam[T]]): Box[LiftResponse] = what match {
      case Nil => Empty

      case Loc.EarlyResponse(func) :: xs =>
        func() match {
          case Full(r) => Full(r)
          case _ => early(xs)
        }

      case x :: xs => early(xs)
    }

    early(allParams)
  }

  /**
   * This method can be overridden to provide a template for this Loc
   */
  def calcTemplate: Box[NodeSeq] = Empty

  /**
   * The first Loc.Template or Loc.ValueTemplate in the param list.
   */
  def paramTemplate: Box[NodeSeq] =
    allParams.flatMap {
      case Loc.Template(f) => Some(f());
      case Loc.ValueTemplate(f) => Some(f(currentValue));
      case _ => None
    }.headOption

  /**
   * The template assocaited with this Loc, if any. Any Loc.Template
   * or Loc.ValueTemplate parameter will take precedence over a value returned
   * by the calcTemplate method.
   */
  def template: Box[NodeSeq] = paramTemplate or calcTemplate

  /**
   * The first Loc.Title in the param list.
   */
  lazy val paramTitle: Box[T => NodeSeq] =
    allParams.flatMap {
      case Loc.Title(f) => Some(f);
      case _ => None
    }.headOption

  /**
   * The title to be displayed for the value associated with this Loc.
   * Any Loc.Title parameter will take precedence over the
   * value returned by the linkText method.
   */
  def title(in: T): NodeSeq = paramTitle.map(_.apply(in)) openOr linkText(in)

  /**
   * The title of the location given the current value associated with this Loc.
   * If no current value is available, this will use the name of this Loc
   * as the title.
   */
  def title: NodeSeq = currentValue.map(title _) openOr Text(name)

  /**
   * The link text to be displayed for a value of type T
   */
  def linkText(in: T): NodeSeq = text.text(in)

  /**
   * The title of the location given the current value associated with this Loc
   */
  def linkText: Box[NodeSeq] = currentValue.map(linkText _)

  private var _menu: Menu = _

  private[sitemap] def menu_=(m: Menu) {
    _menu = m
    m.siteMap.addLoc(this)
  }

  def menu = _menu

  private def testAllParams(params: List[Loc.LocParam[T]], req: Req): Boolean = {
    params.forall {
      case Loc.Test(test) => test(req)
      case _ => true
    }
  }

  def doesMatch_?(req: Req): Boolean = {
    if (link.isDefinedAt(req)) {
      link(req) match {
        case Full(x) if testAllParams(allParams, req) => x
        case Full(x) => false
        case x => x.openOr(false)
      }
    } else false
  }

  def breadCrumbs: List[Loc[_]] = _menu.breadCrumbs ::: List(this)

  def buildKidMenuItems(kids: Seq[Menu]): List[MenuItem] = {
    kids.toList.flatMap(_.loc.buildItem(Nil, false, false)) ::: supplimentalKidMenuItems
  }

  def supplimentalKidMenuItems: List[MenuItem] =
    for (p <- childValues; l <- link.createLink(p))
      yield MenuItem(
        text.text(p),
        l, Nil, false, false,
        allParams.flatMap {
          case v: Loc.LocInfo[_] => List(v())
          case _ => Nil
        }
      )

  def buildMenu: CompleteMenu = {
    CompleteMenu(_menu.buildUpperLines(_menu, _menu, buildKidMenuItems(_menu.kids)))
  }

  private[liftweb] def buildItem(kids: List[MenuItem], current: Boolean, path: Boolean): Box[MenuItem] =
    (calcHidden(kids), testAccess) match {
      case (false, Left(true)) => {
          for {p <- currentValue
               t <- link.createLink(p)}
          yield new MenuItem(
            text.text(p),
            t, kids, current, path,
            allParams.flatMap {
              case v: Loc.LocInfo[_] => List(v())
              case _ => Nil
            },
            placeHolder_?
          )
        }

      case _ => Empty
    }

  protected def calcHidden(kids: List[MenuItem]) = hidden || (hideIfNoKids_? && kids.isEmpty)

  private lazy val _hidden = allParams.contains(Loc.Hidden)

  def hidden = _hidden

  private lazy val groupSet: Set[String] =
    Set(allParams.flatMap{case s: Loc.LocGroup => s.group case _ => Nil} :_*)

  def inGroup_?(group: String): Boolean = groupSet.contains(group)

  def init() {
    params.foreach(_ onCreate(this))
  }

}

trait ConvertableLoc[T] {
  self: Loc[T] =>
    
  /**
   * Converts the String to T that can then be sent to
   * the Loc in createLink
   */
  def convert(str: String): Box[T]
}


/**
 * The Loc companion object, complete with a nice constructor
 */
object Loc {
  type FailMsg = () => LiftResponse

  /**
   * Create a Loc (Location) instance
   *
   * @param name -- the name of the location.  This must be unique across your entire sitemap.
   * It's used to look up a menu item in order to create a link to the menu on a page.
   * @param link -- the Link to the page
   * @param text -- the text to display when the link is displayed
   * @param params -- access test, title calculation, etc.
   */
  def apply(name: String, link: Link[Unit], text: LinkText[Unit], params: LocParam[Unit]*): Loc[Unit] = UnitLoc(name, link, text, params.toList)
  def apply(name: String, link: Link[Unit], text: LinkText[Unit], params: List[LocParam[Unit]]): Loc[Unit] = UnitLoc(name, link, text, params)

  private case class UnitLoc(
    override val name: String,
    override val link: Link[Unit],
    override val text: LinkText[Unit],
    override val params: List[LocParam[Unit]]
  ) extends Loc[Unit] {
    override val defaultValue: Box[Unit] = Full(())

    init()
  }

  case class DataLoc[T](
    override val name: String,
    override val link: Link[T],
    override val text: LinkText[T],
    override val defaultValue: Box[T],
    xparams: LocParam[T]*
  ) extends Loc[T] {
    override val params = xparams.toList

    init()
  }


  /**
   * Algebraic data type for parameters that modify handling of a Loc
   * in a SiteMap
   */
  sealed trait LocParam[-T] {
    def onCreate(loc: Loc[_]){
    }
  }

  /**
   * A type alias for LocParam instances that are applicable to any Loc
   */
  type AnyLocParam = LocParam[Any]

  /**
   * Indicates that the path denominated by Loc requires HTTP authentication
   * and only a user assigned to this role or to a role that is child-of this role
   * can access it.
   */
  case class HttpAuthProtected(role: (Req) => Box[Role]) extends AnyLocParam {

    override def onCreate(loc: Loc[_]) {
      LiftRules.httpAuthProtectedResource.append(
        new LiftRules.HttpAuthProtectedResourcePF() {
          def isDefinedAt(in: Req) = in.path.partPath == loc.link.uriList
          def apply(in: Req): Box[Role] = role(in)
        }
      )
    }
  }

  /**
   * Allows you to generate an early response for the location rather than
   * going through the whole Lift XHTML rendering pipeline
   */
  case class EarlyResponse(func: () => Box[LiftResponse]) extends AnyLocParam

  /**
   * Tests to see if the request actually matches the requirements for access to
   * the page.  For example, if a parameter is missing from the request, this
   * is a good way to restrict access to the page.
   */
  case class Test(test: Req => Boolean) extends AnyLocParam

  /**
   * If the test returns True, the page can be accessed, otherwise,
   * the result of FailMsg will be sent as a response to the browser.
   * If the Loc cannot be accessed, it will not be displayed in menus.
   *
   * @param test -- the function that tests access to the page
   * @param failMsg -- what to return the the browser (e.g., 304, etc.) if
   * the page is accessed.
   */
  case class If(test: () => Boolean, failMsg: FailMsg) extends AnyLocParam
  case class IfValue[T](test: Box[T] => Boolean, failMsg: FailMsg) extends LocParam[T]

  /**
   * MenuCssClass is used to add css to the Menu node.  The css allows for
   * replacing menu with an icon and other super-fun and helpful things.
   * cssClass is a StringFunc which can either be a String constant or
   * a Function that returns a String.  Thus, you can compute the
   * css based on the current state or you can have a constant.  Syntactically
   * you can use either:
   * <pre>
   * MenuCssClass("foobar")
   * MenuCssClass(() => calculateCssForMyMenuItem())
   * </pre>
   */
  case class MenuCssClass(cssClass: StringFunc) extends AnyLocParam

  /**
   * Unless the test returns True, the page can be accessed, otherwise,
   * the result of FailMsg will be sent as a response to the browser.
   * If the Loc cannot be accessed, it will not be displayed in menus.
   *
   * @param test -- the function that tests access to the page
   * @param failMsg -- what to return the the browser (e.g., 304, etc.) if
   * the page is accessed.
   */
  case class Unless(test: () => Boolean, failMsg: FailMsg) extends AnyLocParam
  case class UnlessValue[T](test: Box[T] => Boolean, failMsg: FailMsg) extends LocParam[T]

  /**
   * Allows extra access testing for a given menu location such that
   * you can generically return a response during access control
   * testing
   */
  case class TestAccess(func: () => Box[LiftResponse]) extends AnyLocParam
  case class TestValueAccess[T](func: Box[T] => Box[LiftResponse]) extends LocParam[T]

  /**
   * Allows a user to specify a template based upon a function from the current
   * value encapsulated in the Loc
   */
  case class Template(template: () => NodeSeq) extends AnyLocParam
  case class ValueTemplate[T](template: Box[T] => NodeSeq) extends LocParam[T]

  /**
   * This LocParam may be used to specify a function that calculates a title for the page
   * based upon the current value encapsulated by this Loc.
   */
  case class Title[T](title: T => NodeSeq) extends LocParam[T]

  /**
   * If the Loc is in a group (or groups) like "legal" "community" etc.
   * the groups can be specified and recalled at the top level
   */
  case class LocGroup(group: String*) extends AnyLocParam

  /**
   * An extension point for adding arbitrary lazy values to a Loc.
   */
  trait LocInfo[X] extends AnyLocParam {
    def apply(): Box[() => X]
  }

  /**
   * A single snippet that's assocaited with a given location... the snippet
   * name and the snippet function'
   */
  case class Snippet(name: String, func: NodeSeq => NodeSeq) extends AnyLocParam

  /**
   * Allows you to create a handler for many snippets that are associated with
   * a Loc
   */
  trait LocSnippets extends PartialFunction[String, NodeSeq => NodeSeq] with AnyLocParam

  /**
   * If this parameter is included, the item will not be visible in the menu, but
   * will still be accessable.
   */
  case object Hidden extends AnyLocParam

  /**
   * If the Loc has no children, hide the Loc itself
   */
  case object HideIfNoKids extends AnyLocParam

  /**
   * Is the Loc a stateless Loc... it will be served
   * in stateless mode
   */
  case object Stateless extends AnyLocParam

  /**
   * The Loc does not represent a menu itself, but is the parent menu for
   * children (implies HideIfNoKids)
   */
  case object PlaceHolder extends AnyLocParam

  /**
   * Extension point for user-defined LocParam instances.
   */
  trait UserLocParam[-T] extends LocParam[T]

  /**
   * A subclass of LocSnippets with a built in dispatch method (no need to
   * implement isDefinedAt or apply... just
   * def dispatch: PartialFunction[String, NodeSeq => NodeSeq]
   */
  trait DispatchLocSnippets extends LocSnippets {
    def dispatch: PartialFunction[String, NodeSeq => NodeSeq]

    def isDefinedAt(n: String) = dispatch.isDefinedAt(n)

    def apply(n: String) = dispatch.apply(n)
  }

  /**
   * A function that can be used to calculate the link text from the current
   * value encapsulated by the Loc.
   */
  case class LinkText[-T](text: T => NodeSeq)

  /**
  * The companion object to LinkText that contains some helpful implicit conversion
  */
  object LinkText {
    implicit def nodeSeqToLinkText[T](in: => NodeSeq): LinkText[T] = LinkText[T](T => in)
    implicit def strToLinkText[T](in: => String): LinkText[T] = LinkText(T => Text(in))
  }

  /**
   * This defines the Link to the Loc.
   *
   * @param uriList -- the URL to match
   *
   * @param matchHead_? -- false -- absolute match.  true -- match anything
   * that begins with the same path.  Useful for opening a set of directories
   * (for example, help pages)
   */
  class Link[-T](val uriList: List[String], val matchHead_? : Boolean) extends PartialFunction[Req, Box[Boolean]] {
    def this(b: List[String]) = this(b, false)

    def isDefinedAt(req: Req): Boolean = {
      if (matchHead_?) req.path.partPath.take(uriList.length) == uriList
      else uriList == req.path.partPath
    }

    /**
     * Is the Loc external
     */
    def external_? = false

    def apply(in: Req): Box[Boolean] = {
      if (isDefinedAt(in)) Full(true)
      else throw new MatchError("Failed for Link "+uriList)
    }

    /**
     * Override this method to modify the uriList with data from the Loc's value
     */
    def pathList(value: T): List[String] = uriList

    /**
     * Creates a string representation of the path to the Loc.
     */
    def createPath(value: T): String = {
      val path: List[String] = pathList(value)

      if (matchHead_?) {
        path.mkString("/", "/", "/")
      } else if (SiteMap.rawIndex_? && path == List("index")) {
        "/"
      } else if (path.length > 1 && path.last == "index") {
        path.dropRight(1).mkString("/", "/", "/")
      } else {
        path.mkString("/", "/", "")
      }
    }

    /**
     * Returns the value created by createPath wrapped in a boxed scala.xml.Text element.
     * NOTE: This does not create a clickable HTML link on its own!
     */
    def createLink(value: T): Box[NodeSeq] = Full(Text(createPath(value)))
  }

  object Link {
    def apply(urlLst: List[String], matchHead_? : Boolean, url: String) = {
      new Link[Unit](urlLst, matchHead_?) {
        override def createLink(value: Unit): Box[NodeSeq] = Full(Text(url))
      }
    }

    implicit def strLstToLink(in: Seq[String]): Link[Unit] = new Link[Unit](in.toList)
    implicit def strPairToLink(in: (Seq[String], Boolean)): Link[Unit] = new Link[Unit](in._1.toList, in._2)
  }

  object ExtLink {
    def apply(url: String) = new Link[Unit](Nil, false) {
      override def createLink(value: Unit): Box[NodeSeq] = Full(Text(url))

      /**
       * Is the Loc external
       */
      override def external_? = true
    }
  }

  @deprecated def alwaysTrue(a: Req) = true
  @deprecated def retString(toRet: String)(other: Seq[(String, String)]) = Full(toRet)

  implicit def strToFailMsg(in: => String): FailMsg = () => {
    RedirectWithState(
      LiftRules.siteMapFailRedirectLocation.mkString("/", "/", ""),
      RedirectState(Empty, in -> NoticeType.Error)
    )
  }

  implicit def strFuncToFailMsg(in: () => String): FailMsg = strToFailMsg(in())

  implicit def redirectToFailMsg(in: => RedirectResponse): FailMsg = () => in
}

case class CompleteMenu(lines: Seq[MenuItem]) {
  lazy val breadCrumbs: Seq[MenuItem] = lines.flatMap(_.breadCrumbs)
}

case class MenuItem(text: NodeSeq, uri: NodeSeq,  kids: Seq[MenuItem],
                    current: Boolean,
                    path: Boolean,
                    info: List[Box[() => _]]) {

  private var _placeholder = false
  def placeholder_? = _placeholder

  private var _cssClass: Box[String] = Empty
  def cssClass: Box[String] = _cssClass

  def this(text: NodeSeq, uri: NodeSeq,  kids: Seq[MenuItem],
           current: Boolean,
           path: Boolean,
           info: List[Box[() => _]],
           ph: Boolean) = {
    this(text, uri, kids, current, path, info)
    _placeholder = ph
  }

  def this(text: NodeSeq, uri: NodeSeq,  kids: Seq[MenuItem],
           current: Boolean,
           path: Boolean,
           info: List[Box[() => _]],
           ph: Boolean,
           loc: Loc[_]) = {
    this(text, uri, kids, current, path, info)
    _placeholder = ph
    _cssClass = loc.cssClassForMenuItem
  }

  def breadCrumbs: Seq[MenuItem] = {
    if (!path) Nil
    else this :: kids.toList.flatMap(_.breadCrumbs)
  }
}

}
}
