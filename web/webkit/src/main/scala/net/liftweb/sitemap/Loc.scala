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

import net.liftweb.http._
import net.liftweb.common._
import net.liftweb.util._
import Helpers._
import auth._

import scala.xml.{NodeSeq, Text}

/**
 * A menu location
 */
trait Loc[T] {
  def name: String

  def link: Loc.Link[T]

  def text: Loc.LinkText[T]

  def overrideValue: Box[T] = Empty

  object requestValue extends RequestVar[Box[T]](defaultRequestValue) {
    override val __nameSalt = randomString(10)
  }

  /**
   * Override this if the default request value should
   * be something other than Empty
   */
  protected def defaultRequestValue: Box[T] = Empty

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
    allParams.flatMap {
      case a: Loc.MenuCssClass => List(a)
      case _ => Nil
    }.headOption.map(_.cssClass.func)

  /**
   * Given a value calculate the HREF to this item
   */
  def calcHref(in: T): String = appendQueryParameters(link.createPath(in),
                                                      Full(in))


  /**
   * Calculate HREF to this item using currentValue
   */
  def calcDefaultHref: String =  currentValue.map(p => link.createPath(p)).toOption.
    map(path => appendQueryParameters(path, currentValue)).getOrElse("")


  def defaultValue: Box[T]
  
  /**
  * The value of the Loc based on params (either Loc.Value or Loc.CalcValue)
  */
  def paramValue: Box[T] = calcValue.flatMap(f => f()) or staticValue

  private lazy val staticValue: Box[T] = {
    allParams.collectFirst {
      case Loc.Value(v) =>
        v.asInstanceOf[T]
    }
  }
  
  private lazy val calcValue: Box[() => Box[T]] = {
    params.collectFirst {
      case Loc.CalcValue(f: Function0[_]) =>
        f.asInstanceOf[()=>Box[T]]
    }
  }

  /**
   * Calculate the Query parameters
   */
  def queryParameters(what: Box[T]): List[(String, String)] =
    addlQueryParams.flatMap(_()) ::: 
  calcQueryParams.flatMap(_(what))

  protected def appendQueryParams(what: T)(nodeSeq: NodeSeq): NodeSeq = 
    Text(appendQueryParameters(nodeSeq.text, Full(what)))

  protected def appendQueryParameters(in: String, what: Box[T]) = 
    Helpers.appendQueryParameters(in, queryParameters(what))

  private lazy val addlQueryParams: List[() => List[(String, String)]] =
    params.collect{case lp: Loc.QueryParameters => lp.f}


  private lazy val calcQueryParams: List[Box[T] => List[(String, String)]] =
    params.collect{case lp: Loc.LocQueryParameters[T] => lp.f}

  /**
  * The current value of the cell: overrideValue or requestValue.is or defaultValue oe paramValue
  */
  def currentValue: Box[T] = overrideValue or requestValue.is or defaultValue or paramValue

  def childValues: List[T] = Nil

  def params: List[Loc.LocParam[T]]

  def allParams: List[Loc.AnyLocParam] = 
    (params.asInstanceOf[List[Loc.AnyLocParam]]) ::: 
     parentParams :::
     siteMap.globalParams

  private def parentParams: List[Loc.AnyLocParam] = 
    _menu match {
      case null => Nil
      case menu => menu._parent match {
        case Full(parentMenu: Menu) => 
          if (!params.collect{case i: Loc.UseParentParams => true}.isEmpty) {
            parentMenu.loc.allParams.asInstanceOf[List[Loc.LocParam[Any]]]
          } else {
            Nil
          }
        case _ => Nil
      }
    }

  private lazy val _placeHolder_? = allParams.contains(Loc.PlaceHolder)

  def placeHolder_? : Boolean = _placeHolder_?

  private lazy val _hideIfNoKids_? = allParams.contains(Loc.HideIfNoKids)

  def hideIfNoKids_? = placeHolder_? || _hideIfNoKids_?

  def siteMap: SiteMap = _menu.siteMap

  def createDefaultLink: Option[NodeSeq] = 
    currentValue.flatMap(p => link.createLink(p)).toOption.
    map(ns => Text(appendQueryParameters(ns.text, currentValue)))


  def createLink(in: T): Option[NodeSeq] = link.createLink(in).toOption.
  map(ns => Text(appendQueryParameters(ns.text, Full(in))))

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

  /**
   * A `PartialFunction` that maps a snippet name, and an optional `Loc` value, in a `Tuple2`,
   * to a snippet function (`NodeSeq` => `NodeSeq`).
   */
  type SnippetTest = PartialFunction[(String, Box[T]), NodeSeq => NodeSeq]

  /**
   * The snippets defined by the `Loc` class itself, as opposed to those
   * provided by its `LocParams`.
   */
  def snippets: SnippetTest = Map.empty

  /**
   * Is the Loc marked as Stateless (this will force rendering of
   * the page into stateless mode)
   */
  def stateless_? : Boolean = 
    if (Props.devMode) (calcStateless() || reqCalcStateless())
    else (_frozenStateless || reqCalcStateless())

  /**
   * A lazy val used to track statelessness for non-dev mode.
   * By default, it calls calcStateless().
   */
  protected lazy val _frozenStateless = calcStateless()

  /**
   * The method to calculate if this Loc is stateless.  By default
   * looks for the Loc.Stateless Param
   */
  protected def calcStateless(): Boolean = allParams.find {
    case Loc.Stateless => true
    case _ => false
  }.isDefined

  /**
   * Find the stateless calculation Loc params
   */
  protected def findStatelessCalc: (Box[Loc.CalcStateless], Box[Loc.CalcParamStateless[T]]) = (allParams.collect {
    case v @ Loc.CalcStateless(_) => v
  }.headOption,
    allParams.collect {
    case v @ Loc.CalcParamStateless(_) => v
  }.headOption)

  /**
   * The cached Loc params
   */
  protected lazy val _foundStatelessCalc: (Box[Loc.CalcStateless], Box[Loc.CalcParamStateless[T]]) = findStatelessCalc

  protected def foundStatelessCalc: (Box[Loc.CalcStateless], Box[Loc.CalcParamStateless[T]])  =
  if (Props.devMode) findStatelessCalc else _foundStatelessCalc

  /**
   * run the stateless calculation
   */
  protected def reqCalcStateless(): Boolean = {
    val (np, param) = foundStatelessCalc
    (np.map(_.f()) or param.map(_.f(currentValue))) openOr false
  }

  /**
   * The snippets provided by `LocParam`s
   */
  lazy val calcSnippets: SnippetTest =
    allParams
      .collect { case v: Loc.ValueSnippets[T] => v.snippets }
      .reduceLeftOption(_ orElse _)
      .getOrElse(Map.empty)

  /**
   * Look up a snippet by name, taking into account the current
   * `Loc` value.
   */
  def snippet(name: String): Box[NodeSeq => NodeSeq] = {
    snippets orElse calcSnippets lift (name, currentValue)
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
      case Loc.TemplateBox(f) => f()
      case Loc.ValueTemplateBox(f) => f(currentValue)
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
    link.isDefinedAt(req) &&
    testAllParams(allParams, req) &&
    (
      currentValue.isDefined ||
      params.contains(Loc.MatchWithoutCurrentValue)
    )
  }

  def breadCrumbs: List[Loc[_]] = _menu.breadCrumbs ::: List(this)

  def buildKidMenuItems(kids: Seq[Menu]): List[MenuItem] = {
    kids.toList.flatMap(_.loc.buildItem(Nil, false, false)) ::: supplementalKidMenuItems
  }

  def supplementalKidMenuItems: List[MenuItem] =
    for {
      p <- childValues
      l <- link.createLink(p).map(appendQueryParams(p))
    } yield MenuItem(
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
          for {
            p <- currentValue
            t <- link.createLink(p).map(appendQueryParams(p))
          } yield new MenuItem(
            text.text(p),
            t, kids, current, path,
            allParams.flatMap {
              case v: Loc.LocInfo[_] => List(v())
              case _ => Nil
            },
            placeHolder_?, this
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

  private final case class UnitLoc(
    override val name: String,
    override val link: Link[Unit],
    override val text: LinkText[Unit],
    override val params: List[LocParam[Unit]]
  ) extends Loc[Unit] {
    override def defaultValue: Box[Unit] = Full(())

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
  trait LocParam[-T] {
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
   * Allows a user to specify a template based upon a function from the current
   * value encapsulated in the Loc.  Allow the return of Box[NodeSeq] so that it's more
     * friendly to Templates.
   */
  case class TemplateBox(template: () => Box[NodeSeq]) extends AnyLocParam
  case class ValueTemplateBox[T](template: Box[T] => Box[NodeSeq]) extends LocParam[T]

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
  * Calculate the value for the Loc.  This is useful for parameterized
  * menus.  It allows you to calculate the value of the Loc.
  */
  case class CalcValue[T](func: () => Box[T]) extends LocParam[T]
  
  /**
  * The value of Loc
  */
  case class Value[T](value: T) extends LocParam[T]

  /**
   * An extension point for adding arbitrary lazy values to a Loc.
   */
  trait LocInfo[X] extends AnyLocParam {
    def apply(): Box[() => X]
  }

  /**
   * The common interface for `LocParam`s that provide snippet functions,
   * which can be aware of the `Loc` value.
   * @tparam A The type with which the `Loc` is parameterized.
   */
  trait ValueSnippets[A] extends LocParam[A] {
    /**
     * Provides snippets for the `Loc`.
     * @return a `PartialFunction` that maps a snippet name, and an optional `Loc` value, in a `Tuple2`,
     * to a snippet function (`NodeSeq` => `NodeSeq`).
     */
    def snippets: PartialFunction[(String, Box[A]), NodeSeq => NodeSeq]
  }
  object ValueSnippets {
    /**
     * Factory for general `ValueSnippets` instances by wrapping an existing `PartialFunction`.
     * @tparam A the type of the `Loc`'s value
     * @param pf a `PartialFunction` that maps a snippet name, and an optional `Loc` value, in a `Tuple2`,
     * to a snippet function (`NodeSeq` => `NodeSeq`).
     */
    def apply[A](pf: PartialFunction[(String, Box[A]), NodeSeq => NodeSeq]): ValueSnippets[A] = new ValueSnippets[A] {
      def snippets = pf
    }
  }

  /**
   * A single snippet that's associated with a `Loc`, but is
   * not directly aware of the `Loc` value.
   * @see ValueSnippets
   * @param name the snippet name
   * @param _func the snippet function. Note that this is a call-by-name parameter;
   * that is, the function expression passed in will be evaluated each time
   * the function is invoked.
   */
  class Snippet(val name: String, _func: => NodeSeq => NodeSeq) extends ValueSnippets[Any] with AnyLocParam {
    /**
     * The NodeSeq => NodeSeq function 
     */
    def func: NodeSeq => NodeSeq = _func

    def snippets = { case (`name`, _) => func }
  }

  object Snippet {
    /**
     * Build a Loc.Snippet instance out of a name and a DispatchSnippet (or StatefulSnippet, LiftScreen or Wizard).
     * The "render" method will be invoked on the Dispatch snippet
     */
    def apply(name: String, snippet: => DispatchSnippet)(implicit disambiguate: DummyImplicit): Snippet =
      new Snippet(name, ns => snippet.dispatch("render")(ns)) // Issue #919

    /**
     * Build a Loc.Snippet instance for a given name and a function.  Note that the function is call-by-name
     * so that it will be created each time it's used.  This is useful for CSS Selector Transforms
     */
    def apply(name: String, func: => NodeSeq => NodeSeq): Snippet = new Snippet(name, func)
    def unapply(in: Snippet): Option[(String, NodeSeq => NodeSeq)] = Some(in.name -> in.func)
  }

  /**
   * Allows you to create a handler for many snippets that are associated with
   * a Loc, but agnostic to the `Loc`'s value.
   * @see ValueSnippets
   * @see ValueSnippets.apply
   */
  trait LocSnippets extends PartialFunction[String, NodeSeq => NodeSeq] with ValueSnippets[Any] with AnyLocParam {
    def snippets = {
      case (s, _) if isDefinedAt(s) => apply(s)
    }
  }

  /**
   * A subclass of LocSnippets with a built in dispatch method (no need to
   * implement isDefinedAt or apply... just
   * def dispatch: PartialFunction[String, NodeSeq => NodeSeq].
   * @see ValueSnippets
   * @see ValueSnippets.apply
   */
  trait DispatchLocSnippets extends LocSnippets {
    def dispatch: PartialFunction[String, NodeSeq => NodeSeq]

    def isDefinedAt(n: String) = dispatch.isDefinedAt(n)

    def apply(n: String) = dispatch.apply(n)
  }

  /**
   * If this parameter is included, the item will not be visible in the menu, but
   * will still be accessable.
   */
  case object Hidden extends AnyLocParam

  /**
   * If this parameter is included, the Loc will continue to execute even if
   * currentValue is not defined.
   *
   * By default, Lift will determine that a Loc does not match a given request
   * if its currentValue comes up Empty, and as a result will return an HTTP 404.
   * For situations where this is not the desired, "Not Found" behavior, you can
   * add the MatchWithoutCurrentValue LocParam to a Loc, then use the IfValue
   * LocParam to define what should happen when the currentValue is Empty.
   *
   * For example, given some class Thing, you could do the following to trigger
   * a redirect when a Thing with a particular ID isn't found.
   *
   * {{{
   * Menu.param[Thing]("Thing", "Thing", Thing.find(_), _.id) >>
   *   MatchWithoutCurrentValue >>
   *   IfValue(_.isDefined, () => RedirectResponse("/page/to/redirect/to"))
   * }}}
   */
  case object MatchWithoutCurrentValue extends AnyLocParam

  /**
   * If this is a submenu, use the parent Loc's params
   */
  case class UseParentParams() extends AnyLocParam

  /**
   * Calculate additional query parameters to add as a query
   * string to the Loc
   */
  case class QueryParameters(f: () => List[(String, String)]) extends AnyLocParam

  /**
   * Calculate additional query parameters to add as a query
   * string to the Loc
   */
  case class LocQueryParameters[T](f: Box[T] => List[(String, String)]) extends LocParam[T]


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
   * A function that calculates the statelessness of the Loc for the given request
   */
  case class CalcStateless(f: () => Boolean) extends AnyLocParam

  /**
   * A function that calculates the statelessness of the Loc for the given request
   * with the parameterized type passed into the function
   */
  case class CalcParamStateless[-T](f: Box[T] => Boolean) extends LocParam[T]

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
      val path: List[String] = pathList(value).map(Helpers.urlEncode)

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

