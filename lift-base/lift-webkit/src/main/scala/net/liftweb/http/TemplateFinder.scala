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
package http {

import common._
import _root_.java.util.Locale
import _root_.scala.xml._
import util._
import Helpers._
import _root_.java.io.InputStream

/**
 * Contains functions for obtaining templates
 */
object TemplateFinder {
  private val suffixes = List("html", "xhtml", "htm")

  import LiftRules.ViewDispatchPF

  private def checkForLiftView(part: List[String], last: String, what: ViewDispatchPF): Box[NodeSeq] = {
    if (what.isDefinedAt(part)) {
      what(part) match {
        case Right(lv) => if (lv.dispatch.isDefinedAt(last)) lv.dispatch(last)() else Empty
        case _ => Empty
      }
    } else Empty
  }

  private def checkForFunc(whole: List[String], what: ViewDispatchPF): Box[NodeSeq] =
    if (what.isDefinedAt(whole)) what(whole) match {
      case Left(func) => func()
      case _ => Empty
    }
    else Empty

  private def findInViews(whole: List[String], part: List[String],
                          last: String,
                          what: List[ViewDispatchPF]): Box[NodeSeq] =
    what match {
      case Nil => Empty
      case x :: xs =>
        (checkForLiftView(part, last, x) or checkForFunc(whole, x)) match {
          case Full(ret) => Full(ret)
          case _ => findInViews(whole, part, last, xs)
        }
    }

  /**
   * Given a list of paths (e.g. List("foo", "index")),
   * find the template.
   * @param places - the path to look in
   *
   * @return the template if it can be found
   */
  def findAnyTemplate(places: List[String]): Box[NodeSeq] =
    findAnyTemplate(places, S.locale)

  /**
   * Given a list of paths (e.g. List("foo", "index")),
   * find the template.
   * @param places - the path to look in
   * @param locale - the locale of the template to search for
   *
   * @return the template if it can be found
   */
  def findAnyTemplate(places: List[String], locale: Locale): Box[NodeSeq] = {
    /*
     From a Scala coding standpoint, this method is ugly.  It's also a performance
     hotspot that needed some tuning.  I've made the code very imperative and
     tried to make sure there are no anonymous functions created in this method.
     The few extra lines of code and the marginal reduction in readibility should
     yield better performance.  Please don't change this method without chatting with
     me first.  Thanks!  DPP
     */
    val lrCache = LiftRules.templateCache
    val cache = if (lrCache.isDefined) lrCache.open_! else NoCache

    val parserFunction: InputStream => Box[NodeSeq] = 
      S.htmlProperties.htmlParser

    val key = (locale, places)
    val tr = cache.get(key)

    if (tr.isDefined) tr
    else
      {
        val part = places.dropRight(1)
        val last = places.last

        findInViews(places, part, last, LiftRules.viewDispatch.toList) match {
          case Full(lv) =>
            Full(lv)

          case _ =>
            val pls = places.mkString("/", "/", "")

            val se = suffixes.elements
            val sl = List("_" + locale.toString, "_" + locale.getLanguage, "")

            var found = false
            var ret: NodeSeq = null

            while (!found && se.hasNext) {
              val s = se.next
              val le = sl.elements
              while (!found && le.hasNext) {
                val p = le.next
                val name = pls + p + (if (s.length > 0) "." + s else "")
                import scala.xml.dtd.ValidationException
                val xmlb = try {
                  LiftRules.doWithResource(name) { parserFunction } match {
                    case Full(seq) => seq
                    case _ => Empty
                  }
                } catch {
                  case e: ValidationException if Props.devMode | Props.testMode =>
                    return Helpers.errorDiv(<div>Error locating template {name}.<br/>
                      Message:{e.getMessage}<br/>
                      {
                      <pre>{e.toString}{e.getStackTrace.map(_.toString).mkString("\n")}</pre>
                      }
                    </div>)

                  case e: ValidationException => Empty
                }
                if (xmlb.isDefined) {
                  found = true
                  ret = (cache(key) = xmlb.open_!)
                } else if (xmlb.isInstanceOf[Failure] && 
                           (Props.devMode | Props.testMode)) {
                  val msg = xmlb.asInstanceOf[Failure].msg
                  val e = xmlb.asInstanceOf[Failure].exception
                  return Helpers.errorDiv(<div>Error locating template {name}.<br/>Message: {msg}<br/>{
                  {
                    e match {
                      case Full(e) =>
                        <pre>{e.toString}{e.getStackTrace.map(_.toString).mkString("\n")}</pre>
                      case _ => NodeSeq.Empty
                    }
                  }}
                  </div>)
                }
              }
            }

            if (found) Full(ret)
            else lookForClasses(places)
        }
      }
  }

  private def lookForClasses(places: List[String]): Box[NodeSeq] = {
    val (controller, action) = places match {
      case ctl :: act :: _ => (ctl, act)
      case ctl :: _ => (ctl, "index")
      case Nil => ("default_template", "index")
    }
    val trans = List[String => String](n => n, n => camelCase(n))
    val toTry = trans.flatMap(f => (LiftRules.buildPackage("view") ::: ("lift.app.view" :: Nil)).map(_ + "." + f(controller)))

    first(toTry) {
      clsName =>
              try {
                tryo(List(classOf[ClassNotFoundException]), Empty)(Class.forName(clsName).asInstanceOf[Class[AnyRef]]).flatMap {
                  c =>
                          (c.newInstance match {
                            case inst: InsecureLiftView => c.getMethod(action).invoke(inst)
                            case inst: LiftView if inst.dispatch.isDefinedAt(action) => inst.dispatch(action)()
                            case _ => Empty
                          }) match {
                            case null | Empty | None => Empty
                            case n: Group => Full(n)
                            case n: Elem => Full(n)
                            case s: NodeSeq => Full(s)
                            case Some(n: Group) => Full(n)
                            case Some(n: Elem) => Full(n)
                            case Some(n: NodeSeq) => Full(n)
                            case Some(SafeNodeSeq(n)) => Full(n)
                            case Full(n: Group) => Full(n)
                            case Full(n: Elem) => Full(n)
                            case Full(n: NodeSeq) => Full(n)
                            case Full(SafeNodeSeq(n)) => Full(n)
                            case _ => Empty
                          }
                }
              } catch {
                case ite: _root_.java.lang.reflect.InvocationTargetException /* if (ite.getCause.isInstanceOf[ResponseShortcutException]) */ => throw ite.getCause
                case re: ResponseShortcutException => throw re
                case _ => Empty
              }
    }
  }
}

/**
* A case class that contains the information necessary to set up a CometActor
*/
final case class CometCreationInfo(contType: String,
                                   name: Box[String],
                                   defaultXml: NodeSeq,
                                   attributes: Map[String, String],
                                   session: LiftSession)

/**
 * An abstract exception that may be thrown during page rendering.
 * The exception is caught and the appropriate report of a SnippetError
 * is generated
 */
abstract class SnippetFailureException(msg: String) extends Exception(msg) {
  def snippetFailure: LiftRules.SnippetFailures.Value

  def buildStackTrace: NodeSeq = 
    getStackTrace.toList.dropWhile 
  {
    e => {
      val cn = e.getClassName
      cn.startsWith("net.liftweb.http") ||
      cn.startsWith("net.liftweb.common") ||
      cn.startsWith("net.liftweb.util")
    }
  }.filter {
    e => {
      val cn = e.getClassName
      !cn.startsWith("java.lang") &&
      !cn.startsWith("sun.")
    }
  }.take(10).toList.map{
      e =>
      <code><span><br/>{e.toString}</span></code>
    }
}

class StateInStatelessException(msg: String) extends SnippetFailureException(msg) {
  def snippetFailure: LiftRules.SnippetFailures.Value = 
    LiftRules.SnippetFailures.StateInStateless
}


  // an object that extracts an elem that defines a snippet
  private object SnippetNode {
    private def removeLift(str: String): String =
      str.indexOf(":") match {
        case x if x >= 0 => str.substring(x + 1)
        case _ => str
      }

    private def makeMetaData(key: String, value: String, rest: MetaData): MetaData = key.indexOf(":") match {
      case x if x > 0 => new PrefixedAttribute(key.substring(0, x),
                                               key.substring(x + 1),
                                               value, rest)

      case _ => new UnprefixedAttribute(key, value, rest)
    }

    private def pairsToMetaData(in: List[String]): MetaData = in match {
      case Nil => Null
      case x :: xs => {
        val rest = pairsToMetaData(xs)
        x.charSplit('=').map(Helpers.urlDecode) match {
          case Nil => rest
          case x :: Nil => makeMetaData(x, "", rest)
          case x :: y :: _ => makeMetaData(x, y, rest)
        }
      }
    }

    private def isLiftClass(s: String): Boolean =
      s.startsWith("lift:") || s.startsWith("l:")

    private def snippy(in: Elem): Option[(String, MetaData)] =
      for {
        cls <- in.attribute("class")
        snip <- cls.text.charSplit(' ').find(isLiftClass)
      } yield {
        snip.charSplit('?') match {
          case Nil => "this should never happen" -> Null
          case x :: Nil => urlDecode(removeLift(x)) -> Null
          case x :: xs => urlDecode(removeLift(x)) -> pairsToMetaData(xs.flatMap(_.roboSplit("[;&]")))
        }
      }

    private def liftAttrsAndParallel(in: MetaData): (Boolean, MetaData) = {
      var next = in
      var par = false
      var nonLift: MetaData = Null

      while (next != Null) {
        next match {
          // remove the lift class css classes from the class attribute
          case up: UnprefixedAttribute if up.key == "class" =>
            up.value.text.charSplit(' ').filter(s => !isLiftClass(s)) match {
              case Nil =>
              case xs => nonLift = new UnprefixedAttribute("class",
                                                           xs.mkString(" "),
                                                           nonLift)
            }

          case p: PrefixedAttribute 
          if (p.pre == "l" || p.pre == "lift") && p.key == "parallel"
          => par = true


          case p: PrefixedAttribute 
          if p.pre == "lift" && p.key == "snippet"
          => nonLift = p.copy(nonLift)

          

          case a => nonLift = a.copy(nonLift)
        }
        next = next.next
      }
      
      
      (par, nonLift)
    }
           
      
    
    def unapply(baseNode: Node): Option[(Elem, NodeSeq, Boolean, MetaData, String)] =
      baseNode match {
        case elm: Elem if elm.prefix == "lift" || elm.prefix == "l" => {
          Some((elm, elm.child, 
                elm.attributes.find {
                  case p: PrefixedAttribute => p.pre == "lift" && (p.key == "parallel")
                  case _ => false
                }.isDefined,
                elm.attributes, elm.label))
        }

        case elm: Elem => {
          for {
            (snippetName, lift) <- snippy(elm)
          } yield {
            val (par, nonLift) = liftAttrsAndParallel(elm.attributes)
            val newElm = new Elem(elm.prefix, elm.label, 
                                  nonLift, elm.scope, elm.child :_*)
            (newElm, newElm, par || 
             (lift.find {
               case up: UnprefixedAttribute if up.key == "parallel" => true
               case _ => false
               }.
              flatMap(up => AsBoolean.unapply(up.value.text)) getOrElse 
              false), lift, snippetName)
             
          }
        }

        case _ => {
          None
        }
      }
  }

  /**
   * Holds a pair of parameters
   */
  private case class ParamPair(v: Any, clz: Class[_])

  /**
   * a trait that defines some ways of constructing an instance
   */
  private sealed trait ConstructorType
  
  /**
   * A unit constructor... just pass in null
   */
  private final case class UnitConstructor(c: java.lang.reflect.Constructor[_]) extends ConstructorType {
    def makeOne[T]: T = c.newInstance().asInstanceOf[T]
  }

  /**
   * A parameter and session constructor
   */
  private final case class PAndSessionConstructor(c: java.lang.reflect.Constructor[_]) extends ConstructorType {
    def makeOne[T](p: Any, s: LiftSession): T = 
      c.newInstance(p.asInstanceOf[Object], s).asInstanceOf[T]
  }

  /**
   * A parameter constructor
   */
  private final case class PConstructor(c: java.lang.reflect.Constructor[_]) extends ConstructorType {
    def makeOne[T](p: Any): T = 
      c.newInstance(p.asInstanceOf[Object]).asInstanceOf[T]
  }


}
}
