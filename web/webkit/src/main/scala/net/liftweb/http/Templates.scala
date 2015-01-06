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
package http 

import common._
import java.util.Locale
import scala.xml._
import util._
import Helpers._
import java.io.InputStream

/**
 * Contains functions for obtaining templates
 */
object Templates {
  private val suffixes = LiftRules.templateSuffixes

  private def checkForLiftView(part: List[String], last: String, what: LiftRules.ViewDispatchPF): Box[NodeSeq] = {
    if (what.isDefinedAt(part)) {
      what(part) match {
        case Right(lv) => if (lv.dispatch.isDefinedAt(last)) lv.dispatch(last)() else Empty
        case _ => Empty
      }
    } else Empty
  }

  private def checkForFunc(whole: List[String], what: LiftRules.ViewDispatchPF): Box[NodeSeq] =
    if (what.isDefinedAt(whole)) what(whole) match {
      case Left(func) => func()
      case _ => Empty
    }
    else Empty

  private def findInViews(whole: List[String], part: List[String],
                          last: String,
                          what: List[LiftRules.ViewDispatchPF]): Box[NodeSeq] =
    what match {
      case Nil => Empty
      case x :: xs =>
        (checkForLiftView(part, last, x) or checkForFunc(whole, x)) match {
          case Full(ret) => Full(ret)
          case _ => findInViews(whole, part, last, xs)
        }
    }

  private [http] def findTopLevelTemplate(places: List[String], locale: Locale, needAutoSurround: Boolean) = {
    findRawTemplate0(places, locale, needAutoSurround).map(checkForContentId)
  }

  /**
   * Given a list of paths (e.g. List("foo", "index")),
   * find the template.  This method runs checkForContentId
   * on any found templates.  To get the raw template,
   * use findRawTemplate
   * @param places - the path to look in
   *
   * @return the template if it can be found
   */
  def apply(places: List[String]): Box[NodeSeq] = 
    apply(places, S.locale)


  /**
   * Given a list of paths (e.g. List("foo", "index")),
   * find the template.  This method runs checkForContentId
   * on any found templates.  To get the raw template,
   * use findRawTemplate
   * @param places - the path to look in
   * @param locale the locale of the template
   *
   * @return the template if it can be found
   */
  def apply(places: List[String], locale: Locale): Box[NodeSeq] = 
    findRawTemplate(places, locale).map(checkForContentId)

  /**
   * Check to see if the template is marked designer friendly
   * and lop off the stuff before the first surround
   */
  def checkForContentId(in: NodeSeq): NodeSeq = {
    def df(in: MetaData): Option[PrefixedAttribute] = in match {
      case Null => None
      case p: PrefixedAttribute 
      if (p.pre == "l" || p.pre == "lift") && 
      (p.key == "content_id") => Some(p)
      case n => df(n.next)
    }
    
    
    in.flatMap {
      case e: Elem if e.label == "html" => df(e.attributes)
      case _ => None
    }.flatMap {
      md => Helpers.findId(in, md.value.text)
    }.headOption orElse 
    in.flatMap {
      case e: Elem if e.label == "html" =>
        e.child.flatMap {
          case e: Elem if e.label == "body" => {
            e.attribute("data-lift-content-id").headOption.map(_.text) orElse
            e.attribute("class").flatMap {
              ns => {
                val clz = ns.text.charSplit(' ')
                clz.flatMap {
                  case s if s.startsWith("lift:content_id=") =>
                    Some(urlDecode(s.substring("lift:content_id=".length)))
                  case _ => None
                }.headOption
                
              }
            }
          }

          case _ => None
        }
      case _ => None
    }.flatMap {
      id => Helpers.findId(in, id)
    }.headOption getOrElse in
  }

  private def parseMarkdown(is: InputStream, needAutoSurround: Boolean): Box[NodeSeq] =
  for {
    bytes <- Helpers.tryo(Helpers.readWholeStream(is))
    elems <- MarkdownParser.parse(new String(bytes, "UTF-8"))
  } yield {
    if (needAutoSurround)
      <lift:surround with="default" at="content">{elems}</lift:surround>
    else
      elems
  }

  /**
   * Given a list of paths (e.g. List("foo", "index")),
   * find the template.
   * @param places - the path to look in
   * @param locale - the locale of the template to search for
   *
   * @return the template if it can be found
   */
  def findRawTemplate(places: List[String], locale: Locale): Box[NodeSeq] = {
    findRawTemplate0(places, locale, false)
  }

  private def findRawTemplate0(places: List[String], locale: Locale, needAutoSurround: Boolean): Box[NodeSeq] = {
    /*
     From a Scala coding standpoint, this method is ugly.  It's also a performance
     hotspot that needed some tuning.  I've made the code very imperative and
     tried to make sure there are no anonymous functions created in this method.
     The few extra lines of code and the marginal reduction in readibility should
     yield better performance.  Please don't change this method without chatting with
     me first.  Thanks!  DPP
     */

     val resolver = LiftRules.externalTemplateResolver.vend()
    val key = (locale, places)

     if (resolver.isDefinedAt(key)) {
      resolver(key)
      } else {
    val lrCache = LiftRules.templateCache
    val cache = if (lrCache.isDefined) lrCache.openOrThrowException("passes isDefined") else NoCache

    val parserFunction: InputStream => Box[NodeSeq] = 
      S.htmlProperties.htmlParser


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

            val se = suffixes.iterator
            val sl = List("_" + locale.toString, "_" + locale.getLanguage, "")

            var found = false
            var ret: NodeSeq = null

            while (!found && se.hasNext) {
              val s = se.next
              val le = sl.iterator
              while (!found && le.hasNext) {
                val p = le.next
                val name = pls + p + (if (s.length > 0) "." + s else "")
                import scala.xml.dtd.ValidationException
                val xmlb = try {
                  LiftRules.doWithResource(name) { is =>
                    if (s == "md") {parseMarkdown(is, needAutoSurround)} else
                    parserFunction(is) } match {
                    case Full(seq) => seq
                    case _ => Empty
                  }
                } catch {
                  case e: ValidationException if Props.devMode | Props.testMode =>
                    return Helpers.errorDiv(<div>Error locating template: <b>{name}</b><br/>
                      Message: <b>{e.getMessage}</b><br/>
                      {
                      <pre>{e.toString}{e.getStackTrace.map(_.toString).mkString("\n")}</pre>
                      }
                    </div>)

                  case e: ValidationException => Empty
                }
                if (xmlb.isDefined) {
                  found = true
                  ret = (cache(key) = xmlb.openOrThrowException("passes isDefined"))
                } else if (xmlb.isInstanceOf[Failure] && 
                           (Props.devMode | Props.testMode)) {
                  val msg = xmlb.asInstanceOf[Failure].msg
                  val e = xmlb.asInstanceOf[Failure].exception
                  return Helpers.errorDiv(<div>Error locating template: <b>{name}</b><br/>Message: <b>{msg}</b><br/>{
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
}

  private def lookForClasses(places: List[String]): Box[NodeSeq] = {
    val (controller, action) = places match {
      case ctl :: act :: _ => (ctl, act)
      case ctl :: _ => (ctl, "index")
      case Nil => ("default_template", "index")
    }
    val trans = List[String => String](n => n, n => camelify(n))
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
                case ite: java.lang.reflect.InvocationTargetException => 
                  throw ite.getCause
                case e: NoClassDefFoundError => Empty
              }
    }
  }
}

/**
 * Throw this exception if there's a catostrophic failure executing
 * a snippet
 */
class SnippetExecutionException(msg: String) extends SnippetFailureException(msg) {
  def snippetFailure = LiftRules.SnippetFailures.ExecutionFailure 
}

/**
 * An abstract exception that may be thrown during page rendering.
 * The exception is caught and the appropriate report of a SnippetError
 * is generated
 */
abstract class SnippetFailureException(msg: String) extends LiftFlowOfControlException(msg) {
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


  // FIXME Needed to due to https://issues.scala-lang.org/browse/SI-6541,
  // which causes existential types to be inferred for the generated
  // unapply of a case class with a wildcard parameterized type.
  // Ostensibly should be fixed in 2.12, which means we're a ways away
  // from being able to remove this, though.
  import scala.language.existentials

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

