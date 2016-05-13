package net.liftweb.javascript

import net.liftweb.http.{TransientRequestVar, RequestVar, LiftRules}
import net.liftweb.util._
import Helpers._
import org.mozilla.javascript.{NativeJavaObject, ScriptableObject, Context}
import xml.NodeSeq
import net.liftweb.actor.LAFuture
import net.liftweb.common._

/**
 * You can add a JavaScript context to Lift so that
 * you can run server-side JavaScript as part of Lift page
 * rendering.
 *
 * In Boot.scala, just do `JavaScriptContext.install()`
 * and you get a JavaScript execution context around
 * all HTTP requests.
 */
object JavaScriptContext {
  /**
   * Hook into LiftRules to put a JavaScript
   * execution loanwrapper around everything and
   * also slurp in <script> tags with the data-lift-server attribute.
   */
  def install() {
    LiftRules.allAround.append(JSWrapper)
    LiftRules.tagProcessor.prepend {
      case ("script", e, session) if e.attribute("data-lift-server").isDefined =>
        exec(e.text)
        NodeSeq.Empty
    }

    LiftRules.dataAttributeProcessor.append {
      case ("jssource", value, elem, session) =>

        val (rule, v2): (NodeSeq => NodeSeq, Box[String]) =
          value.roboSplit("\\#\\>") match {
            case x :: Nil => (PassThru, Full(x))
            case x :: "it" :: Nil => session.buildXformer(x, Nil) -> Empty
            case x :: str :: Nil if str.startsWith("it.") =>
              session.buildXformer(x, str.roboSplit("\\.").filter(_ != "it")) -> Empty
            case x :: xs => session.buildXformer(x, Nil) -> Full(xs.mkString)
            case _ => (PassThru, Full(value))
          }


        v2 match {
          case Full(v22) =>
            exec(v22) match {
              case fut: LAFuture[_] => val ret = new LAFuture[NodeSeq]
              fut.foreach(v => ret.satisfy(session.runSourceContext(v, rule, elem)))
              ret

              case func: Function0[_] =>
                () => {
                  session.runSourceContext(func(), rule, elem)
                }

              case x => session.runSourceContext(x, rule, elem)
            }

          case _ => rule(elem)
        }

    }
  }

  private object currentScript extends TransientRequestVar[JSScope](new JSScope) {
    registerCleanupFunc(in => get.bye())
  }

  private object JSWrapper extends LoanWrapper {
    def apply[T](f: => T): T = {
      currentScript.get
      f
    }
  }

  /**
   * Execute some JavaScript in the current context
   * @param str the string to execute
   * @return the value returned from the JavaScript execution
   */
  def exec(str: String): AnyRef = currentScript.get.exec(str)

  private class JSScope {
    private var initted = false
    private var context: Context = null
    private var scope: ScriptableObject = null



    def init() {
      context = Context.enter()
      scope = context.initStandardObjects()
    }

    def bye() {
      if (initted) Context.exit()
    }

    def exec(str: String): AnyRef = synchronized{
      if (!initted) init()
      context.evaluateString(scope, str, "Lift", 0, null)  match {
        case njo: NativeJavaObject => njo.unwrap()
        case x => x
      }
    }
  }
}

