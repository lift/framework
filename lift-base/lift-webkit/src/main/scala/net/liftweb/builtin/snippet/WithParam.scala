package net.liftweb.builtin.snippet

import _root_.scala.xml._
import _root_.scala.collection.immutable.{Map}
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.js._
import _root_.net.liftweb.common._
import _root_.net.liftweb.util._

object WithParamVar extends RequestVar[Map[String, NodeSeq]](Map.empty)

object WithParam extends DispatchSnippet {

  def dispatch : DispatchIt = {
    case _ => render _
  }

  def render(kids: NodeSeq) : NodeSeq = {
    (for {ctx <- S.session ?~ ("FIX"+"ME: Invalid session")
          req <- S.request ?~ ("FIX"+"ME: Invalid request")
    } yield {
       val name = S.attr.~("name").map(_.text).getOrElse("main")
       WithParamVar(WithParamVar.get + (name -> ctx.processSurroundAndInclude(PageName.get, kids)))
       NodeSeq.Empty
    }) match {
      case Full(x) => x
      case Empty => Comment("FIX"+ "ME: session or request are invalid")
      case Failure(msg, _, _) => Comment(msg)
    }
  }

}
