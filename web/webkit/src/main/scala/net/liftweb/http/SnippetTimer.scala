package net.liftweb.http

import xml.NodeSeq
import net.liftweb.util.TimeHelpers._
import net.liftweb.util.Props

/**
 * Times the performance of evaluating of individual snippets
 */
object SnippetTimer {

  /**
   * The configured snippet timing function.  Defaults to not timing snippets.
   */
  lazy val evaluate: String => (=> NodeSeq) => NodeSeq = {
    if(Props.getBool("run.timesnippets",defVal = false))
      timeSnippet _
    else
      noTiming _
  }

  /**
   * Times the evaluation of a snippet
   * @param snippetName String The name of the snippet
   * @param f The snippet function
   * @return NodeSeq The result of evaluating f
   */
  def timeSnippet(snippetName:String)(f: => NodeSeq) = {
    logTime("Snippet %s (and children) evaluation" format snippetName, f)
  }

  /**
   * A function which doesn't time a snippet but just evaluates it
   * @param snippetName String Name of the snippet which is ignored
   * @param f The snippet function
   * @return NodeSeq The result of evaluating f
   */
  def noTiming(snippetName:String)(f: => NodeSeq) = f
}
