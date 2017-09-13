package net.liftweb.http

import xml.NodeSeq
import net.liftweb.util.TimeHelpers._
import net.liftweb.util.Props

/**
 * A snippet timer is a general interface for timing snippets. A few default implementations are
 * provided and can be selected by setting LiftRules.snippetTimer as you need.
 */
trait SnippetTimer {
  def timeSnippet(snippetName: String)(snippetFunc: => NodeSeq): NodeSeq
}

/**
 * A SnippetTimer that does not do anything.
 */
object NoOpSnippetTimer extends SnippetTimer {
  override def timeSnippet(snippetName: String)(snippetFunc: => NodeSeq): NodeSeq = snippetFunc
}

/**
 * A SnippetTimer that logs its timings to the console.
 */
object LoggingSnippetTimer extends SnippetTimer {
  override def timeSnippet(snippetName: String)(snippetFunc: => NodeSeq): NodeSeq = {
    logTime(s"Snippet $snippetName evaluation", snippetFunc)
  }
}
