package net.liftweb.util

import xml.{Elem, NodeSeq}
import net.liftweb.common.Box
import net.liftweb.markdown.ThreadLocalTransformer

object MarkdownParser {
  lazy val matchMetadata = """(?m)\A(:?[ \t]*\n)?(?:-{3,}+\n)?(^([a-zA-Z0-9 _\-]+)[=:]([^\n]*)\n+(:?[ \t]*\n)?)+(:?-{3,}+\n)?""".r

  lazy val topMetadata = """(?m)^([^:]+):[ \t]*(.*)$""".r

  lazy val lineSplit = """(?m)^(.*)$""".r

  lazy val linkDefs = """(?m)^\p{Space}{0,3}\[([^:]+)[=:](?:[ ]*)(.+)\]:""".r

  lazy val hasYaml = """(?s)(?m)^[yY][aA][mM][lL][ \t]*\{[ \t]*$(.*?)^\}[ \t]*[yY][Aa][mM][Ll][ \t]*$""".r
  lazy val htmlHasYaml = """(?s)(?m)\A(:?[ \t]*\n)*^[yY][aA][mM][lL][ \t]*\{[ \t]*$(.*?)^\}[ \t]*[yY][Aa][mM][Ll][ \t]*$""".r

  def childrenOfBody(in: NodeSeq): NodeSeq = {
    (in \ "body").toList match {
      case Nil => in
      case xs => xs.collect {
        case e: Elem => e
      }.flatMap(_.child)
    }
  }

  private lazy val threadLocalTransformer = new ThreadLocalTransformer

  def parse(in: String): Box[NodeSeq] = {
    for {
      str <- Helpers.tryo(threadLocalTransformer.apply(in))
      res = Html5.parse("<html><head><title>I eat yaks</title></head><body>" + str + "</body></html>")
      info <- res.map {
        res => (res \ "body").collect {
          case e: Elem => e
        }.flatMap(_.child)
      }
    } yield info
  }
}
