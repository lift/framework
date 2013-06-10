package net.liftweb.markdown

/*
 * Copyright 2013 WorldWide Conferencing, LLC
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
 *
 * Based on https://github.com/chenkelmann/actuarius originally developed by
 * Christoph Henkelmann http://henkelmann.eu/
 */

import scala.util.parsing.input.{Position, Reader}
import java.util.StringTokenizer
import scala.collection.mutable.{HashMap, ArrayBuffer, ListBuffer}


/**
 * Represents a line of markdown.
 * The prefix is the beginning of the line that indicates the line type,
 * the payload is the actual content after the prefix.
 */
sealed abstract class MarkdownLine(val prefix:String, val payload:String){
    /**
     * Constructs a MarkdownLine where the prefix is the empty String and the
     * payload is the whole line.
     */
    def this(c:String) = this ("", c)

    /**
     * Returns the full line as it was originally, i.e. prefix+payload.
     */
    def fullLine = prefix + payload
}

/**Represents lines of verbatim xml.
 * Actually this class is a little cheat, as it represents multiple lines.
 * But it is a token that is created when "parsing with a line scope", so it is not too bad.
 */
case class XmlChunk(content:String) extends MarkdownLine(content)
/** Represents the underline for a setext style header
 */
case class SetExtHeaderLine(content:String, headerLevel:Int) extends MarkdownLine(content)

/**
 * An atx style header line.
 * Trims hashes automatically and determines the header level from them.
 */
case class AtxHeaderLine(pre:String, pay:String) extends MarkdownLine(pre, pay) {
    /** removes all whitespace, nl and trailing hashes from the payload
     * "  foo ##  \n" => "foo"
     */
    def trimHashes() = {
        val s = payload.trim
        var idx = s.length - 1
        while (idx >= 0 && s.charAt(idx) == '#') idx -= 1
        s.substring(0,idx+1).trim
    }

    def headerLevel = prefix.length
}
/** A line consisting only of whitespace.
 */
case class EmptyLine(content:String) extends MarkdownLine(content)
/** A horizontal ruler line.
 */
case class RulerLine(content:String) extends MarkdownLine(content)
/** A line indicating a block quote (starts with "> ")
 */
case class BlockQuoteLine(pre:String, pay:String) extends MarkdownLine(pre, pay)
/** A line indicating the start of an unordered list item (starts with "   *  ")
 */
case class UItemStartLine(pre:String, pay:String) extends MarkdownLine(pre, pay)
/** A line indicating the start of an ordered list item (starts with "   [NUMBER].  ")
 */
case class OItemStartLine(pre:String, pay:String) extends MarkdownLine(pre, pay)
/** A line in verbatim code or the continuation of a list item
 */
case class CodeLine(pre:String, pay:String) extends MarkdownLine(pre, pay)
/** Starting line of a fenced code block: three backticks followed by an optional
 * language token
 */
case class ExtendedFencedCode(pre:String, pay:String) extends MarkdownLine(pre, pay) {
    def languageFormat = pay.trim()
}
/** Ending line of a fenced code block: three backticks followed by optional whitespace 
 */
case class FencedCode(pre:String) extends MarkdownLine(pre)
/** Any other line.
 */
case class OtherLine(content:String) extends MarkdownLine(content)


/** Definition of a link or url that can be referenced by id.
 */
case class LinkDefinition(id:String, url:String, title:Option[String])

/** Stub class that is an intermediate result when parsing link definitions.
 */
case class LinkDefinitionStart(id:String, url:String) {
    def toLinkDefinition(title:Option[String]) = new LinkDefinition(id, url, title)
}

/**
 * This class allows us to reference a map with link definitions resulting from the line parsing during block parsing.
 * It extends a Reader for MarkdownLines and allows us to add the said map to the parsing context.
 * This is basically a modification of the parser monad's state. 
 */
case class MarkdownLineReader private (val lines:Seq[MarkdownLine],
                                       val lookup:Map[String, LinkDefinition],
                                       val lineCount:Int)
        extends Reader[MarkdownLine] {
    /** Not existing line that signals EOF.
     * This object cannot be referenced by any other code so it will fail all line parsers. 
     */
    private object EofLine extends MarkdownLine("\nEOF\n")


    def this(ls:Seq[MarkdownLine], lu:Map[String, LinkDefinition]) = this(ls, lu, 1)
    def this(ls:Seq[MarkdownLine]) = this (ls, Map())
    def first = if (lines.isEmpty) EofLine else lines.head
    def rest  = if (lines.isEmpty) this else new MarkdownLineReader(lines.tail, lookup, lineCount + 1)
    def atEnd = lines.isEmpty
    def pos   = new Position {
        def line   = lineCount
        def column = 1
        protected def lineContents = first.fullLine
    }
}

/**
 * Parses single lines into tokens.
 * Markdown lines are differentiated by their beginning.
 * These lines are then organized in blocks by the BlockParsers.
 */
trait LineParsers extends InlineParsers {

    /////////////////////////////////
    // Link definition pre-parsing //
    /////////////////////////////////

    /** The Start of a link definition: the id in square brackets, optionally indented by three spaces
     */
    def linkDefinitionId:Parser[String] =
        """ {0,3}\[""".r ~> markdownText(Set(']'), true) <~ ("]:" ~ ows) ^^ {_.trim.toLowerCase}
    /** The link url in a link definition.
     */
    def linkDefinitionUrl:Parser[String] =
        (elem('<') ~> markdownText(Set('>'), true) <~ '>' ^^ {_.mkString.trim}) |
        (markdownText(Set(' ','\t'), true) ^^ {_.mkString})
    /** The title in a link definition.
     */
    def linkDefinitionTitle:Parser[String] =
        ows ~> ("""\"[^\n]*["]""".r |
                """\'[^\n]*\'""".r  |
                """\([^\n]*\)""".r) <~ ows ^^ { s => s.substring(1,s.length-1) }

    /** A link definition that later gets stripped from the output.
     * Either a link definition on one line or the first line of a two line link definition.
     */
    def linkDefinitionStart:Parser[(LinkDefinitionStart, Option[String])] =
        linkDefinitionId ~ linkDefinitionUrl ~ opt(linkDefinitionTitle) ^^ {case i ~ u ~ t => (new LinkDefinitionStart(i, u), t)}


    //////////////////////////////////////////
    // Lines for XML Block tokenizing       //
    //////////////////////////////////////////

    /** A line that starts an xml block: an opening xml element fragment.
     */
    def xmlBlockStartLine:Parser[String] = guard('<' ~ xmlName) ~> rest
    /** A line that ends an xml block: a line starting with an xml end tag
     */
    def xmlBlockEndLine:Parser[String] = guard(xmlEndTag) ~> rest
    /** A line not starting with an xml end tag
     */
    def notXmlBlockEndLine:Parser[String] = not(xmlEndTag) ~> rest


    //////////////////////////////
    // Markdown line tokenizing //
    //////////////////////////////

    /** Parses the line under a setext style level 1 header: =====
     */
    val setextHeader1:Parser[SetExtHeaderLine] = """=+([ \t]*)$""".r ^^ {new SetExtHeaderLine(_, 1)}

    /** Parses the line under a setext style level 2 header: -----
     */
    val setextHeader2:Parser[SetExtHeaderLine] = """((\-)+)([ \t]*)$""".r ^^ {new SetExtHeaderLine(_, 2)}

    /** Parses headers of the form: ### header ###
     */
    val atxHeader:Parser[AtxHeaderLine] = """#+""".r ~ rest ^^ {
        case prefix ~ payload => new AtxHeaderLine(prefix, payload)
    }

    /** Parses a horizontal rule.
     */
    val ruler:Parser[MarkdownLine] = """ {0,3}(((-[ \t]*){3,})|((\*[ \t]*){3,}))$""".r ^^ { new RulerLine(_) }

    /** Matches a line starting with up to three spaces, a '>' and an optional whitespace.
     * (i.e.: the start or continuation of a block quote.)
     */
    val blockquoteLine:Parser[BlockQuoteLine] = """ {0,3}\>( )?""".r ~ rest ^^ {
        case prefix ~ payload => new BlockQuoteLine(prefix,payload)
    }


    /** A line that starts an unordered list item.
     * Matches a line starting with up to three spaces followed by an asterisk, a space, and any whitespace.
     */
    val uItemStartLine:Parser[UItemStartLine] = (""" {0,3}[\*\+-] [\t\v ]*""".r) ~ rest ^^ {
        case prefix ~ payload => new UItemStartLine(prefix, payload)
    }


    /** A line that starts an ordered list item.
     * Matches a line starting with up to three spaces followed by a number, a dot and a space, and any whitespace
     */
    val oItemStartLine:Parser[OItemStartLine] = (""" {0,3}[0-9]+\. [\t\v ]*""".r) ~ rest ^^ {
        case prefix ~ payload => new OItemStartLine(prefix, payload)
    }

    /** Accepts an empty line. (A line that consists only of optional whitespace or the empty string.)
     */
    val emptyLine:Parser[MarkdownLine] = """([ \t]*)$""".r ^^ {new EmptyLine(_)}

    /** Matches a code example line: any line starting with four spaces or a tab.
     */
    val codeLine:Parser[CodeLine] = ("    " | "\t") ~ rest ^^ {
        case prefix ~ payload => new CodeLine(prefix, payload)
    }
        
    /**
     * A fenced code line. Can be the start or the end of a fenced code block 
     */
    val fencedCodeLine:Parser[FencedCode] = """ {0,3}\`{3,}[\t\v ]*""".r ^^ {
        case prefix => new FencedCode(prefix) 
    }
        
    /** Matches the start of a fenced code block with additional language token: 
     * up to three spaces, three or more backticks, whitespace, an optional
     * language token, optional whitespace 
     */
    val extendedFencedCodeLine:Parser[ExtendedFencedCode] = fencedCodeLine ~ """\w+[\t\v ]*""".r ^^ {
        case prefix ~ languageToken => new ExtendedFencedCode(prefix.fullLine, languageToken) 
    }  

    /** Matches any line. Only called when all other line parsers have failed.
     * Makes sure line tokenizing does not fail and we do not loose any lines on the way.
     */
    val otherLine:Parser[OtherLine] = rest ^^ {new OtherLine(_)}

    ///////////////////////////////////////////////////////////////
    // combined parsers for faster tokenizing based on lookahead //
    ///////////////////////////////////////////////////////////////
    /** First tries for a setext header level 2, then for a ruler.
     */
    val setext2OrRulerOrUItem:Parser[MarkdownLine] = setextHeader2 | ruler | uItemStartLine
    /** First tries for a ruler, then for an unordered list item start.
     */
    val rulerOrUItem:Parser[MarkdownLine] = ruler | uItemStartLine
    /** First tries if the line is empty, if not tries for a code line.
     */
    val emptyOrCode:Parser[MarkdownLine] = emptyLine | codeLine
    
    /** Parses one of the fenced code lines
     */
    val fencedCodeStartOrEnd:Parser[MarkdownLine] = extendedFencedCodeLine | fencedCodeLine  
}

