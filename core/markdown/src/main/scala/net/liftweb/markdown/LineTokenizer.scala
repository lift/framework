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

import scala.collection.mutable.{HashMap, ArrayBuffer}
import scala.language.postfixOps
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{Position, Reader}
import scala.xml

/**
 * A Reader for reading whole Strings as tokens.
 * Used by the Tokenizer to parse whole lines as one Element.
 */
case class LineReader private (val lines:Seq[String],
                               val lineCount:Int)
        extends Reader[String] {
    /**should never be used anywhere, just a string that should stick out for better debugging*/
    private def eofLine = "EOF"
    def this(ls:Seq[String]) = this(ls, 1)
    def first = if (lines.isEmpty) eofLine else lines.head
    def rest  = if (lines.isEmpty) this else new LineReader(lines.tail, lineCount + 1)
    def atEnd = lines.isEmpty
    def pos   = new Position {
        def line   = lineCount
        def column = 1
        protected def lineContents = first
    }
}

/**
 * Chops the input into lines and turns those lines into line tokens.
 * Also takes care of preprocessing link definitions and xml blocks.
 */
class LineTokenizer() extends Parsers {
    object lineParsers extends LineParsers

    /**we munch whole lines (OM NOM NOM)
     */
    type Elem = String

    /** Determines if xml blocks may be included verbatim.
     * If true, they are passed through, else they are escaped and turned into paragraphs
     */
    def allowXmlBlocks = true

    /**
     * Returns a parser based on the given line parser.
     * The resulting parser succeeds if the given line parser consumes the whole String.
     */
    def p[T](parser:lineParsers.Parser[T]):Parser[T] = Parser{in =>
        if (in.atEnd) {
            Failure("End of Input.", in)
        } else {
            lineParsers.parseAll(parser, in.first) match {
                case lineParsers.Success(t, _) => Success(t, in.rest)
                case n:lineParsers.NoSuccess   => Failure(n.msg, in)
            }
        }
    }

    /** Returns the first char in the given string or a newline if the string is empty.
     * This is done to speed up header parsing. Used to speed up line tokenizing substantially
     * by using the first char in a line as lookahead for which parsers to even try.
     */
    def firstChar(line:String):Char = {
        if (line.length == 0) '\n' else line.charAt(0)
    }

    /**Finds the char in the given line that is the best indication of what kind of markdown line this is.
     * The “special” Markdown lines all start with up to three spaces. Those are skipped if present.
     * The first char after those (up to)three spaces or a newline is returned.
     */
    def indicatorChar(line:String):Char = {
        var i = 0
        //skip the first three spaces, if present
        while (i < 3 && i < line.length && line.charAt(i) == ' ') i += 1
        //return the next char after the spaces or a newline if there are no more
        if (i==line.length) '\n'
        else                line.charAt(i)
    }

    ////////////////////////
    // Link definitions   //
    ////////////////////////

    /** Tries to parse an URL from the next line if necessary.
     * The passed tuple is the result from a previous parser and used to decide how to continue parsing.
     */
    def maybeUrlInNextLine(prev:(LinkDefinitionStart, Option[String])):Parser[LinkDefinition] = prev match {
        case (lds, Some(title)) => success(lds.toLinkDefinition(Some(title)))
        case (lds, None)        => Parser {in =>
            if (in.atEnd) {
                Success(lds.toLinkDefinition(None), in)
            } else {
                lineParsers.parseAll(lineParsers.linkDefinitionTitle, in.first) match {
                    case lineParsers.Success(title, _) => Success(lds.toLinkDefinition(Some(title)), in.rest)
                    case _                             => Success(lds.toLinkDefinition(None), in)
                }
            }
        }
    }

    /**
     * Parses a link definition.
     */
    def linkDefinition:Parser[LinkDefinition] = p(lineParsers.linkDefinitionStart) into(maybeUrlInNextLine)

    /////////////////
    // XML blocks  //
    /////////////////
    /** The start of a verbatim XML chunk: any line starting directly with an XML element
     */
    def xmlChunkStart = p(lineParsers.xmlBlockStartLine)

    /** Parses any line that does not start with a closing XML element.
     */
    def notXmlChunkEnd = p(lineParsers.notXmlBlockEndLine)

    /** Parses a line beginning with a closing XML tag.
     */
    def xmlChunkEnd = p(lineParsers.xmlBlockEndLine)

    /** Very dumb parser for XML chunks.
     */
    def xmlChunk = xmlChunkStart ~ (notXmlChunkEnd*) ~ xmlChunkEnd ^^ {
        case s ~ ms ~ e => new XmlChunk(s + "\n" + ms.mkString("\n") + "\n" + e + "\n")
    }

    /** Parses Markdown Lines. Always succeeds.
     */
    def lineToken = Parser{ in =>
        if (in.atEnd) {
            Failure("End of Input.", in)
        } else {
            val line      = in.first
            (firstChar(line), indicatorChar(line)) match {
                case ('=', _) => p(lineParsers.setextHeader1)(in)
                case ('-', _) => p(lineParsers.setext2OrRulerOrUItem)(in)
                case ('#', _) => p(lineParsers.atxHeader)(in)
                case (_, '-') => p(lineParsers.rulerOrUItem)(in)
                case (_, '*') => p(lineParsers.rulerOrUItem)(in)
                case (_, '+') => p(lineParsers.uItemStartLine)(in)
                case (_, '>') => p(lineParsers.blockquoteLine)(in)
                case (_, n) if (n >= '0' && n <= '9') => p(lineParsers.oItemStartLine)(in)
                case (_, ' ') => p(lineParsers.emptyOrCode)(in)
                case (_, '\t')=> p(lineParsers.emptyOrCode)(in)
                case (_, '\n')=> p(lineParsers.emptyLine)(in)
                case (_, '`') => p(lineParsers.fencedCodeStartOrEnd)(in)
                case _        => p(lineParsers.otherLine)(in)
            }
        }
    } | p(lineParsers.otherLine) //this makes sure every line is consumed, even if our guess was no good

    /** Parses link definitions and verbatim xml blocks
     */
    def preprocessToken = Parser{ in =>
        if (in.atEnd) {
            Failure("End of Input.", in)
        } else {
            val line      = in.first
            (firstChar(line), indicatorChar(line)) match {
                //link definitions have absolute precedence
                case (_, '[') => linkDefinition(in)
                //then filter out xml blocks if allowed
                case ('<', _) if (allowXmlBlocks) => xmlChunk(in)
                //no token for preprocessing
                case _        => Failure("No preprocessing token.", in)
            }
        }
    }

    /** Parses tokens that may occur inside a block. Works like the normal token parser except that
     * it does not check for link definitions and verbatim XML.
     */
    def innerTokens(lookup:Map[String, LinkDefinition]):Parser[MarkdownLineReader] = phrase(lineToken *) ^^ {
        case ts => new MarkdownLineReader(ts, lookup)
    }

    /** Parses first level line tokens, i.e. Markdown lines, XML chunks and link definitions.
     */
    def tokens:Parser[MarkdownLineReader] = phrase((preprocessToken | lineToken) *) ^^ { case ts =>
        val lines = new ArrayBuffer[MarkdownLine]()
        val lookup = new HashMap[String, LinkDefinition]()
        for (t <- ts) { t match {
            case ld:LinkDefinition => lookup(ld.id) = ld
            case ml:MarkdownLine   => lines.append(ml)
        } }
        new MarkdownLineReader(lines.toList, lookup.toMap)
    }

    /** Simple preprocessing: split the input at each newline. These whole lines are then fed to
     * the actual Tokenizer.
     */
    def splitLines(s:String):List[String] = {
        def chopWindoze(line:String) = {
            if (line.endsWith("\r")) {
                line.substring(0, line.length-1)
            } else {
                line
            }
        }

        s.split('\n').map(chopWindoze(_)).toList
    }

    /** Turns a list of inner lines (the payloads of the lines making up the block)
     * into line tokens. Does not check for XML chunks or link definitions.
     */
    def innerTokenize(lines:List[String], lookup:Map[String, LinkDefinition])=
        innerTokens(lookup)(new LineReader(lines)) match {
            case Success(reader, _) => reader
            case n:NoSuccess        =>
                throw new IllegalStateException("Inner line Tokenizing failed. This is a bug. Message was: " + n.msg)
        }

    /** Tokenizes a whole Markdown document.
     */
    def tokenize(s:String):MarkdownLineReader = tokenize(splitLines(s))

    /** Tokenizes a preprocessed Markdown document.
     */
    def tokenize(lines:List[String]):MarkdownLineReader = tokenize(new LineReader(lines))

    /** Tokenizes preprocessed lines read from a line reader.
     */
    def tokenize(lines:Reader[String]):MarkdownLineReader = tokens(lines) match {
        case Success(reader, _) => reader
        case n:NoSuccess        =>
                throw new IllegalStateException("Tokenizing failed. This is a bug. Message was: " + n.msg)
    }
}
