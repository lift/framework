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

import scala.language.postfixOps

import collection.immutable.StringOps
import collection.mutable.ListBuffer
import xml.{Group, Node, Text, NodeSeq, Elem => XmlElem, TopScope, XML}
import xml.parsing.XhtmlParser
import util.parsing.combinator.{Parsers, RegexParsers}

/**
 * A parser for the markdown language.
 * Works on pre-parsed lines that can be created by a LineParser.
 */

trait BlockParsers extends Parsers {
    type Elem   = MarkdownLine
    //why does this not allow us to access the lookup map in the lookup parser?
    //override type Input  = MarkdownLineReader
    //hmm, compiler does not accept this, though MarkdownLineReader extends Reader[MarkdownLine]...

    /**
     * Used to define the output format of parsed blocks and whether verbatim xml blocks are allowed.
     */
    def deco():Decorator = Decorator

    /**
     * returns the current indentation string repeated the given number of levels
     */
    def indent(level:Int):String = deco.indentation * level

    private val tokenizer = new LineTokenizer()

    /** A markdown block element.
     */
    sealed abstract class MarkdownBlock extends InlineParsers{
        override def deco = BlockParsers.this.deco

        /** adds the resulting xhtml snippet to the given string builder
         */
        def addResult(level:Int, out:StringBuilder):Unit
        /** returns the resulting xhtml snippet as a string
         */
        def result():String = {
            val sb = new StringBuilder
            addResult(0, sb)
            sb.toString
        }
    }



    //////////////////////////
    // non-recursive blocks //
    //////////////////////////

    /**:?
     * Represents a block of verbatim xml
     */
    class VerbatimXml(line:XmlChunk) extends MarkdownBlock {
        def addResult(level:Int, out:StringBuilder) {out.append(line.content)}
    }

    /**
     * Represents a horizontal ruler
     */
    object Ruler extends MarkdownBlock {
        def addResult(level:Int, out:StringBuilder) {out.append(indent(level)).append(deco.decorateRuler)}
    }

    /**
     * Represents a header
     */
    case class Header(content:String, headerLevel:Int, lookup:Map[String, LinkDefinition]) extends MarkdownBlock{
        def addResult(level:Int, out:StringBuilder) {
            out.append(indent(level)).append(deco.decorateHeaderOpen(headerLevel))
               .append(applyInline(content, lookup))
               .append(indent(level)).append(deco.decorateHeaderClose(headerLevel))
        }
    }

    /**
     * Represents a block of verbatim qouted code
     */
    class CodeBlock(lines:List[MarkdownLine]) extends MarkdownBlock{
        def addResult(level:Int, out:StringBuilder) {
            out.append(indent(level)).append(deco.decorateCodeBlockOpen)
            for (line <- lines) {
                val escaped = escapeXml(line.payload)
                out.append(escaped).append('\n')
                //out.append(line.content)
            }
            out.append(indent(level)).append(deco.decorateCodeBlockClose)
        }
    }
    
    class FencedCodeBlock(language:String, lines:List[MarkdownLine]) extends MarkdownBlock{
        def addResult(level:Int, out:StringBuilder) {
            out.append(indent(level)).append(deco.decorateCodeBlockOpen)
            for (line <- lines) {
                val escaped = escapeXml(line.fullLine)
                out.append(escaped).append('\n')
                //out.append(line.content)
            }
            out.append(indent(level)).append(deco.decorateCodeBlockClose)
        }
    }

    /**
     * Represents a paragraph of text
     */
    class Paragraph(lines:List[MarkdownLine], lookup:Map[String, LinkDefinition])
            extends MarkdownBlock{

        def addResult(level:Int, out:StringBuilder) {
            out.append(indent(level)).append(deco.decorateParagraphOpen)
            addResultPlain(level, out)
            out.append(indent(level)).append(deco.decorateParagraphClose)
        }

        /**
         * Adds the result without any decoration, (no wrapping tags)
         * Used for building list items that don't have their content wrappend in paragraphs
         */
        def addResultPlain(level:Int, out:StringBuilder) {

            val temp = new StringBuilder()
            lines.foreach(line => temp.append(indent(level)).append(line.payload).append('\n'))
            val result = applyInline(temp.toString, lookup)
            out.append(result)

            //lines.foreach(line => out.append(indent(level)).append(escapeXml(line.content)))

            //drop last newline so paragraph closing tag ends the line
            if (!out.isEmpty && out.charAt(out.length-1) == '\n') out.deleteCharAt(out.length-1)
        }
    }

    //////////////////////
    // recursive blocks //
    //////////////////////

    /**
     * Represents a quoted text block. Text in the block is recursively evaluated.
     */
    class Blockquote(lines:List[MarkdownLine], lookup:Map[String, LinkDefinition])
            extends MarkdownBlock {
        def addResult(level:Int, out:StringBuilder) {
            //the block parser needs to recurse:
            val innerLines = lines.map(line => line.payload)
            val reader = BlockParsers.this.tokenizer.innerTokenize(innerLines, lookup)
            //now apply the normal markdown parser to the new content
            val innerBlocks = BlockParsers.this.applyBlocks(reader)
            //wrap the resulting blocks in blockquote tags
            out.append(indent(level)).append(deco.decorateBlockQuoteOpen)
            innerBlocks.foreach(block => block.addResult(level+1, out))
            out.append(indent(level)).append(deco.decorateBlockQuoteClose)
        }
    }

    /**
     * Helper class to build lists. Allows easy checking if an item ends with empty lines and
     * recursively builds the content of an item.
     */
    class ListItem(val lines:List[MarkdownLine], lookup:Map[String, LinkDefinition]) extends LineParsers {
        def endsWithNewline = lines.size > 1 && (lines.last.isInstanceOf[EmptyLine])

        def addResult(level:Int, out:StringBuilder, paragraph_? : Boolean) {
            out.append(indent(level)).append(deco.decorateItemOpen)
            //the block parser needs to recurse:
            val innerLines = lines.map(line => line.payload)
            val reader = BlockParsers.this.tokenizer.innerTokenize(innerLines, lookup)
            //now apply the normal markdown parser to the new content
            val innerBlocks = BlockParsers.this.applyBlocks(reader)
            innerBlocks match {
                case (p:Paragraph) :: Nil if (!paragraph_?) => p.addResultPlain(level+1, out)
                case _                                      => innerBlocks.foreach(block => block.addResult(level+1, out))
            }
            out.append(indent(level)).append(deco.decorateItemClose)
        }
    }

    /**
     * Base class for ordered and unordered lists, allows for correct handling of paragraphs in lists.
     */
    abstract class ListBlock (items:List[ListItem]) extends MarkdownBlock {
        /**
         * This method recursively goes through the given list and adds the items contents.
         * It checks the previous item if it ends with empty lines. If it does, it signals the
         * current item to create paragraphs. In order for this method to work it has to be
         * called with the first item prepended twice in front of the list. So if the list is
         * a::b::c, call this method with a::a::b::c
         */
        protected def addResult(level:Int, out:StringBuilder, list:List[ListItem]):Unit = list match{
            case last::current::rest => {
                current.addResult(level + 1, out, last.endsWithNewline)
                addResult(level, out, current::rest)
            }
            case _                   => {}//end of recursion, list with one item or less
        }

        /**
         * calls recursive handling of nested items
         */
        def addResult(level:Int, out:StringBuilder) {
            addResult(level, out, items.head::items)
        }
    }

    /**
     * An ordered (i.e. numbered) list of items.
     */
    class OList (items:List[ListItem]) extends ListBlock(items) {
        override def addResult(level:Int, out:StringBuilder) {
            out.append(indent(level)).append(deco.decorateOListOpen)
            super.addResult(level, out)
            out.append(indent(level)).append(deco.decorateOListClose)
        }
    }

    /**
     * An unordered list of items.
     */
    class UList (items:List[ListItem]) extends ListBlock(items) {
        override def addResult(level:Int, out:StringBuilder) {
            out.append(indent(level)).append(deco.decorateUListOpen)
            super.addResult(level, out)
            out.append(indent(level)).append(deco.decorateUListClose)
        }
    }


    /////////////////////////////////////////////////////////////
    //////////////// helpers                /////////////////////
    /////////////////////////////////////////////////////////////

    /**
     * Parses a line of the given type T
     */
    def line[T](c:Class[T]):Parser[T] = Parser {in =>
        if (in.first.getClass == c) Success(in.first.asInstanceOf[T], in.rest)
        else                        Failure("Not a fitting line.", in)
    }
    
    /** 
     * Parses a line of any type *but* T
     */
    def notLine[T](c:Class[T]):Parser[MarkdownLine] = Parser {in =>
        if      (in.atEnd)               Failure("At end of input.", in)
        else if (in.first.getClass == c) Failure("Not a fitting line.", in)
        else                             Success(in.first, in.rest)
    }

    /**
     * Parses any line.
     */
    def anyLine:Parser[MarkdownLine] = Parser {in =>
        if (in.atEnd) Failure("End of input reached.", in)
        else Success(in.first, in.rest)
    }

    def emptyLine:Parser[EmptyLine] = line(classOf[EmptyLine])

    /**accepts zero or more empty lines
     */
    def optEmptyLines:Parser[List[MarkdownLine]] = emptyLine*

    /** accepts one or more empty lines
     */
    def emptyLines:Parser[List[MarkdownLine]] = emptyLine+

    /** returns the current link lookup from the reader
     * always succeeds, never consumes input
     */
    def lookup:Parser[Map[String, LinkDefinition]] = Parser { in =>
        //why is the instanceof necessary? re-declaring type Input above does not change anything :(
        Success(in.asInstanceOf[MarkdownLineReader].lookup, in)
    }

    ///////////////////
    // Block parsers //
    ///////////////////

    def atxHeader:Parser[Header] = line(classOf[AtxHeaderLine]) ~ lookup ^^ {
        case l ~ lu => new Header(l.trimHashes, l.headerLevel, lu)
    }

    def setExtHeader:Parser[Header] =
        not(emptyLine) ~> anyLine ~ line(classOf[SetExtHeaderLine]) ~ lookup ^^
                {case l ~ setext ~ lu => new Header(l.fullLine.trim, setext.headerLevel, lu)}

    /** parses a horizontal ruler
     */
    def ruler:Parser[MarkdownBlock] = (line(classOf[RulerLine]) | line(classOf[SetExtHeaderLine])) ^^^ {Ruler}

    /** parses a verbatim xml block
     */
    def verbatimXml:Parser[VerbatimXml] = line(classOf[XmlChunk]) ^^ {new VerbatimXml(_)}

    /** parses a code block
     */
    def codeBlock:Parser[CodeBlock] = line(classOf[CodeLine]) ~ ((optEmptyLines ~ line(classOf[CodeLine]))*) ^^ {
        case l ~ pairs => new CodeBlock( l :: pairs.map({case (a~b) => a++List(b)}).flatten )
    }
    
    /**
     * Parses a fenced code block: a line starting a fenced code block with 
     * "```", followed by any lines that do not stop it, optionally followed
     * by the ending line. Optionally parsing the stopping line causes the 
     * code block to extend to the end of the document. (This is the github 
     * behavior, where omitting the line closing the code block causes the 
     * block to extend to the end of the document as well)
     */
    def fencedCodeBlock:Parser[FencedCodeBlock] = 
          (line(classOf[ExtendedFencedCode])|line(classOf[FencedCode])) ~
          (notLine(classOf[FencedCode])*) ~                                            
          opt(line(classOf[FencedCode]))^^ {
        case (start:ExtendedFencedCode) ~ lines ~ _ => new FencedCodeBlock(start.languageFormat, lines)
        case _ ~ lines ~ _ => new FencedCodeBlock("", lines)
    }
      
                                            //line(classOf[FencedCodeStart]) ~ 
                                            //((not(line(classOf[FencedCodeEnd]))*) ~ 
                                            //opt(line(classOf[FencedCodeEnd])) ^^ {
    //    case start ~ lines ~ end => new CodeBlock(lines.map(_.fullLine))
    //}


    /** a consecutive block of paragraph lines
     *  returns the content of the matched block wrapped in <p> tags
     */
    def paragraph:Parser[Paragraph] = lookup ~ (line(classOf[OtherLine])+) ^^ {case lu ~ ls => new Paragraph(ls, lu)}

    /**
     * Parses a blockquote fragment: a block starting with a blockquote line followed
     * by more blockquote or paragraph lines, ends optionally with empty lines 
     */
    def blockquoteFragment:Parser[List[MarkdownLine]] =
        line(classOf[BlockQuoteLine]) ~ ((line(classOf[BlockQuoteLine]) | line(classOf[OtherLine]))*) ~ (optEmptyLines) ^^ {
            case l ~ ls ~ e => (l :: ls ++ e)
        }

    /**
     * Parses a quoted block. A quoted block starts with a line starting with "> "
     * followed by more blockquote lines, paragraph lines following blockqoute lines
     * and may be interspersed with empty lines
     */
    def blockquote:Parser[Blockquote] = lookup ~ (blockquoteFragment+) ^^ {
        case lu ~ fs => new Blockquote(fs.flatten, lu)
    }


    /**
     * parses a list of lines that may make up the body of a list item
     */
    def itemLines:Parser[List[MarkdownLine]] = ((line(classOf[CodeLine])|line(classOf[OtherLine]))*)

    /**
     * The continuation of a list item:
     * A line indented by four spaces or a tab (a continuation line), followed by more continuation or paragraph
     * lines followed by empty lines
     */
    def itemContinuation:Parser[List[MarkdownLine]] =
        optEmptyLines ~ line(classOf[CodeLine]) ~ itemLines ^^ {
            case e ~ c ~ cs => e ++ (c :: cs)
        }

    /**parses an item in an unsorted list
     */
    def uItem:Parser[ListItem] = lookup ~ line(classOf[UItemStartLine]) ~ itemLines ~ (itemContinuation*) ~ optEmptyLines ^^ {
        case lu ~ s ~ ls ~ cs ~ e => new ListItem(s :: ls ++ cs.flatten ++ e, lu)
    }

    /**parses an item in a sorted list
     */
    def oItem:Parser[ListItem] = lookup ~ line(classOf[OItemStartLine]) ~ itemLines ~ (itemContinuation*) ~ optEmptyLines ^^ {
        case lu ~ s ~ ls ~ cs ~ e => new ListItem(s :: ls ++ cs.flatten ++ e, lu)
    }

    /** parses an unordered list
     */
    def uList:Parser[UList] = (uItem+) ^^ {new UList(_)}

    /** parses an ordered list
     */
    def oList:Parser[OList] = (oItem+) ^^ {new OList(_)}


    ///////////////////////////////////////////////////////////////
    /////////////////// high level processing /////////////////////
    ///////////////////////////////////////////////////////////////

    /**
     * parses first level blocks (all blocks, including xml)
     */
    def outerBlock:Parser[MarkdownBlock] = (verbatimXml <~ optEmptyLines) | innerBlock

    /**
     * speed up block processing by looking ahead
     */
    def fastBlock:Parser[MarkdownBlock] = Parser { in =>
        if (in.atEnd) {
            Failure("End of Input.", in)
        } else {
            in.first match {
                case l:AtxHeaderLine => atxHeader(in)
                case l:RulerLine => ruler(in)
                //setext headers have been processed before we are called, so this is safe
                case l:SetExtHeaderLine => ruler(in)
                case l:CodeLine => codeBlock(in)
                case l:ExtendedFencedCode => fencedCodeBlock(in)
                case l:FencedCode => fencedCodeBlock(in)
                case l:BlockQuoteLine => blockquote(in)
                case l:OItemStartLine => oList(in)
                case l:UItemStartLine => uList(in)
                case _ => paragraph(in)
            }
        }
    }

    /**
     * parses inner blocks (everything excluding xml)
     */
    def innerBlock:Parser[MarkdownBlock] = (setExtHeader | fastBlock) <~ optEmptyLines

    /**
     * a markdown parser
     */
    def markdown:Parser[List[MarkdownBlock]] = optEmptyLines ~> (outerBlock*)

    /** Generic apply method to run one of our pasers on the given input.
     */
    def apply[T](p:Parser[T], in:MarkdownLineReader):T = {
         phrase(p)(in) match {
            case Success(t, _) => t
            case e: NoSuccess  => throw new IllegalArgumentException("Could not parse '" + in + "': " + e)
        }
    }

    /** parses all blocks from the given reader
     */
    def applyBlocks(in:MarkdownLineReader):List[MarkdownBlock] = apply((optEmptyLines ~> (innerBlock*)), in)

    /** Generic apply method to test a single parser
     */
    def apply[T](p:Parser[T], list:List[MarkdownLine]):T = apply(p, new MarkdownLineReader(list))

    /** Parses the given input as a markdown document and returns the string result
     */
    def apply(in:MarkdownLineReader):String = {
        phrase(markdown)(in) match {
            case Success(bs, _) => {
                val builder = new StringBuilder()
                bs.foreach(block => block.addResult(0, builder))
                builder.toString
            }
            case e: NoSuccess => throw new IllegalArgumentException("Could not parse " + in + ": " + e)
        }
    }
}
