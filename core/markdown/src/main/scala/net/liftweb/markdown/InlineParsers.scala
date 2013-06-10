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

/**
 * A parser for inline markdown, markdown escapes and XML escapes.
 * This is used by the result classes of the block parsers to handle
 * Markdown within a block.
 */

trait InlineParsers extends BaseParsers {

    /**
     * Defines how the output is formatted and whether inline xml elements are allowed.
     */
    def deco():Decorator = Decorator

    /////////////////////////////////////
    // Types we use for inline parsing //
    /////////////////////////////////////

    /**
     * Defines a lookup map for link definitions.
     */
    type LinkMap       = Map[String, LinkDefinition]

    /**
     * A set of tags we have already created. Used to prevent nesting a link in a link or an emphasis in an emphasis.
     */
    type VisitedTags   = Set[String]

    /**
     *  Keeps track of visited tags and provides a lookup for link ids.
     */
    case class InlineContext(val map:LinkMap, val tags:VisitedTags) {
        def this(m:LinkMap) = this(m, Set())
        def this()          = this(Map())
        def addTag(tag:String) = new InlineContext(map, tags + tag)
    }

    /** This array is used as a lookup for mapping markdown escapes
     * to the resulting char (if necessary already escaped for XML)
     * Disgusting, I know, but this is such an often called operation
     * that this is the fastest way to do it, even in the year 2010.
     */
    private val escapableMarkdownChars = new Array[String](127)
    escapableMarkdownChars('\\') = "\\"
    escapableMarkdownChars('`')  = "`"
    escapableMarkdownChars('*')  = "*"
    escapableMarkdownChars('_')  = "_"
    escapableMarkdownChars('{')  = "{"
    escapableMarkdownChars('}')  = "}"
    escapableMarkdownChars('[')  = "["
    escapableMarkdownChars(']')  = "]"
    escapableMarkdownChars('(')  = "("
    escapableMarkdownChars(')')  = ")"
    escapableMarkdownChars('#')  = "#"
    escapableMarkdownChars('+')  = "+"
    escapableMarkdownChars('-')  = "-"
    escapableMarkdownChars('=')  = "="
    escapableMarkdownChars('>')  = "&gt;"
    escapableMarkdownChars('.')  = "."
    escapableMarkdownChars('!')  = "!"

    /**
     * Parses markdown text up to any of the chars defined in the given map.
     * used to quickly escape any text between special inline markdown like
     * emphasis.
     */
    def markdownText(special:Set[Char], markdownEscapes:Boolean) = Parser{ in =>
        if (in.atEnd) {
            Failure("End of input.", in)
        } else {
            var start = in.offset
            var i = in.offset
            val s = in.source
            val end = s.length
            val result = new StringBuffer()
            //process chars until we hit a special char or the end
            while (i<end && !special.contains(s.charAt(i))) {
                val c = s.charAt(i)
                val xmlEscape = escapeFastForXml(c)
                if (markdownEscapes && c == '\\' && i+1 < end && escapableMarkdownChars(s.charAt(i+1))!=null) {
                    result.append(s.subSequence(start, i).toString)
                    result.append(escapableMarkdownChars(s.charAt(i+1)))
                    i += 2
                    start = i
                } else if (xmlEscape != null) {
                    result.append(s.subSequence(start, i).toString)
                    result.append(xmlEscape)
                    i += 1
                    start = i
                } else {
                    i += 1
                }
            }
            if (start != i) result.append(s.subSequence(start, i).toString)
            if (result.length == 0) Failure("No text consumed.", in)
            else                    Success(result.toString(), in.drop(i - in.offset))
        }
    }

    /**
     *  all markdown inline element parsers or'ed together
     */
    //this was SLOW AS HELL
    //def elementParsers(ctx:InlineContext) = Parser { inline =>
        //markdownEscape | br | code | xmlTag | //simple inline
        //a(ctx) | strong(ctx) | em(ctx) | fastA(ctx) | refA(ctx) | img(ctx) //recursive inline

    //}
    /* explicit match is faster than the map lookup
    private val elementParserLookup:Map[Char,(InlineContext=>Parser[String])] = Map(
        '\\' -> (ctx => aChar), ' ' -> (ctx => br), '`' -> (ctx => code), '<' -> (ctx => xmlTag | fastA(ctx)),
        '[' -> (ctx => a(ctx) | refA(ctx)), '*' -> (ctx => spanAsterisk(ctx)), '_' -> (ctx => spanUnderscore(ctx)),
        '!' -> (ctx => img(ctx))
    )
    */

    //TODO:better handling of "  \n" here. Stopping at every space costs us 20% time!
    /** Chars that may indicate the start of a special Markdown inline sequence.
     */
    val specialInlineChars = Set(' ', '`', '<', '[', '*', '_', '!')
    /** Chars that may indicate the start of a special markdown inline sequence or the end of a link text.
     */
    val specialLinkInlineChars = specialInlineChars + ']'

    /** Hand rolled parser that parses a chunk of special inline markdown (like links or emphasis)
     * based on a one char lookahead.
     */
    def elementParsers(ctx:InlineContext) = Parser{ in =>
        if (in.atEnd) {
            Failure("End of Input Reached", in)
        } else {
            in.first match {
                case ' ' => br(in)
                case '`' => code(in)
                case '<' => (xmlTag | fastLink(ctx))(in)
                case '[' => link(ctx)(in)
                case '*' => spanAsterisk(ctx)(in)
                case '_' => spanUnderscore(ctx)(in)
                case '!' => img(ctx)(in)
                case _   => Failure("Lookahead does not start inline element.", in)
            }
        }
    }

    /** Parses a single inline token. Either a span element or a chunk of text.
     */
    def oneInline(ctx:InlineContext):Parser[String] =
        markdownText(specialInlineChars, true) | elementParsers(ctx) | aChar

    /** Parser for inline markdown, always consumes all input, returns the resulting HTML.
     */
    def inline(m:LinkMap):Parser[String] = (oneInline(new InlineContext(m))*) ^^ {_.mkString}



    ///////////////////////////////////////////////////////////
    //   Inline Elements:                                    //
    //   br,code,xml tag,fast link,link,image,emphasis,strong, text chunk  //
    ///////////////////////////////////////////////////////////

    /** Parses two spaces at the end of a line to a manual break (<br/>)
     */
    val br:Parser[String] = ("  \n") ^^^ {deco.decorateBreak() + "\n"}


    /** Parses an inline code element.
     * An inline code element is surrounded by single backticks ("`")
     * or double backticks ("``").
     */
    val code:Parser[String] = ((("``" ~> ((not("``")~> aChar)+) <~ "``")^^{_.mkString}) |
                               ('`' ~> markdownText(Set('`'), false) <~ '`') ) ^^ {
        c => deco.decorateCode(c.mkString)
    }


    /** Parses any xml tag and escapes attribute values.
     */
    val xmlTag:Parser[String] = if (deco.allowVerbatimXml) (xmlEndTag | xmlStartOrEmptyTag)
                                else failure("Inline XML processing disabled.")


    /** A shortcut markdown link of the form <http://example.com>
     */
    def fastLink(ctx:InlineContext):Parser[String] =
        if (ctx.tags.contains("a")){
            failure("Cannot nest a link in a link.")
        } else {
            elem('<') ~> markdownText(Set('>',' ', '<', '\n'), true) <~ '>' ^^ { u => deco.decorateLink(u, u, None) }
        }

    /** A link started by square brackets, either a reference or a a link with the full URL.
     */
    def link(ctx:InlineContext):Parser[String] = fullLink(ctx) | referenceLink(ctx)

    /** A markdown link with the full url given.
     */
    def fullLink(ctx:InlineContext):Parser[String] =
        if (ctx.tags.contains("a")){
            failure("Cannot nest a link in a link.")
        } else {
            '[' ~> linkInline(ctx.addTag("a")) ~ ("](" ~ ows) ~ url ~ ows ~ title <~ (ows ~ ')') ^^ {
                case txt ~ _ ~ u ~ _ ~ ttl => deco.decorateLink(txt, u, ttl)
            }
        }

    /** A markdown link which references an url by id.
     */
    def referenceLink(ctx:InlineContext):Parser[String] =
        if (ctx.tags.contains("a")){
            failure("Cannot nest a link in a link.")
        } else {
            ref(ctx.addTag("a")) ^^ {
                case (LinkDefinition(_, u, ttl), txt) => deco.decorateLink(txt, u, ttl)
            }
        }

    /** Inline markdown in a link. Like normal inline stuff but stops when it reaches a closing square bracket.
     */
    def linkInline(ctx:InlineContext):Parser[String] = //( (not(']') ~> oneInline(ctx.addTag("a")))* ) ^^ {_.mkString}
        ((markdownText(specialLinkInlineChars, true) | elementParsers(ctx) | ((not(']') ~> aChar)))*) ^^ {_.mkString}

    /** We parse everything as a link/img url until we hit whitespace or a closing brace.
     */
    val url:Parser[String] = markdownText(Set(')', ' ', '\t'), true)

    /** A title is everything in quotation marks. We allow even quotation marks in quotation marks.
     * We look ahead if we hit the closing brace after the quotation marks to detect if the title
     * ends or not.
     */
    val title:Parser[Option[String]] = opt('"' ~> ((markdownText(Set('"'),true) ~ opt(not('"'~ows~')') ~> aChar))*) <~ '"') ^^ {
            case None         => None
            case Some(chunks) => {
                val result = new StringBuilder()
                for (chunk <- chunks) { chunk match {
                    case (text) ~ None    => result.append(text)
                    case (text) ~ Some(s) => result.append(text).append(s)
                } }
                Some(result.toString)
            }
        }

    /** Plaintext variant to refInline. Escapable text until a square bracket is hit.
     */
    val refText:Parser[String] = markdownText(Set(']'), true)

    /** Parses an id reference. (Any text that is not a square bracket)
     *  Succeeds only if the parsed id is found in the given lookup.
     * Returns the found link definition and the matched text.
     */
    def idReference(ctx:InlineContext):Parser[(String, LinkDefinition)] =
        guard(acceptMatch(ctx.map)(refText ^^ (_.trim.toLowerCase)))  ~  refText ^^ {case ld ~ t => (t, ld)}
    /**
     * A markdown reference of the form [text][id], [idText][] or [idText]
     * Parser returns a tuple with the link definition first and the text to display second.
     */
    def ref(ctx:InlineContext):Parser[(LinkDefinition, String)] =
        ('[' ~> linkInline(ctx)    ~ (']' ~ opt(' ') ~ '[') ~ idReference(ctx) <~ ']'  ^^ {
           case t                 ~ dummy                  ~ pair                     =>  (pair._2, t)} ) |
        ('[' ~> idReference(ctx) <~ (']' ~ opt(opt(' ') ~ '['  ~ ows               ~ ']')) ^^ {
           case (t, ld)                                                               =>  (ld, t)} )

    /**
     * Parses either a referenced or a directly defined image.
     */
    def img(ctx:InlineContext):Parser[String] = elem('!') ~> (directImg | refImg(ctx))

    /** An image with an explicit path.
     */
    val directImg:Parser[String] =
        elem('[') ~> refText ~ ("](" ~ ows) ~ url ~ ows ~ title <~ (ows ~ ')') ^^ {
            case altText ~ _ ~ path ~ _ ~ ttl => deco.decorateImg(altText, path, ttl)
        }
    /**
     * Parses a referenced image.
     */
    def refImg(ctx:InlineContext):Parser[String] = ref(ctx) ^^ {
        case (LinkDefinition(_, u, ttl), alt) => deco.decorateImg(alt, u, ttl)
    }

    /** Parses inline in a span element like bold or emphasis or link up until the given end marker
     */
    def spanInline(end:Parser[Any], ctx:InlineContext):Parser[String] =
        (markdownText(specialInlineChars, true) | elementParsers(ctx) | (not(end) ~> aChar)) ^^ {_.mkString}

    /** Parses a span element like __foo__ or *bar*
     */
    def span(limiter:String, ctx:InlineContext):Parser[String] =
        (limiter~not(ws))~>
                (spanInline(  (not(lookbehind(Set(' ', '\t', '\n'))) ~ limiter),    ctx)+)  <~
         limiter ^^ {
            _.mkString
        }

    /** Either an emphasis or a strong text wrapped in asterisks.
     */
    def spanAsterisk  (ctx:InlineContext) = strongAsterisk(ctx)   | emAsterisk(ctx)

    /** Either an emphasis or strong text wrapped in underscores.
     */
    def spanUnderscore(ctx:InlineContext) = strongUnderscore(ctx) | emUnderscore(ctx)

    /**Parses emphasized text wrapped in asterisks: *foo*
     */
    def emAsterisk(ctx:InlineContext):Parser[String] =
        if (ctx.tags.contains("em")) {
            failure("Cannot nest emphasis.")
        } else {
            span("*", ctx.addTag("em")) ^^ { deco.decorateEmphasis(_) }
        }


    /**Parses emphasized text wrapped in underscores: _foo_
     */
    def emUnderscore(ctx:InlineContext):Parser[String] =
        if (ctx.tags.contains("em")) {
            failure("Cannot nest emphasis.")
        } else {
            span("_", ctx.addTag("em")) ^^ { deco.decorateEmphasis(_) }
        }

    /**Parses strong text in asterisks: **foo**
     */
    def strongAsterisk(ctx:InlineContext):Parser[String] =
        if (ctx.tags.contains("strong")) {
            failure("Cannot nest strong text.")
        } else {
            span("**", ctx.addTag("strong")) ^^ { deco.decorateStrong(_) }
        }

    /**Parses strong text in underscores: __foo__
     */
    def strongUnderscore(ctx:InlineContext):Parser[String] =
        if (ctx.tags.contains("strong")) {
            failure("Cannot nest strong text.")
        } else {
            span("__", ctx.addTag("strong")) ^^ { deco.decorateStrong(_) }
        }


    /**
     * Runs the inline parser on the given input and returns the result
     */
    def applyInline(s:String, m:LinkMap)  = apply(inline(m), s)

    /**
     * Escapes the given string so it it can be embedded in xml.
     * Markdown escapes are not processed.
     */
    def escapeXml(s:String) = {
        var i = 0
        val end = s.length
        val result = new StringBuffer()
        //process chars until we hit a special char or the end
        while (i<end) {
            val out = s.charAt(i)
            //if it is a an xml reserved char, xml escape it, else just add it
            val xmlEscape = escapeFastForXml(out)
            if (xmlEscape != null) result.append(xmlEscape)
            else                   result.append(out)
            //advance a char
            i += 1
        }
        result.toString
    }
}