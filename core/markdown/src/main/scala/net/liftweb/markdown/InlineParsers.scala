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
                } else if (xmlEscape != null && c == '&' && checkForSemi(i, s, end)) {
                  i += 1
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

  private lazy val entList = List(("quot",34), ("amp",38), ("lt",60), ("gt",62), ("nbsp",160), ("iexcl",161), ("cent",162), ("pound",163), ("curren",164), ("yen",165),
    ("euro",8364), ("brvbar",166), ("sect",167), ("uml",168), ("copy",169), ("ordf",170), ("laquo",171), ("shy",173), ("reg",174), ("trade",8482),
    ("macr",175), ("deg",176), ("plusmn",177), ("sup2",178), ("sup3",179), ("acute",180), ("micro",181), ("para",182), ("middot",183), ("cedil",184),
    ("sup1",185), ("ordm",186), ("raquo",187), ("frac14",188), ("frac12",189), ("frac34",190), ("iquest",191), ("times",215), ("divide",247),
    ("Agrave",192), ("Aacute",193), ("Acirc",194), ("Atilde",195), ("Auml",196), ("Aring",197), ("AElig",198), ("Ccedil",199), ("Egrave",200),
    ("Eacute",201), ("Ecirc",202), ("Euml",203), ("Igrave",204), ("Iacute",205), ("Icirc",206), ("Iuml",207), ("ETH",208), ("Ntilde",209),
    ("Ograve",210), ("Oacute",211), ("Ocirc",212), ("Otilde",213), ("Ouml",214), ("Oslash",216), ("Ugrave",217), ("Uacute",218), ("Ucirc",219),
    ("Uuml",220), ("Yacute",221), ("THORN",222), ("szlig",223), ("agrave",224), ("aacute",225), ("acirc",226), ("atilde",227), ("auml",228),
    ("aring",229), ("aelig",230), ("ccedil",231), ("egrave",232), ("eacute",233), ("ecirc",234), ("euml",235), ("igrave",236), ("iacute",237),
    ("icirc",238), ("iuml",239), ("eth",240), ("ntilde",241), ("ograve",242), ("oacute",243), ("ocirc",244), ("otilde",245), ("ouml",246),
    ("oslash",248), ("ugrave",249), ("uacute",250), ("ucirc",251), ("uuml",252), ("yacute",253), ("thorn",254), ("yuml",255), ("OElig",338),
    ("oelig",339), ("Scaron",352), ("scaron",353), ("Yuml",376), ("circ",710), ("ensp",8194), ("emsp",8195), ("zwnj",204), ("zwj",8205), ("lrm",8206),
    ("rlm",8207), ("ndash",8211), ("mdash",8212), ("lsquo",8216), ("rsquo",8217), ("sbquo",8218), ("ldquo",8220), ("rdquo",8221), ("bdquo",8222),
    ("dagger",8224), ("Dagger",8225), ("permil",8240), ("lsaquo",8249), ("rsaquo",8250), ("fnof",402), ("bull",8226), ("hellip",8230), ("prime",8242),
    ("Prime",8243), ("oline",8254), ("frasl",8260), ("weierp",8472), ("image",8465), ("real",8476), ("alefsym",8501), ("larr",8592), ("uarr",8593),
    ("rarr",8594), ("darr",8495), ("harr",8596), ("crarr",8629), ("lArr",8656), ("uArr",8657), ("rArr",8658), ("dArr",8659), ("hArr",8660),
    ("forall",8704), ("part",8706), ("exist",8707), ("empty",8709), ("nabla",8711), ("isin",8712), ("notin",8713), ("ni",8715), ("prod",8719),
    ("sum",8721), ("minus",8722), ("lowast",8727), ("radic",8730), ("prop",8733), ("infin",8734), ("ang",8736), ("and",8743), ("or",8744),
    ("cap",8745), ("cup",8746), ("int",8747), ("there4",8756), ("sim",8764), ("cong",8773), ("asymp",8776), ("ne",8800), ("equiv",8801), ("le",8804),
    ("ge",8805), ("sub",8834), ("sup",8835), ("nsub",8836), ("sube",8838), ("supe",8839), ("oplus",8853), ("otimes",8855), ("perp",8869), ("sdot",8901),
    ("lceil",8968), ("rceil",8969), ("lfloor",8970), ("rfloor",8971), ("lang",9001), ("rang",9002), ("loz",9674), ("spades",9824), ("clubs",9827),
    ("hearts",9829), ("diams",9830), ("Alpha",913), ("Beta",914), ("Gamma",915), ("Delta",916), ("Epsilon",917), ("Zeta",918), ("Eta",919),
    ("Theta",920), ("Iota",921), ("Kappa",922), ("Lambda",923), ("Mu",924), ("Nu",925), ("Xi",926), ("Omicron",927), ("Pi",928), ("Rho",929),
    ("Sigma",931), ("Tau",932), ("Upsilon",933), ("Phi",934), ("Chi",935), ("Psi",936), ("Omega",937), ("alpha",945), ("beta",946), ("gamma",947),
    ("delta",948), ("epsilon",949), ("zeta",950), ("eta",951), ("theta",952), ("iota",953), ("kappa",954), ("lambda",955), ("mu",956), ("nu",957),
    ("xi",958), ("omicron",959), ("pi",960), ("rho",961), ("sigmaf",962), ("sigma",963), ("tau",964), ("upsilon",965), ("phi",966), ("chi",967),
    ("psi",968), ("omega",969), ("thetasym",977), ("upsih",978), ("piv",982))

  private lazy val validEntitySet = Set(entList.map(_._1) :_*)

  private def checkForSemi(i: Int, s: CharSequence, end: Int): Boolean = {
    var pos = i + 1
    val last = i + 10
    val sb = new StringBuffer(20)
    while (pos < end && pos < last) {
      s.charAt(pos) match {
        case ';' if pos > i + 1 =>
          return validEntitySet.contains(sb.toString)
        case c if c == '#' || Character.isLetter(c) || Character.isDigit(c) => sb.append(c)
        case _ => return false
      }
      pos += 1
    }

    false
  }
}
