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

import util.parsing.json.Parser
import util.parsing.combinator.RegexParsers
import collection.SortedMap

/**
 * Basic parsers for Markdown Source.
 * Provides general, small parsers that are used by other parsers.
 * Also contains parsers for XML elements.
 */

trait BaseParsers extends RegexParsers {

    /////////////////////////////////////////
    // Basic parsers used by other parsers //
    /////////////////////////////////////////

    /**
     * Whitespace is sometimes important in markdown parsing,
     * we handle it manually. So this returns false.
     */
    override def skipWhitespace = false

    /** accepts one or more spaces or tabs
     * returns the matched whitespace
     */
    def ws:Parser[String] = """( |\t|\v)+""".r

    /** accepts zero or more spaces or tabs
     * returns the matched whitespace
     */
    def ows:Parser[String] = Parser{in =>
        if (in.atEnd) Success("test", in)
        else {
            var i = in.offset
            val s = in.source
            val end = s.length
            //process chars as long as it is whitespace
            while (i<end && (s.charAt(i)==' ' || s.charAt(i)=='\t')) {
                //advance a char
                i += 1
            }
            Success(s.subSequence(in.offset, i).toString, in.drop(i - in.offset))
        }
    }

    /** Accepts a unix newline and returns a string containing a single newline.
     */
    def nl:Parser[String] = '\n' ^^^ {"\n"}

    /**
     * Matches everything in the parsed string up to the end.
     * Also matches the empty String. Returns the matched String.
     */
    def rest:Parser[String] = Parser { in =>
        if (in.atEnd) {
            Success("", in)
        } else {
            val source = in.source
            val offset = in.offset
            Success(source.subSequence(offset, source.length).toString, in.drop(source.length-offset))
        }
    }

    /**
     * Matches exactly one char, no matter which.
     * This differs from "elem" as it returns a string consisting of that char.
     */
    def any:Parser[String] = Parser{ in =>
        if (in.atEnd) Failure("End of input reached", in)
        else Success(in.first.toString, in.rest)
    }

    /**
     * Matches one of the chars in the given set.
     * Returns a string with the matched char.
     */
    def oneOf(lookup:Set[Char]):Parser[String] = Parser{ in =>
        if (lookup.contains(in.first)) Success(in.first.toString, in.rest)
        else Failure("Expected one of " + lookup + " but found '" + in.first + "'", in)
    }

    /**
     * Matches one of the given char keys in the map.
     * Returns the string value for the matched char in the given map.
     */
    def oneOf(lookup:Map[Char,String]):Parser[String] = Parser{ in =>
        if (lookup.contains(in.first)) Success(lookup(in.first), in.rest)
        else Failure("Expected one of " + lookup.keys + " but found '" + in.first + "'", in)
    }

    /**
     * Looks if the preceding char was one of the given chars.
     * Never consumes any input.
     */
    def lookbehind(cs:Set[Char]):Parser[Unit] = Parser{ in =>
        val source = in.source
        val offset = in.offset
        if (offset == 0) {
            Failure("No chars before current char, cannot look behind.", in)
        } else if (!cs.contains(source.charAt(offset-1))) {
            Failure("Previous char was '" + source.charAt(offset-1) + "' expected one of " + cs, in)
        } else {
            Success((), in)
        }
    }

    /**
     * Returns a verbose description of a char (printed char & hex code).
     * Used for debugging.
     */
    def verboseString(c:Char) = "'" + c + "'(\\u" + Integer.toHexString(c) + ")"

    /**
     * Matches one char in the given range, returns the matched char.
     */
    def range(begin:Char, end:Char):Parser[Char] = Parser{ in =>
        val c = in.first
        if (begin <= c && c <= end) Success(c, in.rest)
        else                        Failure(verboseString(c) + " not in range " +
                                            verboseString(begin) + " - " + verboseString(end),
                                            in)
    }

    def ranges(rs:SortedMap[Char, Char]):Parser[Char] = Parser{ in =>
        if (in.atEnd) Failure("End of input.", in)
        else {
            val c = in.first
            val lower:SortedMap[Char,Char] = rs.to(c)
            val (begin:Char, end:Char) = if (lower.isEmpty) ('\u0001', '\u0000') //this invalid pair always causes failure
                                         else lower.last
                               
            if (begin <= c && c <= end) Success(c, in.rest)
            else                        Failure(verboseString(c) + " not in range " +
                                            verboseString(begin) + " - " + verboseString(end),
                                            in)
        }
    }

    /**
     * Succeeds if the given parsers succeeds and the given function is defined at the parse result.
     * Returns the result of the method applied to the given parsers result. 
     */
    def acceptMatch[S,T](f:PartialFunction[S,T])(p:Parser[S]):Parser[T] = Parser { in =>
        p(in) match {
            case Success(result, next) if (f.isDefinedAt(result)) => Success(f(result), next)
            case Success(result, _)                               => Failure("Function not defined at " + result, in)
            case Failure(msg, _)                                  => Failure(msg, in)
            case Error(msg, _)                                    => Error(msg, in)
        }
    }


    /////////////////////////////////////////////////////////////////////////
    // parsers for basic markdown entities like letters, xml fragments ... //
    /////////////////////////////////////////////////////////////////////////
    /** a mapping of all chars that have to be escaped in xml and the resulting escape sequence
     * The array solution is very ugly, but cuts down block parsing time by 25%
     */
    private val escapedXmlChars = new Array[String](128)
    escapedXmlChars('<') = "&lt;"
    escapedXmlChars('>') = "&gt;"
    escapedXmlChars('"') = "&quot;"
    escapedXmlChars('\'') = "&apos;"
    escapedXmlChars('&') = "&amp;"

    /**
     * Escapes the given char for XML. Returns Either the
     * necessary XML escape Sequence or the same char in a String.
     */
    def escapeForXml(c:Char):String = {
        //looks horrible but massively faster than using a proper map and Option[String]
        val escaped:String = escapeFastForXml(c)
        if (escaped == null) Character.toString(c)
        else                 escaped
    }

    /**
     * Either returns the XML escape sequence for the given char or null.
     * This does not return Option[String] on purpose. While Option[String]
     * would be a much cleaner solution, this is actually called so often
     * that it is a noticeable difference if we use Option here.
     */
    def escapeFastForXml(c:Char) = if (c < escapedXmlChars.length) escapedXmlChars(c)
                                   else null

    /* A single char. If it is one of the chars that have to be escaped in XML it is returned as the xml escape code
     * i.e. parsing '<' returns "&lt;"
     */
    def aChar = Parser{ in =>
        if (in.atEnd) {
            Failure("End of input reached.", in)
        } else {
            Success(escapeForXml(in.first), in.rest)
        }
    }

    val xmlNameStartCharRanges:SortedMap[Char,Char] =
        SortedMap(':' -> ':', 'A' -> 'Z', '_' -> '_', 'a' -> 'z', '\u00C0' -> '\u00D6',
                  '\u00D8' -> '\u00F6', '\u00F8' -> '\u02FF', '\u0370' -> '\u037D', '\u037F' -> '\u1FFF',
                  '\u200C' -> '\u200D', '\u2070' -> '\u218F', '\u2C00' -> '\u2FEF', '\u3001' -> '\uD7FF',
                  '\uF900' -> '\uFDCF', '\uFDF0' -> '\uFFFD')//'\u10000' -> '\uEFFFF'

    val xmlNameCharRanges:SortedMap[Char,Char] =
        xmlNameStartCharRanges ++ SortedMap('-' -> '-',  '.' -> '.', '0'->'9',
            '\u00b7'->'\u00b7', '\u0300' -> '\u0369', '\u203F' -> '\u2040')

    /**Parser for one char that starts an XML name.
      * According to W3C specs except that range #x10000 to #xEFFFF
     * is excluded (cannot be expressed by char literals)
     */
    def xmlNameStartChar:Parser[Char] = ranges(xmlNameStartCharRanges)
    /** Parses an XML name char according to W3C spec except that range #x10000 to #xEFFFF is excluded
     */
    def xmlNameChar:Parser[Char] = ranges(xmlNameCharRanges)
    /** Parses an XML name (tag or attribute name)
     */
    def xmlName:Parser[String] = xmlNameStartChar ~ (xmlNameChar*) ^^ {case c ~ cs => c + cs.mkString}
    /** Parses a Simplified xml attribute: everything between quotes ("foo")
     * everything between the quotes is run through the escape handling
     * That way you can omit xml escaping when writing inline XML in markdown.
     */
    def xmlAttrVal:Parser[String] = 
      ('"'  ~> ((not('"')  ~> aChar)*) <~ '"'  ^^ {'"' +  _.mkString + '"' }) |
      ('\'' ~> ((not('\'') ~> aChar)*) <~ '\'' ^^ {'\'' + _.mkString + '\''})
    /** Parses an XML Attribute with simplified value handling like xmlAttrVal.
     */
    def xmlAttr:Parser[String] = ws ~ xmlName ~ '=' ~ xmlAttrVal ^^ {
        case w ~ name ~ _ ~ value => w + name + '=' + value
    }
    /** Parses an xml start or empty tag, attribute values are escaped.
     */
    def xmlStartOrEmptyTag:Parser[String] = '<' ~> xmlName ~ (xmlAttr*) ~ ows ~ (">" | "/>") ^^ {
        case name ~ attrs ~ w ~ e => '<' + name + attrs.mkString  + w + e
    }

    /** Parses closing xml tags.
     */
    def xmlEndTag:Parser[String] = "</" ~> xmlName <~ ">" ^^ {"</" + _ + ">"}


    /** Runs the given parser on the given input.
     *  Expects the parser to succeed and consume all input.
     *  Throws an IllegalArgumentException if parsing failed.
     */
    def apply[T](p:Parser[T], in:String):T = {
        parseAll(p, in) match {
            case Success(t, _) => t
            case e: NoSuccess  => throw new IllegalArgumentException("Could not parse '" + in + "': " + e)
        }
    }
}
