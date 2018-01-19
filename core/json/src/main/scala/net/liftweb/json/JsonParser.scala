/*
 * Copyright 2009-2010 WorldWide Conferencing, LLC
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
 */

package net.liftweb
package json

import scala.annotation.switch

/** JSON parser.
 */
object JsonParser {
  import java.io._

  class ParseException(message: String, cause: Exception) extends Exception(message, cause)

  /** Parsed tokens from low level pull parser.
   */
  sealed abstract class Token
  case object OpenObj extends Token
  case object CloseObj extends Token
  case class FieldStart(name: String) extends Token
  case object End extends Token
  case class StringVal(value: String) extends Token
  case class IntVal(value: BigInt) extends Token
  case class DoubleVal(value: Double) extends Token
  case class BoolVal(value: Boolean) extends Token
  case object NullVal extends Token
  case object OpenArr extends Token
  case object CloseArr extends Token

  /** Return parsed JSON.
   * @throws ParseException is thrown if parsing fails
   */
  def parse(s: String): JValue = parse(new Buffer(new StringReader(s), false))

  /** Return parsed JSON.
   * @param closeAutomatically true (default) if the Reader is automatically closed on EOF
   * @throws ParseException is thrown if parsing fails
   */
  def parse(s: Reader, closeAutomatically: Boolean = true): JValue =
    parse(new Buffer(s, closeAutomatically))

  /** Return parsed JSON.
   */
  def parseOpt(s: String): Option[JValue] =
    try { parse(s).toOpt } catch { case e: Exception => None }

  /** Return parsed JSON.
   * @param closeAutomatically true (default) if the Reader is automatically closed on EOF
   */
  def parseOpt(s: Reader, closeAutomatically: Boolean = true): Option[JValue] =
    try { parse(s, closeAutomatically).toOpt } catch { case e: Exception => None }

  /** Parse in pull parsing style.
   * Use <code>p.nextToken</code> to parse tokens one by one from a string.
   * @see net.liftweb.json.JsonParser.Token
   */
  def parse[A](s: String, p: Parser => A): A = parse(new StringReader(s), p)

  /** Parse in pull parsing style.
   * Use <code>p.nextToken</code> to parse tokens one by one from a stream.
   * The Reader must be closed when parsing is stopped.
   * @see net.liftweb.json.JsonParser.Token
   */
  def parse[A](s: Reader, p: Parser => A): A = p(new Parser(new Buffer(s, false)))

  private[json] def parse(buf: Buffer): JValue = {
    try {
      astParser(new Parser(buf))
    } catch {
      case e: ParseException => throw e
      case e: Exception => throw new ParseException("parsing failed", e)
    } finally { buf.release }
  }

  // JSON hex unicode strings (\u12AF) are translated into characters through
  // this array. Each number in the array corresponds to the 4-bit value that
  // one number in the hex string will represent. These are combined when
  // reading the unicode string.
  private[this] final val HexChars: Array[Int] = {
    val chars = new Array[Int](128)
    var i = 0
    while (i < 10) {
      chars(i + '0') = i
      i += 1
    }
    i = 0
    while (i < 16) {
      chars(i + 'a') = 10 + i
      chars(i + 'A') = 10 + i
      i += 1
    }
    chars
  }
  // The size of one hex character in bits.
  private[this] final val hexCharSize = 4 // in bits

  private[json] def unquote(string: String): String =
    unquote(new JsonParser.Buffer(new java.io.StringReader(string), false))

  private[this] def unquote(buf: JsonParser.Buffer): String = {
    def unquote0(buf: JsonParser.Buffer): String = {
      val builder = buf.builder
      builder.delete(0, builder.length())
      var c = '\\'
      while (c != '"') {
        if (c == '\\') {
          buf.substring(intoBuilder = true)
          (buf.next: @switch) match {
            case '"'  => builder.append('"')
            case '\\' => builder.append('\\')
            case '/'  => builder.append('/')
            case 'b'  => builder.append('\b')
            case 'f'  => builder.append('\f')
            case 'n'  => builder.append('\n')
            case 'r'  => builder.append('\r')
            case 't'  => builder.append('\t')
            case 'u' =>
              var byte = 0
              var finalChar = 0
              val chars = Array(buf.next, buf.next, buf.next, buf.next)
              while (byte < chars.length) {
                finalChar = (finalChar << hexCharSize) | HexChars(chars(byte).toInt)
                byte += 1
              }
              builder.appendCodePoint(finalChar.toChar)
            case _ =>
              builder.append('\\')
          }
          buf.mark
        }
        c = buf.next
      }
      buf.substring(intoBuilder = true)
      builder.toString
    }

    buf.eofIsFailure = true
    buf.mark
    var c = buf.next
    var forcedReturn: String = null
    while (c != '"') {
      (c: @switch) match {
        case '\\' =>
          forcedReturn = unquote0(buf)
          c = '"'
        case _ =>
          c = buf.next
      }
    }
    buf.eofIsFailure = false

    if (forcedReturn == null) {
      new String(buf.substring())
    } else {
      forcedReturn
    }
  }

  private[json] def parseDouble(s: String) = {
    s.toDouble
  }

  // Intermediate objects and arrays which can be grown mutably for performance.
  // These are finalized into immutable JObject and JArray.
  private[this] case class IntermediateJObject(fields: scala.collection.mutable.ListBuffer[JField])
  private[this] case class IntermediateJArray(bits: scala.collection.mutable.ListBuffer[JValue])

  private val astParser = (p: Parser) => {
    val vals = new ValStack(p)
    var token: Token = null
    var root: Option[JValue] = None

    // At the end of an object, if we're looking at an intermediate form of an
    // object or array, gather up all their component parts and create the final
    // object or array.
    def closeBlock(v: Any) {
      def toJValue(x: Any) = x match {
        case json: JValue => json
        case other: IntermediateJObject => JObject(other.fields.result)
        case other: IntermediateJArray => JArray(other.bits.result)
        case _ => p.fail("unexpected field " + x)
      }

      vals.peekOption match {
        case Some(JField(name: String, value)) =>
          vals.pop(classOf[JField])
          val obj = vals.peek(classOf[IntermediateJObject])
          obj.fields.append(JField(name, toJValue(v)))
        case Some(o: IntermediateJObject) =>
          o.fields.append(vals.peek(classOf[JField]))
        case Some(a: IntermediateJArray) => a.bits.append(toJValue(v))
        case Some(x) => p.fail("expected field, array or object but got " + x)
        case None => root = Some(toJValue(v))
      }
    }

    def newValue(v: JValue) {
      if (!vals.isEmpty)
        vals.peekAny match {
          case JField(name, value) =>
            vals.pop(classOf[JField])
            val obj = vals.peek(classOf[IntermediateJObject])
            obj.fields += (JField(name,v))
          case a: IntermediateJArray => a.bits += v
          case other => p.fail("expected field or array but got " + other)
      } else {
        vals.push(v)
        root = Some(v)
      }
    }

    do {
      token = p.nextToken
      token match {
        case OpenObj          => vals.push(IntermediateJObject(scala.collection.mutable.ListBuffer()))
        case FieldStart(name) => vals.push(JField(name, null))
        case StringVal(x)     => newValue(JString(x))
        case IntVal(x)        => newValue(JInt(x))
        case DoubleVal(x)     => newValue(JDouble(x))
        case BoolVal(x)       => newValue(JBool(x))
        case NullVal          => newValue(JNull)
        case CloseObj         => closeBlock(vals.popAny)
        case OpenArr          => vals.push(IntermediateJArray(scala.collection.mutable.ListBuffer()))
        case CloseArr         => closeBlock(vals.popAny)
        case End              =>
      }
    } while (token != End)

    root getOrElse JNothing
  }

  private[this] final val EOF: Char = (-1).asInstanceOf[Char]

  private class ValStack(parser: Parser) {
    import java.util.ArrayDeque
    private[this] val stack = new ArrayDeque[Any](32)

    def popAny = stack.poll
    def pop[A](expectedType: Class[A]) = convert(stack.poll, expectedType)
    def push(v: Any) = stack.addFirst(v)
    def peekAny = stack.peek
    def peek[A](expectedType: Class[A]) = convert(stack.peek, expectedType)
    def replace[A](newTop: Any) = {
      stack.pop
      stack.push(newTop)
    }

    private def convert[A](x: Any, expectedType: Class[A]): A = {
      if (x == null) parser.fail("expected object or array")

      try {
        x.asInstanceOf[A]
      } catch {
        case cce: ClassCastException =>
          parser.fail(s"failure during class conversion. I got $x but needed a type of $expectedType", cce)
      }
    }

    def peekOption = if (stack.isEmpty) None else Some(stack.peek)
    def isEmpty = stack.isEmpty
  }

  class Parser(buf: Buffer) {
    import java.util.ArrayDeque

    // Maintains our current nesting context in the form of BlockMode, which
    // indicates if each context is an array or object.
    private[this] val blocks = new ArrayDeque[BlockMode](32)
    private[this] var fieldNameMode = true

    def fail(msg: String, cause: Exception = null) = throw new ParseException(msg + "\nNear: " + buf.near, cause)

    /** Parse next Token from stream.
     */
    def nextToken: Token = {
      def parseString: String =
        try {
          unquote(buf)
        } catch {
          case p: ParseException => throw p
          case cause: Exception => fail("unexpected string end", cause)
        }

      def parseValue(first: Char) = {
        var wasInt = true
        var doubleVal = false
        val buf = this.buf

        // Back up and mark the buffer so that we can extract a substring after
        // that contains the whole value.
        buf.back
        buf.mark
        while (wasInt) {
          val c = buf.next
          (c: @switch) match  {
            case '.' | 'e' | 'E' =>
              doubleVal = true
            case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '-' | '+' =>
              // continue
            case _ =>
              wasInt = false
              if (c != EOF) {
                buf.back // don't include the last character
              }
          }
        }
        buf.forward // substring is exclusive to the last index
        val value = buf.substring()
        buf.back // back up so our current pointer is in the right place
        (doubleVal: @switch) match {
          case true =>
            DoubleVal(parseDouble(new String(value)))
          case false =>
            IntVal(BigInt(new String(value)))
        }
      }

      while (true) {
        (buf.next: @switch) match {
          case '{' =>
            blocks.addFirst(OBJECT)
            fieldNameMode = true
            return OpenObj
          case '}' =>
            blocks.poll
            return CloseObj
          case '"' =>
            if (fieldNameMode && blocks.peek == OBJECT) return FieldStart(parseString)
            else {
              fieldNameMode = true
              return StringVal(parseString)
            }
          case 't' =>
            fieldNameMode = true
            if (buf.next == 'r' && buf.next == 'u' && buf.next == 'e') {
              return BoolVal(true)
            }
            fail("expected boolean")
          case 'f' =>
            fieldNameMode = true
            if (buf.next == 'a' && buf.next == 'l' && buf.next == 's' && buf.next == 'e') {
              return BoolVal(false)
            }
            fail("expected boolean")
          case 'n' =>
            fieldNameMode = true
            if (buf.next == 'u' && buf.next == 'l' && buf.next == 'l') {
              return NullVal
            }
            fail("expected null")
          case ':' =>
            if (blocks.peek == ARRAY) fail("Colon in an invalid position")
            fieldNameMode = false
          case '[' =>
            blocks.addFirst(ARRAY)
            return OpenArr
          case ']' =>
            fieldNameMode = true
            blocks.poll
            return CloseArr
          case c @ ('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '-') =>
            fieldNameMode = true
            return parseValue(c)
          case ' ' |  '\n' | ',' | '\r' | '\t' =>
            // ignore
          case c =>
            c match {
              case `EOF` =>
                buf.automaticClose
                return End
              case _ =>
                fail("unknown token " + c)
            }
        }
      }
      buf.automaticClose
      End
    }

    sealed abstract class BlockMode
    case object ARRAY extends BlockMode
    case object OBJECT extends BlockMode
  }

  /* Buffer used to parse JSON.
   * Buffer is divided to one or more segments (preallocated in segmentPool).
   */
  private[json] final class Buffer(in: Reader, closeAutomatically: Boolean, segmentPool: SegmentPool = Segments) {
    // Reused by the parser when appropriate, allows for a single builder to be
    // used throughout the parse process, and to be written to directly from the
    // substring method, so as to avoid allocating new builders when avoidable.
    private[json] final val builder = new java.lang.StringBuilder(32)

    var offset = 0 // how far into the current segment we've read data
    var curMark = -1
    var curMarkSegment = -1
    var eofIsFailure = false
    private[this] var segments = scala.collection.mutable.ArrayBuffer(segmentPool.apply())
    private[this] var segment: Array[Char] = segments.head.seg
    private[this] var cur = 0 // Pointer which points current parsing location
    private[this] var curSegmentIdx = 0 // Pointer which points current segment

    // Mark the current point so that future substring calls will extract the
    // value from this point to whatever point the buffer has advanced to.
    def mark = {
      if (curSegmentIdx > 0) {
        segments(0) = segments.remove(curSegmentIdx)
        curSegmentIdx = 0
      }

      curMark = cur
      curMarkSegment = curSegmentIdx
    }
    def back = cur = cur-1
    def forward = cur = cur+1

    // Read the next character; reads new data from the reader if necessary.
    def next: Char = {
      if (cur >= offset && read < 0) {
        if (eofIsFailure) throw new ParseException("unexpected eof", null) else EOF
      } else {
        val c = segment(cur)
        cur += 1
        c
      }
    }

    private[this] final val emptyArray = new Array[Char](0)
    // Slices from the last marked point to the current index. If intoBuilder is
    // true, appends it to the buffer's builder and returns an empty array. If
    // false, slices it into a new array and returns that array.
    final def substring(intoBuilder: Boolean = false) = {
      if (curSegmentIdx == curMarkSegment) {
        val substringLength = cur - curMark - 1
        if (intoBuilder) {
          builder.append(segment, curMark, substringLength)
          emptyArray
        } else if (substringLength == 0) {
          emptyArray
        } else {
          val array = new Array[Char](substringLength)
          System.arraycopy(segment, curMark, array, 0, substringLength)
          array
        }
      } else { // slower path for case when string is in two or more segments
        val segmentCount = curSegmentIdx - curMarkSegment + 1
        val substringLength = segmentCount * segmentPool.segmentSize - curMark - (segmentPool.segmentSize - cur) - 1
        val chars =
          if (intoBuilder) {
            emptyArray
          } else {
            new Array[Char](substringLength)
          }

        var i = curMarkSegment
        var offset = 0
        while (i <= curSegmentIdx) {
          val s = segments(i).seg
          val start = if (i == curMarkSegment) curMark else 0
          val end = if (i == curSegmentIdx) cur else s.length+1
          val partLen = end-start-1
          if (intoBuilder) {
            builder.append(s, start, partLen)
          } else {
            System.arraycopy(s, start, chars, offset, partLen)
          }
          offset += partLen
          i = i+1
        }

        curMarkSegment = -1
        curMark = -1

        chars
      }
    }

    def near = {
      val start = (cur - 20) max 0
      val len = ((cur + 1) min segmentPool.segmentSize) - start
      new String(segment, start, len)
    }

    def release = segments.foreach(segmentPool.release)

    private[JsonParser] def automaticClose = if (closeAutomatically) in.close

    // Reads the next available block from the reader. Returns -1 if there's
    // nothing more to read.
    private[this] def read = {
      if (offset >= segment.length) {
        offset = 0
        val segmentToUse =
          (curMarkSegment: @scala.annotation.switch) match {
            case -1 =>
              curSegmentIdx = 0
              segments(0)
            case _ =>
              curSegmentIdx += 1
              if (curSegmentIdx < segments.length) {
                segments(curSegmentIdx)
              } else {
                val segment = segmentPool.apply()
                segments.append(segment)
                segment
              }
          }

        segment = segmentToUse.seg
      }

      val length = in.read(segment, offset, segment.length-offset)
      if (length != -1) {
        cur = offset
        offset += length
        length
      } else -1
    }
  }

  private[json] trait SegmentPool {
    def apply(): Segment
    def release(segment: Segment): Unit
    def segmentSize: Int
  }

  private[json] class ArrayBlockingSegmentPool(override val segmentSize: Int) extends SegmentPool {
    import java.util.concurrent.ArrayBlockingQueue
    import java.util.concurrent.atomic.AtomicInteger

    private[this] val maxNumOfSegments = 10000
    private[this] var segmentCount = new AtomicInteger(0)
    private[this] val segments = new ArrayBlockingQueue[Segment](maxNumOfSegments)
    private[json] def clear = segments.clear

    def apply(): Segment = {
      val s = acquire
      // Give back a disposable segment if pool is exhausted.
      if (s != null) s else DisposableSegment(new Array(segmentSize))
    }

    private[this] def acquire: Segment = {
      val curCount = segmentCount.get
      val createNew =
        if (segments.size == 0 && curCount < maxNumOfSegments)
          segmentCount.compareAndSet(curCount, curCount + 1)
        else false

      if (createNew) RecycledSegment(new Array(segmentSize)) else segments.poll
    }

    def release(s: Segment) = s match {
      case _: RecycledSegment => segments.offer(s)
      case _ =>
    }
  }

  /*
   * A pool of preallocated char arrays.
   */
  private object Segments extends ArrayBlockingSegmentPool(1000)

  sealed trait Segment {
    val seg: Array[Char]
  }
  case class RecycledSegment(seg: Array[Char]) extends Segment
  case class DisposableSegment(seg: Array[Char]) extends Segment
}
