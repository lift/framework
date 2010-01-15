package net.liftweb.json

/*
 * Copyright 2009 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */

/** Fast imperative parser.
 */
object JsonParser {
  import JsonAST._
  import java.io._

  class ParseException(message: String, cause: Exception) extends Exception(message, cause)

  private[json] sealed abstract class Token
  private[json] case object OpenObj extends Token
  private[json] case object CloseObj extends Token
  private[json] case class FieldStart(name: String) extends Token
  private[json] case object End extends Token
  private[json] case class StringVal(value: String) extends Token
  private[json] case class IntVal(value: BigInt) extends Token
  private[json] case class DoubleVal(value: Double) extends Token
  private[json] case class BoolVal(value: Boolean) extends Token
  private[json] case object NullVal extends Token
  private[json] case object OpenArr extends Token
  private[json] case object CloseArr extends Token

  /** Return parsed JSON.
   * @throws ParseException is thrown if parsing fails
   */
  def parse(s: String): JValue = 
    try {
      parse0(s)
    } catch {
      case e: ParseException => throw e
      case e: Exception => throw new ParseException("parsing failed", e)
    }

  private def parse0(s: String): JValue = {
    val buf = new Buffer(new StringReader(s))
    val p = new Parser(buf)
    val vals = new ValStack(p)
    var token: Token = null
    var root: Option[JValue] = None

    // This is a slightly faster way to correct order of fields and arrays than using 'map'.
    def reverse(v: JValue): JValue = v match {
      case JObject(l) => JObject(l.map(reverse).asInstanceOf[List[JField]].reverse)
      case JArray(l) => JArray(l.map(reverse).reverse)
      case JField(name, value) => JField(name, reverse(value))
      case x => x
    }
    
    def closeBlock(v: JValue) {
      vals.peekOption match {
        case Some(f: JField) => 
          val field = vals.pop(classOf[JField])
          val newField = JField(field.name, v)
          val obj = vals.peek(classOf[JObject])
          vals.replace(JObject(newField :: obj.obj))
        case Some(o: JObject) => v match {
          case x: JField => vals.replace(JObject(x :: o.obj))
          case _ => p.fail("expected field but got " + v)
        }
        case Some(a: JArray) => vals.replace(JArray(v :: a.arr))
        case Some(x) => p.fail("expected field, array or object but got " + x)
        case None => root = Some(reverse(v))
      }
    }

    def newValue(v: JValue) {
      vals.peek(classOf[JValue]) match {
        case f: JField =>
          vals.pop(classOf[JField])
          val newField = JField(f.name, v)
          val obj = vals.peek(classOf[JObject])
          vals.replace(JObject(newField :: obj.obj))
        case a: JArray => vals.replace(JArray(v :: a.arr))
        case _ => p.fail("expected field or array")
      }
    }

    do {
      token = p.nextToken
      token match {
        case OpenObj          => vals.push(JObject(Nil))
        case FieldStart(name) => vals.push(JField(name, null))
        case StringVal(x)     => newValue(JString(x))
        case IntVal(x)        => newValue(JInt(x))
        case DoubleVal(x)     => newValue(JDouble(x))
        case BoolVal(x)       => newValue(JBool(x))
        case NullVal          => newValue(JNull)
        case CloseObj         => closeBlock(vals.pop(classOf[JValue]))
        case OpenArr          => vals.push(JArray(Nil))
        case CloseArr         => closeBlock(vals.pop(classOf[JArray]))
        case End              =>
      }
    } while (token != End)

    buf.reset // FIXME in finally block

    root.get
  }
  
  private val EOF = (-1).asInstanceOf[Char]

  private class ValStack(parser: Parser) {
    import java.util.LinkedList
    private[this] val stack = new LinkedList[JValue]()

    def pop[A <: JValue](expectedType: Class[A]) = convert(stack.poll, expectedType)
    def push(v: JValue) = stack.addFirst(v)
    def peek[A <: JValue](expectedType: Class[A]) = convert(stack.peek, expectedType)
    def replace[A <: JValue](newTop: JValue) = stack.set(0, newTop)

    private def convert[A <: JValue](x: JValue, expectedType: Class[A]): A = {
      if (x == null) parser.fail("expected object or array")
      try { x.asInstanceOf[A] } catch { case _: ClassCastException => parser.fail("unexpected " + x) }
    }

    def peekOption = if (stack isEmpty) None else Some(stack.peek)
  }

  private class Parser(buf: Buffer) {
    import java.util.LinkedList

    private[this] val blocks = new LinkedList[BlockMode]()
    private[this] var fieldNameMode = true

    def fail(msg: String) = throw new ParseException(msg, null)
      //FIXME: msg + "\nNear: " + buf.substring((cur-20) max 0, (cur+20) min (buf.length-1)), null)

    def nextToken: Token = {
      def isDelimiter(c: Char) = c == ' ' || c == '\n' || c == ',' || c == '\r' || c == '\t' || c == '}' || c == ']'

      def parseFieldName: String = {
        buf.mark
        var c = buf.next
        while (c != '"') {
          c = buf.next
        }
        buf.substring
      }

      def parseString: String = {
        buf.mark
        var c = 'x'
        do {
          c = buf.next
          if (c == '\\') return parseEscapedString(buf.substring)
        } while (c != '"')
        buf.substring
      }

      def parseEscapedString(base: String): String = {
        val s = new java.lang.StringBuilder(base)
        var c = '\\'
        while (true) {
          if (c == '"') {
            return s.toString
          }

          if (c == '\\') {
            buf.next match {
              case '"'  => s.append('"')
              case '\\' => s.append('\\')
              case '/'  => s.append('/')
              case 'b'  => s.append('\b')
              case 'f'  => s.append('\f')
              case 'n'  => s.append('\n')
              case 'r'  => s.append('\r')
              case 't'  => s.append('\t')
              case 'u' => 
                val chars = Array(buf.next, buf.next, buf.next, buf.next)
                val codePoint = Integer.parseInt(new String(chars), 16)
                s.appendCodePoint(codePoint)
              case _ => s.append('\\')
            }
          } else s.append(c)
          c = buf.next
        }
        error("can't happen")        
      }

      def parseValue(first: Char) = {
        var wasInt = true
        var doubleVal = false
        val s = new StringBuilder
        s.append(first) 
        while (wasInt) {
          val c = buf.next
          if (c == '.' || c == 'e' || c == 'E') {
            doubleVal = true
            s.append(c)
          } else if (!(Character.isDigit(c) || c == '.' || c == 'e' || c == 'E' || c == '-')) {
            wasInt = false
            buf.back
          } else s.append(c)
        }
        val value = s.toString
        if (doubleVal) DoubleVal(value.toDouble) else IntVal(BigInt(value))
      }

      while (true) {
        buf.next match {
          case c if EOF == c => return End //doParse = false
          case '{' =>
            blocks.addFirst(OBJECT)
            fieldNameMode = true
            return OpenObj
          case '}' =>
            blocks.poll
            return CloseObj
          case '"' =>
            if (fieldNameMode && blocks.peek == OBJECT) return FieldStart(parseFieldName)
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
            fieldNameMode = false
          case '[' =>
            blocks.addFirst(ARRAY)
            return OpenArr
          case ']' =>
            fieldNameMode = true
            blocks.poll
            return CloseArr
          case c if Character.isDigit(c) || c == '-' =>
            fieldNameMode = true
            return parseValue(c)
          case c if isDelimiter(c) => 
          case c => fail("unknown token " + c)
        }
      }
      End
    }

    sealed abstract class BlockMode
    case object ARRAY extends BlockMode
    case object OBJECT extends BlockMode
  }

  private type Buf = Array[Char]

  // parse("""["foobarfoobarhello""")  infinite loop
  private[json] class Buffer(in: Reader) {
    var length = -1
    var curMark = -1
    var curMarkBuf = -1
    private[this] var bufs: List[Buf] = Nil
    private[this] var buf: Buf = _
    private[this] var cur = 0 // Pointer which points current parsing location
    private[this] var curBufIdx = 0 // Pointer which points current buffer
    read

    def mark = { curMark = cur; curMarkBuf = curBufIdx }
    def back = cur = cur-1

    def next: Char = {
      try {
        val c = buf(cur)
        if (cur >= length) return EOF
        cur = cur+1
        c
      } catch {
        // suprisingly catching IndexOutOfBounds is faster than: if (cur == buf.length) 
        case e => 
          read
          if (length == -1) EOF else next
      }
    }

    def substring = {
      if (curBufIdx == curMarkBuf) new String(buf, curMark, cur-curMark-1)
      else { // slower path for case when string is in two or more buffers
        var parts: List[(Int, Int, Buf)] = Nil
        var i = curBufIdx
        while (i >= curMarkBuf) {
          val b = bufs(i)
          val start = if (i == curMarkBuf) curMark else 0
          val end = if (i == curBufIdx) cur else b.length+1
          parts = (start, end, b) :: parts
          i = i-1
        }
        val len = parts.map(p => p._2 - p._1 - 1).foldLeft(0)(_ + _)
        val chars = new Array[Char](len)
        i = 0
        var pos = 0

        while (i < parts.size) {
          val (start, end, b) = parts(i)
          val partLen = end-start-1
          System.arraycopy(b, start, chars, pos, partLen)
          pos = pos + partLen
          i = i+1
        }
        new String(chars)
      }
    }

    def reset = bufs.foreach(Buffer.reset)

    private[this] def read = {
      try {
        val newBuf = Buffer()
        length = in.read(newBuf)
        buf = newBuf
        bufs = bufs ::: List(newBuf)
        cur = 0
        curBufIdx = bufs.length-1
      } finally {
        // FIXME close when all read
//        in.close
      }
    }
  }

  private[json] object Buffer {
    import scala.collection.mutable._

    private[json] var bufSize = 512 // FIXME figure out proper size
    private[this] val bufs = new ListBuffer[Buf]()
    private[json] def clear = bufs.clear

    def apply() = {
      synchronized {
        if (bufs.size == 0) {
          mkBuf // FIXME limit max number of buffers
        } else bufs.remove(0)
      }
    }

    def reset(buf: Buf) = synchronized { bufs += buf }

    def mkBuf = new Buf(bufSize) 
  }
}
