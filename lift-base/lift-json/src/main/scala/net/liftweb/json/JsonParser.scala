package net.liftweb.json

/*
 * Copyright 2009-2010 WorldWide Conferencing, LLC
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

  private[json] sealed trait MValue {
    def toJValue: JValue
  }

  private[json] case class MField(name: String, var value: MValue) extends MValue {
    def toJValue = JField(name, value.toJValue)
  }

  private[json] case object MNull extends MValue {
    def toJValue = JNull
  }

  private[json] case class MString(value: String) extends MValue {
    def toJValue = JString(value)
  }

  private[json] case class MInt(value: BigInt) extends MValue {
    def toJValue = JInt(value)
  }

  private[json] case class MDouble(value: Double) extends MValue {
    def toJValue = JDouble(value)
  }

  private[json] case class MBool(value: Boolean) extends MValue {
    def toJValue = JBool(value)
  }

  private[json] trait MBlock[A <: MValue] {
    protected var elems = List[A]()
    def +=(f: A) = elems = f :: elems
  }

  private[json] case class MObject() extends MValue with MBlock[MField] {
    def toJValue = JObject(elems.map(_.toJValue).reverse)
  }

  private[json] case class MArray() extends MValue with MBlock[MValue] {
    def toJValue = JArray(elems.map(_.toJValue).reverse)
  }
  
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
    val p = new Parser(s)
    val vals = new ValStack(p)
    var token: Token = null
    var root: Option[JValue] = None

    def closeBlock(v: MValue) {
      vals.peekOption match {
        case Some(f: MField) => 
          f.value = v
          val field = vals.pop(classOf[MField])
          vals.peek(classOf[MObject]) += field
        case Some(o: MObject) => v match {
          case x: MField => o += x
          case _ => p.fail("expected field but got " + v)
        }
        case Some(a: MArray) => a += v
        case Some(x) => p.fail("expected field, array or object but got " + x)
        case None => root = Some(v.toJValue)
      }
    }

    def newValue(v: MValue) {
      vals.peek(classOf[MValue]) match {
        case f: MField =>
          vals.pop(classOf[MField])
          f.value = v
          vals.peek(classOf[MObject]) += f
        case a: MArray =>
          a += v
        case _ => p.fail("expected field or array")
      }
    }

    do {
      token = p.nextToken
      token match {
        case OpenObj          => vals.push(MObject())
        case FieldStart(name) => vals.push(MField(name, null))
        case StringVal(x)     => newValue(MString(x))
        case IntVal(x)        => newValue(MInt(x))
        case DoubleVal(x)     => newValue(MDouble(x))
        case BoolVal(x)       => newValue(MBool(x))
        case NullVal          => newValue(MNull)
        case CloseObj         => closeBlock(vals.pop(classOf[MValue]))
        case OpenArr          => vals.push(MArray())
        case CloseArr         => closeBlock(vals.pop(classOf[MArray]))
        case End              =>
      }
    } while (token != End)

    root.get
  }

  private class ValStack(parser: Parser) {
    import java.util.LinkedList
    private[this] val stack = new LinkedList[MValue]()

    def pop[A <: MValue](expectedType: Class[A]) = convert(stack.poll, expectedType)
    def push(v: MValue) = stack.addFirst(v)
    def peek[A <: MValue](expectedType: Class[A]) = convert(stack.peek, expectedType)

    private def convert[A <: MValue](x: MValue, expectedType: Class[A]): A = {
      if (x == null) parser.fail("expected object or array")
      try { x.asInstanceOf[A] } catch { case _: ClassCastException => parser.fail("unexpected " + x) }
    }

    def peekOption = if (stack isEmpty) None else Some(stack.peek)
  }

  private class Parser(buf: String) {
    import java.util.LinkedList

    private[this] val blocks = new LinkedList[BlockMode]()
    private[this] var fieldNameMode = true
    private[this] var cur = 0 // Pointer which points current parsing location

    def fail(msg: String) = throw new ParseException(
      msg + "\nNear: " + buf.substring((cur-20) max 0, (cur+20) min (buf.length-1)), null)

    def nextToken: Token = {
      def isDelimiter(c: Char) = c == ' ' || c == '\n' || c == ',' || c == '\r' || c == '\t' || c == '}' || c == ']'

      def parseFieldName: String = {
        cur = cur+1
        val start = cur
        while (true) {
          val c = buf.charAt(cur)
          if (c == '"') {
            cur = cur+1
            return buf.substring(start, cur-1)
          }
          cur = cur+1
        }
        error("can't happen")
      }

      def parseString: String = {
        cur = cur+1
        val s = new java.lang.StringBuilder
        
        def append(c: Char) = s.append(c)
        
        while (true) {
          val c = buf.charAt(cur)
          if (c == '"') {
            cur = cur+1
            return s.toString
          }

          if (c == '\\') {
            cur = cur+1
            buf.charAt(cur) match {
              case '"'  => append('"')
              case '\\' => append('\\')
              case '/'  => append('/')
              case 'b'  => append('\b')
              case 'f'  => append('\f')
              case 'n'  => append('\n')
              case 'r'  => append('\r')
              case 't'  => append('\t')
              case 'u' => 
                val codePoint = Integer.parseInt(buf.substring(cur+1, cur+5), 16)
                cur = cur+4
                s.appendCodePoint(codePoint)
              case _ => append('\\')
            }
          } else append(c)
          cur = cur+1
        }
        error("can't happen")
      }

      def parseValue = {
        var i = cur+1
        var wasInt = true
        var doubleVal = false
        while (wasInt) {
          val c = buf.charAt(i)
          if (c == '.' || c == 'e' || c == 'E') {
            doubleVal = true
            i = i+1
          } else if (!(Character.isDigit(c) || c == '.' || c == 'e' || c == 'E' || c == '-')) {
            wasInt = false
          } else {
            i = i+1
          }
        }
        val value = buf.substring(cur, i)
        cur = i
        if (doubleVal) DoubleVal(value.toDouble) else IntVal(BigInt(value))
      }

      val len = buf.length
      while (cur < len) {
        buf.charAt(cur) match {
          case '{' =>
            blocks.addFirst(OBJECT)
            cur = cur+1
            fieldNameMode = true
            return OpenObj
          case '}' =>
            blocks.poll
            cur = cur+1
            return CloseObj
          case '"' =>
            if (fieldNameMode && blocks.peek == OBJECT) return FieldStart(parseFieldName)
            else {
              fieldNameMode = true
              return StringVal(parseString)
            }
          case 't' =>
            fieldNameMode = true
            if (buf.charAt(cur+1) == 'r' && buf.charAt(cur+2) == 'u' && buf.charAt(cur+3) == 'e' && isDelimiter(buf.charAt(cur+4))) {
              cur = cur+4
              return BoolVal(true)
            }
            fail("expected boolean")
          case 'f' =>
            fieldNameMode = true
            if (buf.charAt(cur+1) == 'a' && buf.charAt(cur+2) == 'l' && buf.charAt(cur+3) == 's' && buf.charAt(cur+4) == 'e' && isDelimiter(buf.charAt(cur+5))) {
              cur = cur+5
              return BoolVal(false)
            }
            fail("expected boolean")
          case 'n' =>
            fieldNameMode = true
            if (buf.charAt(cur+1) == 'u' && buf.charAt(cur+2) == 'l' && buf.charAt(cur+3) == 'l' && isDelimiter(buf.charAt(cur+4))) {
              cur = cur+4
              return NullVal
            }
            fail("expected null")
          case ':' =>
            fieldNameMode = false
            cur = cur+1
          case '[' =>
            blocks.addFirst(ARRAY)
            cur = cur+1
            return OpenArr
          case ']' =>
            fieldNameMode = true
            blocks.poll
            cur = cur+1
            return CloseArr
          case c if Character.isDigit(c) || c == '-' =>
            fieldNameMode = true
            return parseValue
          case c if isDelimiter(c) => cur = cur+1
          case c => fail("unknown token " + c)
        }
      }
      End
    }

    sealed abstract class BlockMode
    case object ARRAY extends BlockMode
    case object OBJECT extends BlockMode
  }
}
