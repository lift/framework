/*
 * Copyright 2006-2010 WorldWide Conferencing, LLC
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

package net.liftweb {
package util {

import _root_.org.apache.commons.codec.binary.Base64
import _root_.java.io.{InputStream, ByteArrayOutputStream, ByteArrayInputStream, Reader, File, FileInputStream, BufferedReader, InputStreamReader}
import _root_.java.security.{SecureRandom, MessageDigest}
import _root_.javax.crypto._
import _root_.javax.crypto.spec._
import _root_.scala.xml.{Node, XML}
import common._

object SecurityHelpers extends StringHelpers with IoHelpers with SecurityHelpers

/**
 * The SecurityHelpers trait provides functions to:<ul>
 * <li> generate random numbers
 * <li> generate keys
 * <li> encrypt/decrypt keys
 * <li> create SHA, SHA-256, MD5 hashs (can be hex encoded)
 * </ul>
 */
trait SecurityHelpers { self: StringHelpers with IoHelpers =>

  /** short alias for java.security.SecureRandom */
  private val random = new _root_.java.security.SecureRandom

  /** return a random Long modulo a number */
  def randomLong(mod: Long): Long = Math.abs(random.nextLong) % mod

  /** return a random int modulo a number */
  def randomInt(mod: Int): Int = Math.abs(random.nextInt) % mod

  /**
   * return true only 'percent' times when asked repeatedly.
   * This function is used in the Skittr example to get a random set of users
   * @param percent percentage as a double number <= 1.0
   */
  def shouldShow(percent: Double): Boolean = random.nextDouble <= percent

  /** create a Blowfish key as an array of bytes */
  def makeBlowfishKey: Array[Byte] = KeyGenerator.getInstance("blowfish").generateKey.getEncoded

  /** create a Blowfish key from an array of bytes*/
  def blowfishKeyFromBytes(key: Array[Byte]): SecretKey = new SecretKeySpec(key, "blowfish")

  /** decrypt a Byte array with a Blowfish key (as a Byte array)*/
  def blowfishDecrypt(enc: Array[Byte], key: Array[Byte]): Array[Byte] = blowfishDecrypt(enc, blowfishKeyFromBytes(key))

  /** decrypt a Byte array with a Blowfish key (as a SecretKey object)*/
  def blowfishDecrypt(enc: Array[Byte], key: SecretKey): Array[Byte] = readWholeStream(decryptStream(new ByteArrayInputStream(enc), key))

  /** decrypt a Byte array with a Blowfish key (as a SecretKey object)*/
  def blowfishDecrypt(enc: String, key: Array[Byte]): String = blowfishDecrypt(enc, blowfishKeyFromBytes(key))

  /** decrypt a Byte array with a Blowfish key (as a SecretKey object)*/
  def blowfishDecrypt(enc: String, key: SecretKey): String = new String(blowfishDecrypt(base64Decode(enc), key), "UTF-8")

  /** encrypt a Byte array with a Blowfish key (as a Byte array)*/
  def blowfishEncrypt(plain: Array[Byte], key: Array[Byte]): Array[Byte] = blowfishEncrypt(plain, blowfishKeyFromBytes(key))

  /** encrypt a Byte array with a Blowfish key (as a SecretKey object)*/
  def blowfishEncrypt(plain: Array[Byte], key: SecretKey): Array[Byte] = readWholeStream(encryptStream(new ByteArrayInputStream(plain), key))

  /** encrypt a String with a Blowfish key (as a Byte array)*/
  def blowfishEncrypt(plain: String, key: Array[Byte]): String = blowfishEncrypt(plain, blowfishKeyFromBytes(key))

  /** encrypt a String with a Blowfish key (as a SecretKey object)*/
  def blowfishEncrypt(plain: String, key: SecretKey): String = base64Encode(blowfishEncrypt(plain.getBytes("UTF-8"), key))

    /** create a 3DES key as an array of bytes */
  def makeTripleDESKey: Array[Byte] = KeyGenerator.getInstance("DESede").generateKey.getEncoded

  /** create a Blowfish key from an array of bytes*/
  def tripleDESKeyFromBytes(key: Array[Byte]): SecretKey = new SecretKeySpec(key, "DESede")

  /** decrypt a Byte array with a Blowfish key (as a Byte array)*/
  def tripleDESDecrypt(enc: Array[Byte], key: Array[Byte]): Array[Byte] = tripleDESDecrypt(enc, tripleDESKeyFromBytes(key))

  /** decrypt a Byte array with a Blowfish key (as a SecretKey object)*/
  def tripleDESDecrypt(enc: Array[Byte], key: SecretKey): Array[Byte] = readWholeStream(tripleDESDecryptStream(new ByteArrayInputStream(enc), key))

  /** decrypt a Byte array with a Blowfish key (as a SecretKey object)*/
  def tripleDESDecrypt(enc: String, key: Array[Byte]): String = tripleDESDecrypt(enc, tripleDESKeyFromBytes(key))

  /** decrypt a Byte array with a Blowfish key (as a SecretKey object)*/
  def tripleDESDecrypt(enc: String, key: SecretKey): String = new String(tripleDESDecrypt(base64Decode(enc), key), "UTF-8")

  /** encrypt a Byte array with a Blowfish key (as a Byte array)*/
  def tripleDESEncrypt(plain: Array[Byte], key: Array[Byte]): Array[Byte] = tripleDESEncrypt(plain, tripleDESKeyFromBytes(key))

  /** encrypt a Byte array with a Blowfish key (as a SecretKey object)*/
  def tripleDESEncrypt(plain: Array[Byte], key: SecretKey): Array[Byte] = readWholeStream(tripleDESEncryptStream(new ByteArrayInputStream(plain), key))

  /** encrypt a String with a Blowfish key (as a Byte array)*/
  def tripleDESEncrypt(plain: String, key: Array[Byte]): String = tripleDESEncrypt(plain, tripleDESKeyFromBytes(key))

  /** encrypt a String with a Blowfish key (as a SecretKey object)*/
  def tripleDESEncrypt(plain: String, key: SecretKey): String = base64Encode(tripleDESEncrypt(plain.getBytes("UTF-8"), key))

  /** decrypt an InputStream with a Blowfish key (as a Byte array)*/
  def decryptStream(in: InputStream, key: Array[Byte]): InputStream = decryptStream(in, blowfishKeyFromBytes(key))

  /** decrypt an InputStream with a Blowfish key (as a SecretKey object)*/
  def decryptStream(in: InputStream, key: SecretKey): InputStream = {
    val cipher = Cipher.getInstance("blowfish")
    cipher.init(Cipher.DECRYPT_MODE, key)
    new CipherInputStream(in, cipher)
  }

   /** decrypt an InputStream with a Blowfish key (as a Byte array)*/
  def tripleDESDDecryptStream(in: InputStream, key: Array[Byte]): InputStream = tripleDESDecryptStream(in, tripleDESKeyFromBytes(key))

  /** decrypt an InputStream with a Blowfish key (as a SecretKey object)*/
  def tripleDESDecryptStream(in: InputStream, key: SecretKey): InputStream = {
    val cipher = Cipher.getInstance("DESede/CBC/PKCS5Padding")
    cipher.init(Cipher.DECRYPT_MODE, key, new IvParameterSpec(new Array[Byte](8)))

    new CipherInputStream(in, cipher)
  }

  def encryptXML(in: Node, key: Array[Byte]): String =
  encryptXML(in, blowfishKeyFromBytes(key))

  def encryptXML(in: Node, key: SecretKey): String =
  blowfishEncrypt(in.toString, key)

  def decryptXML(in: String, key: Array[Byte]): Box[Node] =
  decryptXML(in, blowfishKeyFromBytes(key))

  def decryptXML(in: String, key: SecretKey): Box[Node] =
    for {str <-  Helpers.tryo(blowfishDecrypt(in, key))
         xml <- Helpers.tryo(XML.loadString(str))
    } yield xml

  /** encrypt an InputStream with a Blowfish key (as a Byte array)*/
  def encryptStream(in: InputStream, key: Array[Byte]): InputStream= encryptStream(in, blowfishKeyFromBytes(key))

  /** encrypt an InputStream with a Blowfish key (as a SecretKey object)*/
  def encryptStream(in: InputStream, key: SecretKey): InputStream = {
    val cipher = Cipher.getInstance("blowfish")
    cipher.init(Cipher.ENCRYPT_MODE, key)
    new CipherInputStream(in, cipher)
  }

   /** encrypt an InputStream with a Blowfish key (as a Byte array)*/
  def tripleDESEncryptStream(in: InputStream, key: Array[Byte]): InputStream= tripleDESEncryptStream(in, tripleDESKeyFromBytes(key))

  /** encrypt an InputStream with a Blowfish key (as a SecretKey object)*/
  def tripleDESEncryptStream(in: InputStream, key: SecretKey): InputStream = {
    val cipher = Cipher.getInstance("DESede/CBC/PKCS5Padding")
    cipher.init(Cipher.ENCRYPT_MODE, key, new IvParameterSpec(new Array[Byte](8)))

    new CipherInputStream(in, cipher)
  }

  private final def cleanArray(in: Array[Byte]): Array[Byte] = in.filter(a => a >= 32 && a <= 127)

  /** encode a Byte array in Base 64 */
  def base64Encode(in: Array[Byte]): String = new String(cleanArray((new Base64).encode(in)))

  /** encode a Byte array in Base 64 in a way that's safe for use in URLs */
  def base64EncodeURLSafe(in: Array[Byte]): String = new String(Base64.encodeBase64URLSafe(in))

  /** decode a String in Base 64 */
  def base64Decode(in: String): Array[Byte] = (new Base64).decode(in.getBytes("UTF-8"))

  /** create a MD5 digest from a Byte array */
  def md5(in: Array[Byte]): Array[Byte] = (MessageDigest.getInstance("MD5")).digest(in)

  /** create a MD5 digest from a String */
  def md5(in: String): String = new String(cleanArray((new Base64) encode md5(in.getBytes("UTF-8"))))

  /** create a SHA hash from a Byte array */
  def hash(in : Array[Byte]) : Array[Byte] = {
    (MessageDigest.getInstance("SHA")).digest(in)
  }

  /** create a SHA hash from a String */
  def hash(in: String) : String = {
    new String(cleanArray((new Base64) encode (MessageDigest.getInstance("SHA")).digest(in.getBytes("UTF-8"))))
  }

   /** create a SHA hash from a String */
  def hashHex(in: String) : String = {
    Helpers.hexEncode(MessageDigest.getInstance("SHA").digest(in.getBytes("UTF-8")))
  }

  /** Compare two strings in a way that does not vary if the strings
   * are determined to be not equal early (test every byte... avoids
   * timing attackes */
  def secureEquals(s1: String, s2: String): Boolean = (s1, s2) match {
    case (null, null) => true
    case (null, _) => false
    case (_, null) => false
    case (a, b) => secureEquals(a.getBytes("UTF-8"), b.getBytes("UTF-8"))
  }
    
  /** Compare two byte arrays in a way that does not vary if the arrays
   * are determined to be not equal early (test every byte... avoids
   * timing attackes */
  def secureEquals(s1: Array[Byte], s2: Array[Byte]): Boolean = (s1, s2) match {
    case (null, null) => true
    case (null, _) => false
    case (_, null) => false
    case (a, b) => {
      val la = a.length
      val lb = b.length
      var ret = true
      var pos = 0
      while (pos < la && pos < lb) {
        ret &= (a(pos) == b(pos))
        pos += 1
      }
      ret && la == lb
    }
  }
    

  /** create a SHA-256 hash from a Byte array */
  def hash256(in : Array[Byte]) : Array[Byte] = {
    (MessageDigest.getInstance("SHA-256")).digest(in)
  }

  /** create a SHA-256 hash from a String */
  def hash256(in : String): String = {
    new String(cleanArray((new Base64) encode (MessageDigest.getInstance("SHA-256")).digest(in.getBytes("UTF-8"))))
  }

  /** create an hex encoded SHA hash from a Byte array */
  def hexDigest(in: Array[Byte]): String = {
    val binHash = (MessageDigest.getInstance("SHA")).digest(in)
    hexEncode(binHash)
  }

  /** create an hex encoded SHA-256 hash from a Byte array */
  def hexDigest256(in: Array[Byte]): String = {
    val binHash = (MessageDigest.getInstance("SHA-256")).digest(in)
    hexEncode(binHash)
  }

  def hexDecode(str: String): Array[Byte] = {
    val max = str.length / 2
    val ret = new Array[Byte](max)
    var pos = 0

    def byteOf(in: Char): Int = in match {
      case '0' => 0
      case '1' => 1
      case '2' => 2
      case '3' => 3
      case '4' => 4
      case '5' => 5
      case '6' => 6
      case '7' => 7
      case '8' => 8
      case '9' => 9
      case 'a' | 'A' => 10
      case 'b' | 'B' => 11
      case 'c' | 'C' => 12
      case 'd' | 'D' => 13
      case 'e' | 'E' => 14
      case 'f' | 'F' => 15
        case _ => 0
    }

    while (pos < max) {
      val two = pos * 2
      val ch: Char = str.charAt(two)
      val cl: Char = str.charAt(two + 1)
      ret(pos) = (byteOf(ch) * 16 + byteOf(cl)).toByte
      pos += 1
    }

    ret
  }

  /** encode a Byte array as hexadecimal characters */
  def hexEncode(in: Array[Byte]): String = {
    val sb = new StringBuilder
    val len = in.length
    def addDigit(in: Array[Byte], pos: Int, len: Int, sb: StringBuilder) {
      if (pos < len) {
        val b: Int = in(pos)
        val msb = (b & 0xf0) >> 4
        val lsb = (b & 0x0f)
        sb.append((if (msb < 10) ('0' + msb).asInstanceOf[Char] else ('a' + (msb - 10)).asInstanceOf[Char]))
        sb.append((if (lsb < 10) ('0' + lsb).asInstanceOf[Char] else ('a' + (lsb - 10)).asInstanceOf[Char]))

        addDigit(in, pos + 1, len, sb)
      }
    }
    addDigit(in, 0, len, sb)
    sb.toString
  }

}

}
}
