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

import _root_.org.specs._
import _root_.java.io.ByteArrayInputStream
import common._

object SecurityHelpersSpec extends Specification with SecurityHelpers with IoHelpers with StringHelpers {
  "Security Helpers" should {
    "provide a randomLong method returning a random Long modulo a number" in {
      randomLong(7L) must be_<(7L)
    }
    "provide a randomInt method returning a random Int modulo a number" in {
      randomInt(7) must be_<(7)
    }
    "provide a shouldShow function always returning true only a given percentage of time, expressed as a Int between 0 and 100" in {
      shouldShow(100) must beTrue
      shouldShow(0) must beFalse
    }
    "provide a shouldShow function always returning true only a given percentage of time, expressed as a Double between 0 and 1.0" in {
      shouldShow(1.0) must beTrue
      shouldShow(0.0) must beFalse
    }
    "provide makeBlowfishKey, blowfishEncrypt, blowfishDecrypt functions to encrypt/decrypt Strings with Blowfish keys" in {
      val key = makeBlowfishKey
      val encrypted = blowfishEncrypt("hello world", key)
      encrypted must_!= "hello world"
      blowfishDecrypt(encrypted, key) must_== "hello world"
    }
    "provide a md5 function to create a md5 digest from a string" in {
      md5("hello") must_== "XUFAKrxLKna5cZ2REBfFkg=="
      md5("hello") must_!= md5("hell0")
    }
    "provide a hash function to create a SHA digest from a string" in {
      hash("hello") must_== "qvTGHdzF6KLavt4PO0gs2a6pQ00="
      hash("hello") must_!= hash("hell0")
    }
    "provide a hash256 function to create a SHA-256 digest from a string" in {
      hash256("hello") must_== "LPJNul+wow4m6DsqxbninhsWHlwfp0JecwQzYpOLmCQ="
      hash256("hello") must_!= hash256("hell0")
    }
    "provide a hex encoded SHA hash function" in {
      hexDigest("hello".getBytes) must_== "aaf4c61ddcc5e8a2dabede0f3b482cd9aea9434d"
      hexDigest("hello".getBytes) must_!= hexDigest("hell0".getBytes)
    }
    "provide a hex encoded SHA-256 hash function" in {
      hexDigest256("hello".getBytes) must_== "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824"
      hexDigest256("hello".getBytes) must_!= hexDigest256("hell0".getBytes)
    }
  }
}
import _root_.org.specs.runner._
class SecurityHelpersSpecTest extends JUnit4(SecurityHelpersSpec)

}
}
