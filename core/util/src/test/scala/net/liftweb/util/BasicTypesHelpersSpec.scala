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
import _root_.org.specs.util.DataTables
import _root_.org.specs.runner._
import BasicTypesHelpers._
import _root_.java.io.{InputStream, ByteArrayInputStream}
import common._

class BasicTypesHelpersTest extends JUnit4(BasicTypesHelpersSpec)
object BasicTypesHelpersSpec extends Specification with DataTables {
  "Basic types helpers" should {
    "be lazy" in {
      (false.?[Int]({throw new Exception("Bummer")}).|(3)) must_== 3
      (true.?[Int]( 3). | ({throw new Exception("Bummer")})) must_== 3
    }

    "provide a ternary operator: (condition) ? A | B" in {
      (1 == 2) ? "a" | "b" must_== "b"
    }
    "provide a ?> operator to add an element to a list if an expression is true" in {
      (1 == 1) ?> "a" ::: List("b") must_== List("a", "b")
      (1 == 2) ?> "a" ::: List("b") must_== List("b")
    }
    val failure = Failure(null,null,null)
    "have a toBoolean method converting any object to a reasonable Boolean value" in {
      "object value" | "boolean value" 	|>
       (0: Any)		 ! false			|
       1  			 ! true				|
       (null:Any)    ! false			|
       true		     ! true				|
       false		 ! false            |
       ""   		 ! false            |
       "string"		 ! false            |
       "t"  		 ! true             |
       "total" 		 ! true             |
       "T"  		 ! true             |
       "This"  		 ! true             |
       "0"  		 ! false            |
       None  		 ! false            |
       Some("t")	 ! true             |
       Empty    	 ! false            |
       Full("t")   	 ! true             |
       failure   	 ! false             |
       List("t", "f")! true             |
       { (o: Any, result: Boolean) =>
          toBoolean(o) must_== result
       }
    }

        "have a AsBoolean extractor converting any object to a reasonable Boolean value" in {
      "object value" | "boolean value" 	|>
       "t"  		 ! Some(true)             |
       ""   		 ! None            |
       "string"		 ! None            |
       "total" 		 ! None             |
       "T"  		 ! Some(true)             |
       "This"  		 ! None             |
       "0"  		 ! Some(false)            |
       { (o: String, result: Option[Boolean]) =>
          AsBoolean.unapply(o) must_== result
       }
    }

        "have an AsInt extractor converting any String to a reasonable Int value" in {
      "object value"| "int value"	|>
       "3"			! Some(3) 	        |
       "n"			! None 	        |
       { (o: String, result: Option[Int]) =>
          AsInt.unapply(o) must_== result
       }
    }
    "have an AsLong extractor converting any String to a reasonable Long value" in {
      "object value"| "long value"	|>
       "3"			! Some(3L) 	        |
       "n"			! None 	        |
       { (o: String, result: Option[Long]) =>
          AsLong.unapply(o) must_== result
       }
    }

    "have a toInt method converting any object to a reasonable Int value" in {
      def date(t: Int) = new _root_.java.util.Date(t)
      "object value"| "int value"	|>
       (null:Any)   ! 0				|
       1	    	! 1				|
       1L			! 1 	        |
       List(1, 2)	! 1 	        |
       Some(1)		! 1 	        |
       Full(1)		! 1 	        |
       None			! 0 	        |
       Empty		! 0 	        |
       failure		! 0 	        |
       "3"			! 3 	        |
       "n"			! 0 	        |
       date(3000)	! 3 	        |
       { (o: Any, result: Int) =>
          toInt(o) must_== result
       }
    }
    "have a toLong method converting any object to a reasonable Long value" in {
      def date(t: Int) = new _root_.java.util.Date(t)
      "object value"| "long value"	|>
       (null:Any)   ! 0L			|
       1	    	! 1L			|
       1L			! 1L 	        |
       List(1, 2)	! 1L 	        |
       Some(1)		! 1L 	        |
       Full(1)		! 1L 	        |
       None			! 0L 	        |
       Empty		! 0L 	        |
       failure		! 0L 	        |
       "3"			! 3L 	        |
       "n"			! 0L 	        |
       date(3000)	! 3000L 	    |
       { (o: Any, result: Long) =>
          toLong(o) must_== result
       }
    }
    "have a toByteArrayInputStream reading an InputStream to a ByteArrayInputStream" in {
      var array: Array[Byte] = Array(12, 14)
      val input = new ByteArrayInputStream(array)
      val result = toByteArrayInputStream(input)
      result.read must_== 12
      result.read must_== 14
    }
    "have a isEq method comparing 2 Byte arrays and returning true if they contain the same elements" in {
      var a: Array[Byte] = Array(12, 14)
      var b: Array[Byte] = Array(12, 14)
      var c: Array[Byte] = Array(12, 13)
      isEq(a, b) must beTrue
      isEq(a, c) must beFalse
    }
    "have a notEq method comparing 2 Byte arrays and returning true if they don't contain the same elements" in {
      var a: Array[Byte] = Array(12, 14)
      var b: Array[Byte] = Array(12, 13)
      BasicTypesHelpers.notEq(a, b) must beTrue
    }
  }

  "PartialFunction guard" should {
    "put a guard around a partial function" in {
      val pf1: PartialFunction[String, Unit] = {
        case s if s.startsWith("s") => true
      }

      val pf2: PartialFunction[String, Boolean] = {
        case "snipe" => true
        case "bipe" => false
      }

      val pf3 = pf1.guard(pf2)
      val pf4: PartialFunction[String, Boolean] = pf1.guard(pf3)

      pf3.isDefinedAt("bipe") must_== false
      pf3.isDefinedAt("snipe") must_== true
      
    }
  }
}

}
}
