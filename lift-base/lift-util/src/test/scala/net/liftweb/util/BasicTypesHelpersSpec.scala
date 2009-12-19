package net.liftweb.util

import _root_.org.specs._
import _root_.org.specs.util.DataTables
import _root_.org.specs.runner._
import BasicTypesHelpers._
import _root_.java.io.{InputStream, ByteArrayInputStream}
import common._

class BasicTypesHelpersTest extends JUnit4(BasicTypesHelpersSpec)
object BasicTypesHelpersSpec extends Specification with DataTables {
  "Basic types helpers" should {
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
}
