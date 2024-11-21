/*
 * Copyright 2006-2011 WorldWide Conferencing, LLC
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
package util

import org.specs2.mutable.Specification

import common._
import ClassHelpers._


/**
 * Systems under specification for ClassHelpers.
 */
class ClassHelpersSpec extends Specification  {
  "ClassHelpers Specification".title

  "The findType function" should {
    "return a Full can with the found class when given the type, the name, and a list of packages to conform to" in {
      findType[java.util.List[Object]]("ArrayList", List("java.util")) must_== Full(classOf[java.util.ArrayList[Object]])
    }
    "return an Empty can if the class cannot be coerced to the expected type" in {
      findType[String]("ClassHelpers", List("net.liftweb.util")) must_== Empty
    }
  }
  "the findClass function" should {
    "return a Full can with the found class when given the name and package" in {
      findClass("ClassHelpers", List("net.liftweb.util")) must_== Full(classOf[ClassHelpers])
    }
    "return a Full can with the found class when given the name and package, with an underscored name instead of CamelCased" in {
      findClass("class_helpers", List("net.liftweb.util")) must_== Full(classOf[ClassHelpers])
    }
    "return a Full can with the found class when given the name and a list of packages" in {
      findClass("ClassHelpers", List("net.liftweb.util", "java.util")) must_== Full(classOf[ClassHelpers])
      findClass("ArrayList", List("net.liftweb.util", "java.util")) must_== Full(classOf[java.util.ArrayList[_]])
    }
    "return a Full can with the found class when given the name, a list of packages and a target type to conform to" in {
      findClass("ArrayList", List("java.util"), classOf[java.util.List[Object]]) must_== Full(classOf[java.util.ArrayList[Object]])
    }
    "return an Empty can if no class is found given a name and package" in {
      findClass("ClassHelpers", List("net.liftweb.nothere")) must_== Empty
    }
    "return an Empty can if the class cannot be coerced to the expected type" in {
      findClass("ClassHelpers", List("net.liftweb.util"), classOf[String]) must_== Empty
    }
  }

  "The findClass function" should {
    "return a Full can with the found class when given a list of names and corresponding packages" in {
      findClass(List(("wrong name", List("net.liftweb.util", "other.package")),
                     ("ClassHelpers", List("net.liftweb.util", "other.package")))) must_== Full(classOf[ClassHelpers])
    }
    "use a list of modifiers functions to try to modify the original name in order to find the class" in {
      findClass("classHelpers", List("net.liftweb.util"), List((n: String) => n.capitalize)) must_== Full(classOf[ClassHelpers])
    }
  }

  "The callableMethod_? function" should {
    "return true if the method is public and has no parameters" in {
      val publicParameterLess = classOf[String].getMethod("length")
      callableMethod_?(publicParameterLess) must beTrue
    }
    "return false if the method is public and has parameters" in {
      val publicWithParameters = classOf[String].getMethod("indexOf", classOf[String])
      callableMethod_?(publicWithParameters) must beFalse
    }
    "return false if the method is private" in {
      val privateMethod = classOf[java.util.ArrayList[Object]].getDeclaredMethod("readObject", classOf[java.io.ObjectInputStream])
      callableMethod_?(privateMethod) must beFalse
    }
    "return false if the method is null" in {
      callableMethod_?(null) must beFalse
    }
  }

  "The containsClass function" should {
    "return false if the list to match is null or empty" in {
      containsClass(classOf[String], null) must beFalse
      containsClass(classOf[String], Nil) must beFalse
    }
    "return false if the list to match doesn't contain any class assignable by the tested class" in {
      containsClass(classOf[String], List(classOf[Float], classOf[java.lang.Integer])) must beFalse
    }
  }

  "The classHasControllerMethod function" should {
    "return true if the class has 'name' as a callable method" in {
      classHasControllerMethod(classOf[String], "length") must beTrue
    }
    "return false if the class doesn't have 'name' as a method" in {
      classHasControllerMethod(classOf[String], "isNotEmpty") must beFalse
    }
    "return false if the class has a method but it is not callable" in {
      classHasControllerMethod(classOf[java.util.ArrayList[Object]], "readObject") must beFalse
    }
    "return false if the class is null" in {
      classHasControllerMethod(null, "readObject") must beFalse
    }
  }

  "The invokeControllerMethod function" should {
    "return the result of calling the method on a new instance of the class" in {
      invokeControllerMethod(classOf[String], "length") must_== 0
    }
    "throw an exception when the method is not callable" in {
      invokeControllerMethod(classOf[String], "isNotEmpty") must throwA[NoSuchMethodException]
    }
    "throw an exception if the class is null" in {
      invokeControllerMethod(null, "length") must throwA[NullPointerException]
    }
  }

  "The invokeMethod function" should {
    "return a Failure if the class is null" in {
      invokeMethod(null, "", "length") must beLike { case Failure(_, _, _) => 1 must_== 1 }
    }
    "return a Failure if the instance is null" in {
      invokeMethod(classOf[String], null, "length") must beLike { case Failure(_, _, _) => 1 must_== 1 }
    }
    "return a Failure if the method name is null" in {
      invokeMethod(classOf[String], "", null) must beLike { case Failure(_, _, _) => 1 must_== 1 }
    }
    "return a Failure if the method doesnt exist on the class" in {
      invokeMethod(classOf[String], "", "isNotEmpty") must beLike { case Failure(_, _, _) => 1 must_== 1 }
    }
    "return a Full can with the result if the method exist on the class" in {
      invokeMethod(classOf[String], "", "length") must_== Full(0)
    }
    "return a Full can with the result if the method is an existing static method on the class" in {
      invokeMethod(classOf[java.util.Calendar], null, "getInstance").isEmpty must_== false
    }
    "throw an exception if the method throws an exception" in {
      class SpecificException extends Exception
      class TestSnippet { def throwException = throw new SpecificException  }
      val testSnippet = new TestSnippet
      invokeMethod(testSnippet.getClass, testSnippet, "throwException") must throwA[SpecificException]
    }
  }

  "The invokeMethod function" can {
    "call a method with its parameters" in {
      invokeMethod(classOf[String], "", "valueOf", Array("1")) must_== Full("1")
    }
    "call a method with its parameters and parameter types" in {
      invokeMethod(classOf[String], "", "valueOf", Array("c"), Array(classOf[String])) must_== Full("c")
    }
  }

  "The instantiate function" should {
    "return a full can if a class can be instantiated with a new instance" in {
      instantiate(classOf[String]) must_== Full("")
    }
    "return a failure if a class can not be instantiated with a new instance" in {
      instantiate(classOf[java.util.Calendar]) must beLike { case Failure(_, _, _) => 1 must_== 1 }
    }
  }

  "The createInvoker function" should {
    "return Empty if the instance is null" in {
      createInvoker("length", null) must_== Empty
    }
    "return a Full Box with the function from Unit to a Box containing the result of the method to invoke" in {
      createInvoker("length", "").openOrThrowException("Test").apply() must_== Full(0)
    }
    "The invoker function will throw the cause exception if the method can't be called" in {
      (() => createInvoker("get", "").openOrThrowException("Test").apply())() must throwA[Exception]
    }
  }

}

