/*
 * Copyright 2008-2010 WorldWide Conferencing, LLC
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
package http {
package testing {

import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.common.{ Box, Full, Empty, Failure}
import _root_.net.liftweb.util.{Helpers}
import _root_.scala.collection.mutable.ListBuffer

class TestRunner(clearDB: Box[() => Any], setupDB: Box[() => Any],beforeAssertListeners: List[String => Any],  afterAssertListeners: List[(String, Boolean) => Any],
    beforeTestListeners: List[String => Any], afterTestListeners: List[(String, Boolean, Box[Throwable], List[StackTraceElement]) => Any]) {
  implicit def fToItem(f: () => Any): Item = Item(f)

  def setup[T](what: List[Item]): (() => TestResults, (String,() => T) => T)  = {
  val log = new ListBuffer[Tracker]

   def beforeAssert(name: String): Unit = synchronized {
      log += Tracker(name, true, true, true, Empty, Nil)
      beforeAssertListeners.foreach(_(name))
    }

    def afterAssert(name: String, success: Boolean): Unit = synchronized {
      log += Tracker(name, true, false, success, Empty, Nil)
      afterAssertListeners.foreach(_(name, success))
    }

    def applyAssert(name: String, f:() => T): T = synchronized {
      var success = false
      beforeAssert(name)
      try {
        val ret = f()
        success = true
        ret
      } finally {
        afterAssert(name, success)
      }
    }

    def beforeTest(name: String) {
      log += Tracker(name, false, true, true, Empty, Nil)
      beforeTestListeners.foreach(_(name))
    }

    def afterTest(name: String, success: Boolean, excp: Box[Throwable], trace: List[StackTraceElement]) {
      log += Tracker(name, false, false, success, excp, trace)
      afterTestListeners.foreach(_(name, success, excp, trace))
    }



  def run: TestResults = {

    def doResetDB {
      clearDB.foreach(_())
      setupDB.foreach(_())
    }

    doResetDB


    def runASingleTest(testItem: Item) {
      beforeTest(testItem.name)

      val myTrace = (try{throw new Exception("")} catch {case e => e}).getStackTrace.toList.tail.head
      if (testItem.resetDB) doResetDB
      val (success, trace, excp) = try {
        testItem.getFunc(0)()
        (true, Nil, Empty)
      } catch {
        case e =>
        def combineStack(ex: Throwable,base: List[StackTraceElement]): List[StackTraceElement] = ex match {
          case null => base
          case e => combineStack(e.getCause,e.getStackTrace.toList ::: base)
        }
        val trace = combineStack(e, Nil).takeWhile(e => e.getClassName != myTrace.getClassName || e.getFileName != myTrace.getFileName || e.getMethodName != myTrace.getMethodName).dropRight(2)
        (false, trace, Full(e))
      }

      afterTest(testItem.name, success, excp, trace)
    }

    def runForkTest(testItem: Item, cnt: Int) {
      val threads = for (n <- (1 to cnt).toList) yield {
        val thread = new Thread(new Runnable {def run {
      beforeTest(testItem.name+" thread "+n)

      val myTrace = (try{throw new Exception("")} catch {case e => e}).getStackTrace.toList.tail.head
      if (testItem.resetDB) doResetDB
      val (success, trace, excp) = try {
        testItem.getFunc(n)()
        (true, Nil, Empty)
      } catch {
        case e =>
        def combineStack(ex: Throwable,base: List[StackTraceElement]): List[StackTraceElement] = ex match {
          case null => base
          case e => combineStack(e.getCause,e.getStackTrace.toList ::: base)
        }
        val trace = combineStack(e, Nil).takeWhile(e => e.getClassName != myTrace.getClassName || e.getFileName != myTrace.getFileName || e.getMethodName != myTrace.getMethodName).dropRight(2)
        (false, trace, Full(e))
      }

      afterTest(testItem.name+" thread "+n, success, excp, trace)
      }
        })
        thread.start
        thread
      }

      def waitAll(in: List[Thread]) {
        in match {
          case Nil =>
          case x :: xs => x.join; waitAll(xs)
        }
      }

      waitAll(threads)
    }

    what.foreach{
      testItem =>

      testItem.forkCnt match {
        case 0 => runASingleTest(testItem)
        case n => runForkTest(testItem, n)
      }


    }
    TestResults(log.toList)
  }

  (run _, applyAssert _)
}
}

case class TestResults(res: List[Tracker]) {
  def stats = {
    val rev = res.reverse
    val start = res.map(_.at).reduceLeft((a: Long, b: Long) => if (a < b) a else b)
    val end = res.map(_.at).reduceLeft((a: Long, b: Long) => if (a > b) a else b)
    val assertCnt = res.filter(a => a.isAssert && !a.isBegin).length
    val testCnt = res.filter(a => a.isTest && !a.isBegin).length
    val failedAsserts = res.filter(a => a.isAssert && !a.success)
    val failedTests = res.filter(a => a.isTest && !a.success)

    val append = (failedTests, failedAsserts) match {
      case (ft,fa) if ft.length == 0 && fa.length == 0 => ""
      case (ft, fa) =>
        "\n"+ft.length+" Failed Tests:\n"+ft.map(v => v.name+" "+v.exception.open_!.getMessage+" \n"+
          v.trace.map(st => "           "+st.toString).mkString("\n")).mkString("\n")
    }

    "Ran "+testCnt+" tests and "+assertCnt+" asserts in "+(end - start)/1000L+" seconds"+append
  }
}

class TestFailureError(msg: String) extends _root_.java.lang.Error(msg)

class Item(val name: String, val resetDB: Boolean, val func: Box[() => Any], val forkCnt: Int, forkFunc: Box[Int => Any]) {
  def getFunc(cnt: Int) = {
    (func, forkFunc) match {
      case (Full(f), _) => f
      case (_, Full(cf)) => () => cf(cnt)
      case _ => () =>
    }
  }
}

object Item {
  private var _cnt = 0
  private def cnt() = synchronized {
    _cnt = _cnt + 1
    _cnt
  }
  def apply(f: () => Any) = new Item("Test "+cnt(), false,Full(f), 0, Empty)
  def apply(name: String, f: () => Any) = new Item(name, false, Full(f), 0, Empty)
  def apply(name: String, resetDB: Boolean, f: () => Any) = new Item(name, resetDB, Full(f), 0, Empty)
  def apply(theCnt: Int, f: Int => Any) = new Item("Test "+cnt(), false, Empty, theCnt, Full(f))
  def apply(name: String, theCnt: Int, f: Int => Any) = new Item(name, false, Empty, theCnt, Full(f))
  def apply(name: String, resetDB: Boolean, theCnt: Int, f: Int => Any) = new Item(name, resetDB, Empty, theCnt, Full(f))
}

case class Tracker(name: String, isAssert: Boolean, isBegin: Boolean, success: Boolean,
    exception: Box[Throwable], trace: List[StackTraceElement]) {
  val at = millis
  def isTest = !isAssert
}

}
}
}
