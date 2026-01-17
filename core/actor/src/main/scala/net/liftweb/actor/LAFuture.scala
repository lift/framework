/*
 * Copyright 2009-2011 WorldWide Conferencing, LLC
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
package actor

import scala.collection.mutable.ArrayBuffer

import common._


/**
 * A container that contains a calculated value
 * or may contain one in the future
 */
class LAFuture[T](val scheduler: LAScheduler = LAScheduler, context: Box[LAFuture.Context] = Empty) {
  private var item: T = _
  private var failure: Box[Nothing] = Empty
  private var satisfied = false
  private var aborted = false
  private var toDo: List[T => Unit] = Nil
  private var onFailure: List[Box[Nothing] => Unit] = Nil
  private var onComplete: List[Box[T] => Unit] = Nil

  LAFuture.notifyObservers(this)

  /**
   * Satify the future... perform the calculation
   * the results in setting a value for the future
   */
  def satisfy(value: T): Unit = {
    val funcs = synchronized {
      try {
        if (!satisfied && !aborted) {
          item = value
          satisfied = true
          val result = toDo
          toDo = Nil
          onFailure = Nil
          onComplete.foreach(f => LAFuture.executeWithObservers(scheduler, () => f(Full(value))))
          onComplete = Nil
          result
        } else Nil
      } finally {
        notifyAll()
      }
    }
    funcs.foreach(f => LAFuture.executeWithObservers(scheduler, () => f(value)))
  }

  /**
   * Complete the Future... with a Box... useful from Helpers.tryo
   * @param value
   */
  def complete(value: Box[T]): Unit = {
      value match {
        case Full(v) => satisfy(v)
        case x: EmptyBox => fail(x)
      }
  }

  /**
   * Get the future value
   */
  @scala.annotation.tailrec
  final def get: T = {
    synchronized {
      if (satisfied) {
        return item
      } else if (aborted) {
        throw new AbortedFutureException(failure)
      } else {
        this.wait()
        if (satisfied) {
          return item
        } else if (aborted) {
          throw new AbortedFutureException(failure)
        }
      }
    }

    get
  }

  /**
   * Execute the function with the value. If the
   * value has not been satisfied, execute the function
   * when the value is satified
   */
  def foreach(f: T => Unit):Unit = {
    onSuccess(f)
  }

  /**
   * Map the future over a function
   * @param f the function to apply to the future
   * @tparam A the type that the function returns
   * @return a Future that represents the function applied to the value of the future
   */
  def map[A](f: T => A): LAFuture[A] = {
    val result = new LAFuture[A](scheduler, context)
    val contextFn = LAFuture.inContext(f, context)
    onComplete(v => result.complete(v.flatMap(n => Box.tryo(contextFn(n)))))
    result
  }

  def flatMap[A](f: T => LAFuture[A]): LAFuture[A] = {
    val result = new LAFuture[A](scheduler, context)
    val contextFn = LAFuture.inContext(f, context)
    onComplete(v => v match {
      case Full(v) =>
        Box.tryo(contextFn(v)) match {
          case Full(successfullyComputedFuture) =>
            successfullyComputedFuture.onComplete(v2 => result.complete(v2))
          case e: EmptyBox =>
            result.complete(e)
        }
      case e: EmptyBox =>
        result.complete(e)
    })
    result
  }

  def filter(f: T => Boolean): LAFuture[T] = {
    val result = new LAFuture[T](scheduler, context)
    onComplete(v => result.complete(v.filter(f)))
    result
  }

  def withFilter(f: T => Boolean): LAFuture[T] = filter(f)

  /**
   * Get the future value or if the value is not
   * satisfied after the timeout period, return an
   * Empty
   */
  def get(timeout: Long): Box[T] = synchronized {
    if (satisfied) Full(item)
    else if (aborted) failure
    else {
      try {
        wait(timeout)
        if (satisfied) Full(item)
        else if (aborted) failure
        else Empty
      } catch {
        case _: InterruptedException => Empty
      }
    }
  }

  /**
   * Java-friendly alias for satisfied_?.
   */
  def isSatisfied: Boolean = satisfied_?

  /**
   * Has the future been satisfied
   */
  def satisfied_? = synchronized {satisfied}
  /**
   * Java-friendly alias for aborted_?.
   */
  def isAborted: Boolean = aborted_?

  /**
   * Has the future been aborted
   */
  def aborted_? = synchronized {aborted}

  /**
   * Java-friendly alias for completed_?.
   */
  def isCompleted: Boolean = completed_?
  /**
   * Has the future completed?
   */
  def completed_? : Boolean = synchronized(satisfied || aborted)

  @deprecated("Please use completed_? instead.", "3.1.0")
  def complete_? : Boolean = completed_?

  /**
   * Abort the future.  It can never be satified
   */
  def abort(): Unit = {
    fail(Empty)
  }

  /**
   * Execute the function on success of the future
   *
   * @param f the function to execute on success.
   */
  def onSuccess(f: T => Unit): Unit = {
    val contextFn = LAFuture.inContext(f, context)
    synchronized {
      if (satisfied) {LAFuture.executeWithObservers(scheduler, () => contextFn(item))} else
      if (!aborted) {
        toDo ::= contextFn
      }
    }
  }

  /**
   * Execute a function on failure
   *
   * @param f the function to execute. Will receive a Box[Nothing] which may be a Failure if there's exception data
   */
  def onFail(f: Box[Nothing] => Unit): Unit = {
    val contextFn = LAFuture.inContext(f, context)
    synchronized {
      if (aborted) LAFuture.executeWithObservers(scheduler, () => contextFn(failure)) else
      if (!satisfied) {
        onFailure ::= contextFn
      }
    }
  }

  /**
   * A function to execute on completion of the Future, success or failure
   *
   * @param f the function to execute on completion of the Future
   */
  def onComplete(f: Box[T] => Unit): Unit = {
    val contextFn = LAFuture.inContext(f, context)
    synchronized {
      if (satisfied) {LAFuture.executeWithObservers(scheduler, () => contextFn(Full(item)))} else
      if (aborted) {LAFuture.executeWithObservers(scheduler, () => contextFn(failure))} else
      onComplete ::= contextFn
    }
  }

  /**
   * If the execution fails, do this
   * @param e
   */
  def fail(e: Exception): Unit = {
    fail(Failure(e.getMessage, Full(e), Empty))
  }

  /**
   * If the execution fails as a Box[Nothing], do this
   * @param e
   */
  def fail(e: Box[Nothing]): Unit = {
    synchronized {
      if (!satisfied && !aborted) {
        aborted = true
        failure = e
        onFailure.foreach(f => LAFuture.executeWithObservers(scheduler, () => f(e)))
        onComplete.foreach(f => LAFuture.executeWithObservers(scheduler, () => f(e)))
        onComplete = Nil
        onFailure = Nil
        toDo = Nil

        notifyAll()
      }
    }
  }
}

/**
 * Thrown if an LAFuture is aborted during a get
 */
final class AbortedFutureException(why: Box[Nothing]) extends Exception("Aborted Future")

object LAFuture {
  /**
   * Create an LAFuture from a function that
   * will be applied on a separate thread. The LAFuture
   * is returned immediately and the value may be obtained
   * by calling `get`
   *
   * @param f the function that computes the value of the future
   * @tparam T the type
   * @return an LAFuture that will yield its value when the value has been computed
   */
  def apply[T](f: () => T, scheduler: LAScheduler = LAScheduler, context: Box[Context] = Empty): LAFuture[T] = {
    val result = new LAFuture[T](scheduler, context)
    val contextFn = inContext(f, context)
    scheduler.execute(() => {
      try {
        result.satisfy(contextFn())
      } catch {
        case e: Exception => result.fail(e)
      }
    })
    result
  }

  /**
   * Build a new future with a call-by-name value that returns a type T
   * @param f the call-by-name code the defines the future
   * @tparam T the type that
   * @return
   */
  def build[T](f: => T, scheduler: LAScheduler = LAScheduler, context: Box[Context] = Empty): LAFuture[T] = {
    this.apply(() => f, scheduler, context)
  }

  private val threadInfo = new ThreadLocal[List[LAFuture[_] => Unit]]

  /**
   * Notify all the observers that we created a future.
   *
   * @param future
   */
  private def notifyObservers(future: LAFuture[_]): Unit = {
    val observers = threadInfo.get()
    if (null eq observers) {} else {
      observers.foreach(_(future))
    }
  }

  private def executeWithObservers(scheduler: LAScheduler, f: () => Unit): Unit = {
    val cur = threadInfo.get()
    scheduler.execute(() => {
      val old = threadInfo.get()
      threadInfo.set(cur)
      try {
        f()
      } finally {
        threadInfo.set(old)
      }
    })
  }

  /**
   * Do something when a future is created on this thread. This can be used
   * to see if there's any Future activity on a thread and if there is,
   * we can do smart things on an observing thread.
   *
   * @param observation the function to execute on Future creation
   * @param toDo the action call-by-name code to execute whi
   * @tparam T the type of the value returned by toDo
   * @return the value computed by toDo
   */
  def observeCreation[T](observation: LAFuture[_] => Unit)(toDo: => T): T = {
    val old = threadInfo.get()
    threadInfo.set(if (null eq old) List(observation) else observation :: old)
    try {
      toDo
    } finally {
      threadInfo.set(old)
    }
  }

  /**
   * Given handlers for a value's success and failure and a set of futures, runs
   * the futures simultaneously and invokes either success or failure callbacks
   * as each future completes. When all futures are complete, if the handlers
   * have not either satisfied or failed the overall result, `onAllFuturesCompleted`
   * is called to complete it. If it *still* isn't complete, the overall result
   * is failed with an error.
   *
   * Note that the success and failure functions are guaranteed to be run in a
   * thread-safe manner. Each is passed the value, the result future, the
   * accumulating `ArrayBuffer`, and the index of the future that has been
   * completed. For the failure handler, the value is the `Box` of the failure.
   */
  def collect[T, A](
    onFutureSucceeded: (T, LAFuture[A], ArrayBuffer[Box[T]], Int)=>Unit,
    onFutureFailed: (Box[Nothing], LAFuture[A], ArrayBuffer[Box[T]], Int)=>Unit,
    onAllFuturesCompleted: (LAFuture[A], ArrayBuffer[Box[T]])=>Unit,
    futures: LAFuture[T]*
  ): LAFuture[A] = {
    val result = new LAFuture[A]

    if (futures.isEmpty) {
      onAllFuturesCompleted(result, new ArrayBuffer[Box[T]](0))
    } else {
      val sync = new Object
      val len = futures.length
      val accumulator = new ArrayBuffer[Box[T]](len)
       // pad array so inserts at random places are possible
      for (i <- 0 to len) { accumulator.insert(i, Empty) }
      var gotCnt = 0

      futures.toList.zipWithIndex.foreach {
        case (future, index) =>
          future.onSuccess {
            value => sync.synchronized {
              gotCnt += 1
              onFutureSucceeded(value, result, accumulator, index)

              if (gotCnt >= len && ! result.completed_?) {
                onAllFuturesCompleted(result, accumulator)

                if (! result.completed_?) {
                  result.fail(Failure("collect invoker did not complete result"))
                }
              }
            }
          }
          future.onFail {
            failureBox => sync.synchronized {
              gotCnt += 1
              onFutureFailed(failureBox, result, accumulator, index)

              if (gotCnt >= len && ! result.completed_?) {
                onAllFuturesCompleted(result, accumulator)

                if (! result.completed_?) {
                  result.fail(Failure("collect invoker did not complete result"))
                }
              }
            }
          }
      }
    }

    result
  }


  /**
   * Collect all the future values into the aggregate future
   * The returned future will be satisfied when all the
   * collected futures are satisfied
   */
  def collect[T](future: LAFuture[T]*): LAFuture[List[T]] = {
    collect[T, List[T]](
      onFutureSucceeded = { (value, result, values, index) =>
        values.insert(index, Full(value))
      },
      onFutureFailed = { (valueBox, result, values, index) => result.fail(valueBox) },
      onAllFuturesCompleted = { (result, values) => result.satisfy(values.toList.flatten) },
      future: _*
    )
  }

  /**
   * Collect all the future values into the aggregate future
   * The returned future will be satisfied when all the
   * collected futures are satisfied or if any of the
   * futures is Empty, then immediately satisfy the
   * returned future with an Empty
   */
  def collectAll[T](future: LAFuture[Box[T]]*): LAFuture[Box[List[T]]] = {
    collect[Box[T], Box[List[T]]](
      onFutureSucceeded = { (value, result, values, index) =>
        value match {
          case Full(realValue) =>
            values.insert(index, Full(Full(realValue)))
          case other: EmptyBox =>
            result.satisfy(other)
        }
      },
      onFutureFailed = { (valueBox, result, values, index) => result.fail(valueBox) },
      onAllFuturesCompleted = { (result: LAFuture[Box[List[T]]], values: ArrayBuffer[Box[Box[T]]]) =>
        result.satisfy(Full(values.toList.flatten.flatten))
      },
      future: _*
    )
  }

  private def inContext[T](f: () => T, context: Box[LAFuture.Context]): () => T = {
    context.map(_.around(f)) openOr f
  }

  private def inContext[A, T](f: (A) => T, context: Box[LAFuture.Context]): (A) => T = {
    context.map(_.around(f)) openOr f
  }

  /**
    * Allows to wrap function in another function providing some additional functionality.
    * It may choose to execute or not execute that functionality, but should not interpret
    * or change the returned value; instead, it should perform orthogonal actions that
    * need to occur around the given functionality. Typical example is setting up DB
    * transaction.
    *
    * This is similar to [[net.liftweb.common.CommonLoanWrapper]], however, it decorates the
    * function eagerly. This way, you can access current thread's state which is essential
    * to do things like set up a HTTP session wrapper
    */
  trait Context {
    def around[T](fn: () => T): () => T
    def around[A, T](fn: (A) => T): (A) => T
  }
}
