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

import common._


/**
 * A container that contains a calculated value
 * or may contain one in the future
 */
class LAFuture[T](val scheduler: LAScheduler) {
  private var item: T = _
  private var failure: Box[Nothing] = Empty
  private var satisfied = false
  private var aborted = false
  private var toDo: List[T => Unit] = Nil
  private var onFailure: List[Box[Nothing] => Unit] = Nil
  private var onComplete: List[Box[T] => Unit] = Nil

  def this() {
    this(LAScheduler)
  }

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
          val ret = toDo
          toDo = Nil
          onFailure = Nil
          onComplete.foreach(f => LAFuture.executeWithObservers(scheduler, () => f(Full(value))))
          onComplete = Nil
          ret
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
  final def get: T = synchronized {
    if (satisfied) item
    else if (aborted) throw new AbortedFutureException(failure)
    else {
      this.wait()
      if (satisfied) item
      else if (aborted) throw new AbortedFutureException(failure)
      else get
    }
  }

  /**
   * Execute the function with the value. If the
   * value has not been satisfied, execute the function
   * when the value is satified
   */
  def foreach(f: T => Unit) {
    onSuccess(f)
  }

  /**
   * Map the future over a function
   * @param f the function to apply to the future
   * @tparam A the type that the function returns
   * @return a Future that represents the function applied to the value of the future
   */
  def map[A](f: T => A): LAFuture[A] = {
    val ret = new LAFuture[A](scheduler)
    onComplete(v => ret.complete(v.map(f)))
    ret
  }

  def flatMap[A](f: T => LAFuture[A]): LAFuture[A] = {
    val ret = new LAFuture[A](scheduler)
    onComplete(v => v match {
      case Full(v) => f(v).onComplete(v2 => ret.complete(v2))
      case e: EmptyBox => ret.complete(e)
    })
    ret
  }

  def filter(f: T => Boolean): LAFuture[T] = {
    val ret = new LAFuture[T](scheduler)
    onComplete(v => ret.complete(v.filter(f)))
    ret
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
        else Empty
      } catch {
        case _: InterruptedException => Empty
      }
    }
  }

  /**
   * Has the future been satisfied
   */
  def isSatisfied: Boolean = synchronized {satisfied}

  /**
   * Has the future been aborted
   */
  def isAborted: Boolean = synchronized {aborted}

  /**
   * Abort the future.  It can never be satified
   */
  def abort() {
    fail(Empty)
  }

  /**
   * Execute the function on success of the future
   *
   * @param f the function to execute on success.
   */
  def onSuccess(f: T => Unit) {
    synchronized {
      if (satisfied) {LAFuture.executeWithObservers(scheduler, () => f(item))} else
      if (!aborted) {
        toDo ::= f
      }
    }
  }

  /**
   * Execute a function on failure
   *
   * @param f the function to execute. Will receive a Box[Nothing] which may be a Failure if there's exception data
   */
  def onFail(f: Box[Nothing] => Unit) {
    synchronized {
      if (aborted) LAFuture.executeWithObservers(scheduler, () => f(failure)) else
      if (!satisfied) {
        onFailure ::= f
      }
    }
  }

  /**
   * A function to execute on completion of the Future, success or failure
   *
   * @param f the function to execute on completion of the Future
   */
  def onComplete(f: Box[T] => Unit) {
    synchronized {
      if (satisfied) {LAFuture.executeWithObservers(scheduler, () => f(Full(item)))} else
      if (aborted) {LAFuture.executeWithObservers(scheduler, () => f(failure))} else
      onComplete ::= f
    }
  }

  /**
   * If the execution fails, do this
   * @param e
   */
  def fail(e: Exception) {
    fail(Failure(e.getMessage, Full(e), Empty))
  }

  /**
   * If the execution fails as a Box[Nothing], do this
   * @param e
   */
  def fail(e: Box[Nothing]) {
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

  /**
   * Has the future completed?
   */
  def complete_? : Boolean = synchronized(satisfied || aborted)
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
  def apply[T](f: () => T, scheduler: LAScheduler = LAScheduler): LAFuture[T] = {
    val ret = new LAFuture[T](scheduler)
    scheduler.execute(() => {
      try {
      ret.satisfy(f())
      } catch {
        case e: Exception => ret.fail(e)
      }
    })
    ret
  }

  /**
   * Build a new future with a call-by-name value that returns a type T
   * @param f the call-by-name code the defines the future
   * @tparam T the type that
   * @return
   */
  def build[T](f: => T, scheduler: LAScheduler = LAScheduler): LAFuture[T] = {
    this.apply(() => f, scheduler)
  }

  private val threadInfo = new ThreadLocal[List[LAFuture[_] => Unit]]

  /**
   * Notify all the observers that we created a future.
   *
   * @param future
   */
  private def notifyObservers(future: LAFuture[_]) {
    val observers = threadInfo.get()
    if (null eq observers) {} else {
      observers.foreach(_(future))
    }
  }

  private def executeWithObservers(scheduler: LAScheduler, f: () => Unit) {
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
   * Collect all the future values into the aggregate future
   * The returned future will be satisfied when all the
   * collected futures are satisfied
   */
  def collect[T](future: LAFuture[T]*): LAFuture[List[T]] = {
    val sync = new Object
    val len = future.length
    val vals = new collection.mutable.ArrayBuffer[Box[T]](len)
     // pad array so inserts at random places are possible
    for (i <- 0 to len) { vals.insert(i, Empty) }
    var gotCnt = 0
    val ret = new LAFuture[List[T]]

    future.toList.zipWithIndex.foreach {
      case (f, idx) => 
        f.foreach {
          v => sync.synchronized {
            vals.insert(idx, Full(v))
            gotCnt += 1
            if (gotCnt >= len) {
              ret.satisfy(vals.toList.flatten)
            }
          }
        }
    }

    ret
  }

  /**
   * Collect all the future values into the aggregate future
   * The returned future will be satisfied when all the
   * collected futures are satisfied or if any of the
   * futures is Empty, then immediately satisfy the
   * returned future with an Empty
   */
  def collectAll[T](future: LAFuture[Box[T]]*): LAFuture[Box[List[T]]] = {
    val sync = new Object
    val len = future.length
    val vals = new collection.mutable.ArrayBuffer[Box[T]](len)
    // pad array so inserts at random places are possible
    for (i <- 0 to len) { vals.insert(i, Empty) }
    var gotCnt = 0
    val ret = new LAFuture[Box[List[T]]]

    future.toList.zipWithIndex.foreach {
      case (f, idx) => 
        f.foreach {
          vb => sync.synchronized {
            vb match {
              case Full(v) => {
                vals.insert(idx, Full(v))
                gotCnt += 1
                if (gotCnt >= len) {
                  ret.satisfy(Full(vals.toList.flatten))
                }
              }
              
              case eb: EmptyBox => {
                ret.satisfy(eb)
              }
            }
          }
        }
    }

    ret
  }
}

