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
class LAFuture[T] /*extends Future[T]*/ {
  private var item: T = _
  private var satisfied = false
  private var aborted = false
  private var toDo: List[T => Unit] = Nil

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
          toDo
        } else Nil
      } finally {
        notifyAll()
      }
    }
    funcs.foreach(f => LAScheduler.execute(() => f(value)))
  }

  /**
   * Get the future value
   */
  def get: T = synchronized {
    if (satisfied) item
    else {
      this.wait()
      if (satisfied) item
      else if (aborted) throw new AbortedFutureException()
      else get
    }
  }

  /**
   * Execute the function with the value. If the
   * value has not been satisfied, execute the function
   * when the value is satified
   */
  def foreach(f: T => Unit) {
    val todo = synchronized {
      if (satisfied) {
        val v = item
        () => f(v)
      } else {
        toDo ::= f
        () => ()
      }
    }

    todo()
  }

  /**
   * Get the future value or if the value is not
   * satisfied after the timeout period, return an
   * Empty
   */
  def get(timeout: Long): Box[T] = synchronized {
    if (satisfied) Full(item)
    else if (aborted) Empty
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
    synchronized {
      if (!satisfied && !aborted) {
        aborted = true
        notifyAll()
      }
    }
  }
}

/**
 * Thrown if an LAFuture is aborted during a get
 */
final class AbortedFutureException() extends Exception("Aborted Future")

object LAFuture {
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

