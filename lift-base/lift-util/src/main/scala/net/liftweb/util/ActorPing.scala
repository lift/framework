/*
 * Copyright 2007-2010 WorldWide Conferencing, LLC
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

import _root_.java.util.concurrent._
import Helpers.TimeSpan
import common._

/**
 * The ActorPing object schedules an actor to be ping-ed with a given message after a specified
 * delay. If you need recurrent scheduled pings you will need to reschedule.
 * 
 * The schedule methods return a ScheduledFuture object which can be cancelled if necessary
 */
object ActorPing extends Loggable {

  /**
   * Set this variable to the number of threads to allocate in the thread pool
   */
  @volatile var threadPoolSize = 16 // issue 194

  @volatile var maxThreadPoolSize = threadPoolSize * 25
  
  @volatile var buildExecutor: () => ThreadPoolExecutor =
    () => new ThreadPoolExecutor(threadPoolSize, 
                                 maxThreadPoolSize,
                                 60,
                                 TimeUnit.SECONDS,
                                 new LinkedBlockingQueue)
  
    

  /** The underlying <code>java.util.concurrent.ScheduledExecutor</code> */
  private var service: ScheduledExecutorService = Executors.newSingleThreadScheduledExecutor(TF)

  private var pool = buildExecutor()

  /**
   * Re-create the underlying <code>SingleThreadScheduledExecutor</code>
   */
  def restart: Unit = synchronized
  { if ((service eq null) || service.isShutdown)
    service = Executors.newSingleThreadScheduledExecutor(TF) 
   if ((pool eq null) || pool.isShutdown)
     pool = buildExecutor()
 }


  /**
   * Shut down the underlying <code>SingleThreadScheduledExecutor</code>
   */
  def shutdown: Unit = synchronized { 
    service.shutdown 
    pool.shutdown
  }

  /**
   * Schedules the sending of a message to occur after the specified delay.
   *
   * @return a <code>ScheduledFuture</code> which sends the <code>msg</code> to
   * the <code>to<code> Actor after the specified TimeSpan <code>delay</code>.
   */
  def schedule[T](to: SimpleActor[T], msg: T, delay: TimeSpan): ScheduledFuture[Unit] =
  this.schedule(() => Helpers.tryo( to ! msg ), delay)

   /**
   * Schedules the sending of a message to occur after the specified delay.
   *
   * @return a <code>ScheduledFuture</code> which sends the <code>msg</code> to
   * the <code>to<code> Actor after the specified TimeSpan <code>delay</code>.
   */
  def schedule(f: () => Unit, delay: TimeSpan): ScheduledFuture[Unit] = synchronized {
    val r = new Runnable {
      def run() { 
        try {
          f.apply()
        } catch {
          case e: Exception => logger.error(e)
        }
      }
    }

    val fast = new _root_.java.util.concurrent.Callable[Unit] {
      def call(): Unit = {
        try {
          ActorPing.this.restart
          pool.execute(r)
        } catch {
          case e: Exception => logger.error(e)
        }
      }
    }

    try {
      this.restart
      service.schedule(fast, delay.millis, TimeUnit.MILLISECONDS)
    } catch {
      case e: RejectedExecutionException => throw ActorPingException("ping could not be scheduled", e)
    }
  }
}

/**
 * Send by the scheduled actor to sign off from recurrent scheduling
 */
case object UnSchedule

/**
 * Send to the actor that we scheduled for recurrent ping
 */
case object Scheduled

/**
 * Exception thrown if a ping can't be scheduled.
 */
case class ActorPingException(msg: String, e: Throwable) extends RuntimeException(msg, e)

private object TF extends ThreadFactory {
  val threadFactory = Executors.defaultThreadFactory()
  def newThread(r: Runnable) : Thread = {
    val d: Thread = threadFactory.newThread(r)
    d setName "ActorPing"
    d setDaemon true
    d
  }
}

}
}
