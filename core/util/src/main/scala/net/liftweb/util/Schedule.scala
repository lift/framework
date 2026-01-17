/*
 * Copyright 2007-2011 WorldWide Conferencing, LLC
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

import java.util.concurrent._

import actor.ThreadPoolRules
import common._
import util.Helpers
import util.Helpers.TimeSpan

class ScheduleJBridge {
  def schedule: Schedule = Schedule
}

/**
 * The Schedule object schedules an actor to be ping-ed with a given message after a specified
 * delay. If you need recurrent scheduled pings you will need to reschedule.
 * 
 * The schedule methods return a ScheduledFuture object which can be cancelled if necessary
 */
object Schedule extends Schedule

/**
 * The Schedule object schedules an actor to be ping-ed with a given message after a specified
 * delay. If you need recurrent scheduled pings you will need to reschedule.
 * 
 * The schedule methods return a ScheduledFuture object which can be cancelled if necessary
 */
sealed trait Schedule extends Loggable {

  /**
   * Set this variable to the number of threads to allocate in the thread pool
   */
  @volatile var threadPoolSize = 16 // issue 194

  @volatile var maxThreadPoolSize = threadPoolSize * 25

  /**
   * If it's Full, then create a ArrayBlockingQueue
   * otherwith create a LinkedBlockingQueue.  Default
   * to Full(200000)
   */
  @volatile var blockingQueueSize: Box[Int] = Full(200000)
  
  @volatile var buildExecutor: () => ExecutorService =
    () => new ThreadPoolExecutor(threadPoolSize, 
                                 maxThreadPoolSize,
                                 60,
                                 TimeUnit.SECONDS,
                                 blockingQueueSize match {
                                   case Full(x) => 
                                     new ArrayBlockingQueue(x)
                                   case _ => new LinkedBlockingQueue
                                 })
  
    

  /** The underlying <code>java.util.concurrent.ScheduledExecutor</code> */
  @volatile var buildService: () => ScheduledExecutorService = () => Executors.newSingleThreadScheduledExecutor(TF)

  private var service: ScheduledExecutorService = buildService()
  private var pool = buildExecutor()

  /**
   * Re-create the underlying <code>SingleThreadScheduledExecutor</code>
   */
  def restart: Unit = synchronized { if ((service eq null) || service.isShutdown)
    if ((service eq null) || service.isShutdown)
      service = buildService()
    if ((pool eq null) || pool.isShutdown)
      pool = buildExecutor()
  }


  /**
   * Shut down the underlying <code>SingleThreadScheduledExecutor</code>
   */
  def shutdown(): Unit = synchronized {
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
  def perform[T](to: SimpleActor[T], msg: T, delay: Long): ScheduledFuture[Unit] =
  this.schedule(() => Helpers.tryo( to ! msg ), TimeSpan(delay))

   /**
   * Schedules the sending of a message to occur after the specified delay.
   *
   * @return a <code>ScheduledFuture</code> which applies the function f
   * after delay
   */
  def perform(f: () => Unit, delay: Long): ScheduledFuture[Unit] =
    schedule(f, TimeSpan(delay))


  /**
   * Schedules the application of a function
   *
   * @return a <code>ScheduledFuture</code> which executes the function f
   * immediately on a worker thread
   */
  def apply(f: () => Unit): ScheduledFuture[Unit] = schedule(f, TimeSpan(0))
  
  /**
   * Schedules the application of a function
   *
   * @return a <code>ScheduledFuture</code> which executes the function f
   * after the delay
   */
  def apply(f: () => Unit, delay: TimeSpan): ScheduledFuture[Unit] = 
    schedule(f, delay)
  
  /**
   * Schedules the application of a function
   *
   * @return a <code>ScheduledFuture</code> which executes the function f
   * after the delay
   */
  def schedule(f: () => Unit, delay: TimeSpan): ScheduledFuture[Unit] = 
    synchronized {
      val r = new Runnable {
        def run(): Unit =  { 
          try {
            f.apply()
          } catch {
            case e: Exception => logger.error(e.getMessage, e)
          }
        }
      }
      
      val fast = new java.util.concurrent.Callable[Unit] {
        def call(): Unit = {
          try {
            Schedule.this.restart
            pool.execute(r)
          } catch {
            case e: Exception => logger.error(e.getMessage, e)
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
    d setName "Lift Scheduler"
    d setDaemon true

    if (ThreadPoolRules.nullContextClassLoader) {
      d setContextClassLoader null
    }
    d
  }
}

