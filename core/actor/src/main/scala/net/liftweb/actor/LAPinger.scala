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
package actor

import java.util.concurrent._

/**
 * Rules for dealing with thread pools, both in lift-actor and
 * in lift-util
 */
object ThreadPoolRules {
  /**
   * When threads are created in the thread factories, should
   * they null the context class loader.  By default false,
   * but it you set it to true, Tomcat complains less about stuff.
   * Must be set in the first line of Boot.scala
   */
  @volatile var nullContextClassLoader: Boolean = false
}

/**
 * LAPinger is for scheduling LiftActors to be pinged with an arbitrary message at some point
 * in the future.
 */
object LAPinger {

  /**The underlying <code>java.util.concurrent.ScheduledExecutor</code> */
  @volatile var buildService: () => ScheduledExecutorService = () => Executors.newSingleThreadScheduledExecutor(TF)

  private var service: ScheduledExecutorService = buildService()

  /**
   * Re-create the underlying <code>SingleThreadScheduledExecutor</code>
   */
  def restart: Unit = synchronized {
    if ((service eq null) || service.isShutdown)
      service = buildService()
  }

  /**
   * Shut down the underlying <code>SingleThreadScheduledExecutor</code>
   */
  def shutdown: Unit = synchronized {
    service.shutdown
  }

  /**
   * Schedules the sending of a message to occur after the specified delay.
   *
   * @param to The LiftActor to send the message to.
   * @param msg The message to send.
   * @param delay The number of milliseconds to delay before sending msg
   * @return a <code>ScheduledFuture</code> which sends the <code>msg</code> to
   * the <code>to<code> Actor after the specified TimeSpan <code>delay</code>.
   */
  def schedule[T](to: SpecializedLiftActor[T], msg: T, delay: Long): ScheduledFuture[Unit] = {
    val r = new Callable[Unit] {
      def call: Unit = {
        to ! msg
      }
    }
    try {
      service.schedule(r, delay, TimeUnit.MILLISECONDS)
    } catch {
      case e: RejectedExecutionException => throw PingerException(msg.toString + " could not be scheduled on " + to, e)
    }
  }

}

/**
 * Exception thrown if a ping can't be scheduled.
 */
case class PingerException(msg: String, e: Throwable) extends RuntimeException(msg, e)

private object TF extends ThreadFactory {
  val threadFactory = Executors.defaultThreadFactory()

  def newThread(r: Runnable): Thread = {
    val d: Thread = threadFactory.newThread(r)
    d setName "ActorPinger"
    d setDaemon true
    if (ThreadPoolRules.nullContextClassLoader) {
      d setContextClassLoader null
    }
    d
  }
}
