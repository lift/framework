/*
 * Copyright 2009-2010 WorldWide Conferencing, LLC
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
package common {

/**
 * The simple definition of an actor.  Something that
 * can receive a message of type T.
 */
trait SimpleActor[-T] {
  /**
   * Send a message to the Actor
   *
   * @param param the message to send
   */
  def !(param: T): Unit
}

/**
 * An Actor that can receive a message of any type
 */
trait SimplestActor extends SimpleActor[Any]

/**
 * An Actor that can receive messsages of type T and
 * return responses of type R.
 */
trait TypedActor[-T, +R] extends SimpleActor[T] {
  def !?(param: T): R

  /**
   * Compatible with Scala Actors' !? method
   */
  def !?(timeout: Long, message: Any): Box[R]


  /**
   * Asynchronous message send. Send-and-receive eventually. Waits on a Future for the reply message.
   * If recevied within the Actor default timeout interval then it returns Some(result) and if a timeout
   * has occured None.
   */
  def !!(message: T): Box[R]

  /**
   * Asynchronous message send. Send-and-receive eventually. Waits on a Future for the reply message.
   * If recevied within timout interval that is specified then it returns Some(result) and if a timeout
   * has occured None.
   */
  def !!(message: T, timeout: Long): Box[R]

}

/**
 * Generic Actor interface. Can receive any type of message.
 * Can return (via !! and !?) messages of type R.
 */
trait GenericActor[+R] extends TypedActor[Any, R]

/**
 * Generic Actor interface. Can receive any type of message.
 * Can return (via !! and !?) messages of any type.
 */
trait SimplestGenericActor extends GenericActor[Any]


trait ForwardableActor[From, To] {
  self: TypedActor[From, To] =>

  protected def forwardMessageTo(msg: From,
                                 forwardTo: TypedActor[From, To]): Unit

  protected def reply(msg: To): Unit
}

}
}
