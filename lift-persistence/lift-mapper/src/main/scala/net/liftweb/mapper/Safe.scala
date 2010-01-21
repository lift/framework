/*
 * Copyright 2006-2010 WorldWide Conferencing, LLC
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
package mapper {

import _root_.java.security.{SecureRandom, MessageDigest}
import _root_.org.apache.commons.codec.binary.Base64
import _root_.net.liftweb.util.{StringHelpers, Helpers, ThreadGlobal}

/**
 * Manage the current "safety" state of the stack
 */
object Safe {
  private val rand = new SecureRandom
  /**
   * Get the next "safe" number
   */
  def next = rand.nextLong
  private val threadLocal = new ThreadGlobal[Long]

  /**
   * Is the current context "safe" for the object with the
   * given safety code?
   */
  def safe_?(test : Long) : Boolean = test == threadLocal.value

  /**
   * Marks access to a given object as safe for the duration of the function
   */
  def runSafe[T](x : Long)(f : => T) : T = {
     threadLocal.doWith(x)(f)
  }

  def randomString(len: Int): String = StringHelpers.randomString(len)
}

}
}
