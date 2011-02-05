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
package util {

import common._


/**
 * A simple Read-through cache.
 *
 * An example of using it with a ProtoUser subclass:
 *
 * object UserCache extends KeyedCache[Long, User](100, Full(0.75f),
 *   (id: Long) => User.find(By(User.id, id)))
 *
 * @param size the size of the cache
 * @param loadFactor the optional Load Factor
 * @param cons A function that will take a value of type K and return a Box[T]
 *   populated into the cache if the return value is Full.
 *
 * @author Steve Jenson (stevej@pobox.com)
 */
class KeyedCache[K, T](size: Int, loadFactor: Box[Float], cons: K => Box[T]) {
  val cache = new LRU[K, T](size, loadFactor)

  /**
   * Evict a value from the cache.
   */
  def remove(key: K) = cache.remove(key)

  /**
   * Update the cache yourself with KeyedCache(1) = "hello"
   */
  def update(key: K, value: T) = cache.update(key, value)

  /**
   * If the cache contains a value mapped to {@code key} then return it,
   * otherwise run cons and add that value to the cache and return it.
   */
  def apply(key: K): Box[T] = if (cache.contains(key)) {
    Full(cache(key))
  } else {
    cons(key) match {
      case f@Full(v) => cache.update(key, v); f
      case _ => Empty
    }
  }
}

}
}
