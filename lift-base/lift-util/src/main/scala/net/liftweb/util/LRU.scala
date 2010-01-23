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
 * LRU Cache wrapping {@link org.apache.commons.collections.map.LRUMap}
 *
 * @param size the maximum number of Elements allowed in the LRU map
 * @param loadFactor the Load Factor to construct our LRU with.
 */
class LRU[KeyType, ValueType](size: Int, loadFactor: Box[Float]) extends net.liftweb.common.LRUMap[KeyType, ValueType](size, loadFactor) {
  // Alternate constructor that gives you no load factor.
  def this(size: Int) = this(size, Empty)

  /*

  private val map = loadFactor match {
    case Full(lf) => new LRUMap(size, lf)
    case _ => new LRUMap(size)
  }

  def update(k: KeyType, v: ValueType) {
    map.put(k, v)
  }

  def remove(k: KeyType) = map.remove(k)

  def get(k: KeyType): Box[ValueType] =
  if (map.containsKey(k)) Full(this.apply(k))
  else Empty

  def apply(k: KeyType): ValueType = map.get(k).asInstanceOf[ValueType]
  def contains(k: KeyType): Boolean = map.containsKey(k)
  def keys: List[KeyType] = map.keySet().toList.map(_.asInstanceOf[KeyType])
  */
}

}
}
