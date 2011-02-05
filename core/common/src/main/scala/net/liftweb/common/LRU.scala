/*
 * Copyright 2010 WorldWide Conferencing, LLC
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

private[common] trait LinkedListElem[T1, T2] {
  private[common] var _prev: LinkedListElem[T1, T2] = null
  private[common] var _next: LinkedListElem[T1, T2] = null
  private[common] def value1: T1
  private[common] var value2: T2 = _


  private[common] def remove {
    _prev._next = _next
    _next._prev = _prev
  }

  private[common] def addAtHead(what: LinkedListElem[T1, T2]) {
    what._next = _next
    what._prev = this
    _next._prev = what
    this._next = what
    what
  }

  private[common] def addAtTail(what: LinkedListElem[T1, T2]) {
    what._prev = _prev
    what._next = this
    _prev._next = what
    this._prev = what
    what
  }
}



/**
 * Implements an LRU Hashmap
 */
class LRUMap[K, V](initMaxSize: Int, loadFactor: Box[Float], expiredFunc: ((K, V) => Unit)*) extends LinkedListElem[K, V] {
  import java.util.HashMap

  def this(size: Int) = this(size, Empty)

  private var _maxSize = initMaxSize

  def maxSize = _maxSize

  def updateMaxSize(newMaxSize: Int) {
    val oldMaxSize = _maxSize
    _maxSize = newMaxSize

    if (newMaxSize < oldMaxSize) {
      doRemoveIfTooMany()
    }
  }

  _prev = this
  _next = this

  private[common] def value1: K = throw new NullPointerException("Foo")


  private[this] val localMap = new HashMap[K, LinkedListElem[K, V]](maxSize / 4, loadFactor openOr 0.75f)

  def get(key: K): Box[V] = localMap.get(key) match {
    case null => Empty
    case v =>
      v.remove
    addAtHead(v)
    Full(v.value2)
  }

  def apply(key: K) = get(key).open_!

  def contains(key: K): Boolean = localMap.containsKey(key)

  def -(key: K) = remove(key)

  def remove(key: K) {
    localMap.get(key) match {
      case null =>
	case v =>
          v.remove
      localMap.remove(key)
    }
  }

  def update(key: K, value: V) {
    localMap.get(key) match {
      case null =>
        val what = new LinkedListElem[K, V] {def value1 = key}
      what.value2 = value
      addAtHead(what)
      localMap.put(key, what)

      doRemoveIfTooMany()

      case v =>
        v.remove
      addAtHead(v)
      v.value2 = value
    }
  }

  /**
   * Override this method to implement a test to see if a particular
   * element can be expired from the cache
   */
  protected def canExpire(k: K, v: V): Boolean = {
    true
  }

  /**
   * A mechanism for expiring elements from cache.  This method
   * can devolve into O(n ^ 2) if lots of elements can't be
   * expired
   */
  private def doRemoveIfTooMany() {
    while (localMap.size > maxSize) {
      var toRemove = _prev
      while (!canExpire(toRemove.value1, toRemove.value2)) {
        toRemove = toRemove._prev
        if (toRemove eq this) return
      }
      toRemove.remove
      localMap.remove(toRemove.value1)
      expired(toRemove.value1, toRemove.value2)
      expiredFunc.foreach(_(toRemove.value1, toRemove.value2))
    }
  }

  /**
   * Called when a key/value pair is removed
   */
  protected def expired(key: K, value: V) {

  }

  def keys: List[K] = elements.toList.map(_._1)

  def elements: Iterator[(K, V)] = {
    val set = localMap.entrySet.iterator
    new Iterator[(K, V)] {
      def hasNext = set.hasNext
      def next: (K, V) = {
        val k = set.next
        (k.getKey, k.getValue.value2)
      }
    }
  }

  def size: Int = localMap.size

}

}
}
