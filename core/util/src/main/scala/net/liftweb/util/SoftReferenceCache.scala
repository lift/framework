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

import _root_.java.lang.ref.{ReferenceQueue,SoftReference};
import _root_.java.util._
import _root_.java.util.concurrent.locks._
import _root_.net.liftweb.util._
import Map._
import Helpers._
import ActorPing._
import common._

/**
 * Companion module that has the role of monitoring garbage collected references and remove the orphaned
 * keys from the cache. The monitor is started by calling <i>initialize</i> function and terminated by
 * calling <i>shutDown</i>. It monitors all SoftReferenceCache instances in the context of the same classloader.
 * It can also be used as a factory for obtaining new instances of SoftReferenceCache class
 */
object SoftReferenceCache {

  @volatile
  private var terminated = false;

  private[util] val refQueue = new ReferenceQueue[Any]();

  /**
   * Create a new SoftReferenceCache instance
   */
  def apply[K, V](size: Int) = new SoftReferenceCache[K,V](size)

  /**
   * Initialize the orphan keys monitor
   */
  def initialize = {
    // A daemon thread is more approapriate here then an Actor as
    // we'll do blocking reads from the reference queue
    val thread = new Thread(new Runnable(){
                              def run(){
                                processQueue
                              }
                            })
    thread.setDaemon(true)
    thread.start
  }

  /**
   * ShutDown the monitoring
   */
  def shutDown = {
    terminated = true;
  }

  private def processQueue {
    while (!terminated) {
      tryo {
        // Wait 30 seconds for something to appear in the queue.
        val sftVal = refQueue.remove(30000).asInstanceOf[SoftValue[_,_]];
        if (sftVal != null) {
          sftVal.cache.remove(sftVal.key);
        }
      }
    }
  }
}

case object ProcessQueue
case object Done

/**
 * A Map that holds the values as SoftReference-s. It also applies a LRU policy for the cache entries.
 */
class SoftReferenceCache[K, V](cacheSize: Int) {

  val cache = new LinkedHashMap[K, SoftValue[K, V]]() {
    override def removeEldestEntry(eldest: Entry[K, SoftValue[K, V]]): Boolean = {
      return size() > cacheSize;
    }
  }

  val rwl = new ReentrantReadWriteLock();

  val readLock = rwl.readLock
  val writeLock = rwl.writeLock

  private def lock[T](l: Lock)(block: => T): T = {
    l lock;
    try {
      block
    } finally {
      l unlock
    }
  }

  /**
   * Returns the cached value mapped with this key or Empty if not found
   *
   * @param key
   * @return Box[V]
   */
  def apply(key: K): Box[V] = lock(readLock) {
    Box.!!(cache.get(key)) match {
      case Full(value) =>
         Box.!!(value.get) or {
            remove(key);
            Empty
         }
      case _ => Empty
    }
  }

  /**
   * Puts a new keyed entry in cache
   * @param tuple: (K, V)*
   * @return this
   */
  def += (tuple: (K, V)*) = {
    lock(writeLock) {
      for (t <- tuple) yield {
        cache.put(t._1, new SoftValue(t._1, t._2, this, SoftReferenceCache.refQueue));
      }
    }
    this
  }

  /**
   * Removes the cache entry mapped with this key
   *
   * @returns the value removed
   */
  def remove(key: Any): Box[V] = {
    lock(writeLock) {
      for {value <- Box.!!(cache.remove(key).asInstanceOf[SoftValue[K, V]])
           realValue <- Box.!!(value.get)
      } yield realValue
    }
  }


  def keys = cache.keySet

}

class SoftValue[K, V](k: K,
                      v: V,
                      lruCache: SoftReferenceCache[K, V],
                      queue: ReferenceQueue[Any]) extends SoftReference[V](v, queue) {
    def key: K = k
    def cache: SoftReferenceCache[K, V] = lruCache
}

}
}
