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
package util {

import _root_.scala.xml.{NodeSeq}
import _root_.java.util.{Locale}
import common._

trait TemplateCache[K, V] {

  type T = K

  /**
   * Returns a cached template by a key. If the template is not cached yet,
   * it will be provided by templateProvider.
   */
  def get(key: K): Box[V];

  /**
   * Adds the node in the cache
   */
  def set(key: K, node: V): V

  def update(key: K, node: V): V = set(key, node)

  /**
   * Removes a template from the cache
   */
  def delete(key: K): Unit
}

/**
 * A cache that caches nothing
 */
object NoCache extends TemplateCache[(Locale, List[String]), NodeSeq] {

  def get(key: T): Box[NodeSeq] = Empty

  def set(key: T, node: NodeSeq): NodeSeq = node

  def delete(key: T) {
  }
}

/**
 * Companion module for InMemoryCache
 */
object InMemoryCache {
  def apply(templatesCount: Int) = new InMemoryCache(templatesCount)
}

/**
 * Caches templates in a LRU map
 */
class InMemoryCache(templatesCount: Int) extends
TemplateCache[(Locale, List[String]), NodeSeq] {
  private val cache : LRU[(Locale, List[String]), NodeSeq] = new LRU(templatesCount)

  def get(key: T): Box[NodeSeq] = {
    cache.synchronized {
      cache.get(key)
    }
  }

  def set(key: T, node: NodeSeq): NodeSeq = cache.synchronized {
    cache(key) = node
    node
  }

  override def delete(key: T) {
    cache.synchronized(cache.remove(key))
  }

}

}
}
