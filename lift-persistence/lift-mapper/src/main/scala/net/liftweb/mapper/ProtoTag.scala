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

import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import Helpers._

trait MetaProtoTag[ModelType <: ProtoTag[ModelType]] extends KeyedMetaMapper[Long, ModelType] {
  self: ModelType =>
  override def dbTableName: String //  = "tags"
  def cacheSize: Int

  private val idCache = new LRU[Long, ModelType](cacheSize)
  private val tagCache = new LRU[String, ModelType](cacheSize)

  def findOrCreate(ntag: String): ModelType = synchronized {
    val tag = capify(ntag)
    if (tagCache.contains(tag)) tagCache(tag)
    else {
      find(By(name, tag)) match {
        case Full(t) => tagCache(tag) = t; t
        case _ => val ret: ModelType = (createInstance).name(tag).saveMe
          tagCache(tag) = ret
          ret
      }
    }
  }

  override def findDbByKey(dbId: ConnectionIdentifier, key: Long): Box[ModelType] = synchronized {
    if (idCache.contains(key)) Full(idCache(key))
    else {
      val ret = super.findDbByKey(dbId,key)
      ret.foreach(v => idCache(key) = v)
      ret
    }
  }

  /**
  * Split the String into tags
  */
  def split(in: String): List[String] = in.roboSplit(",").map(capify)

  /**
  * Split the String into tags and find all the tags
  */
  def splitAndFind(in: String): List[ModelType] = split(in).map(findOrCreate)

  def capify: String => String = Helpers.capify _
}

abstract class ProtoTag[MyType <: ProtoTag[MyType]] extends KeyedMapper[Long, MyType] with Ordered[MyType] {
  self: MyType =>

  def getSingleton: MetaProtoTag[MyType]

  // the primary key for the database
  object id extends MappedLongIndex(this)

  def primaryKeyField = id

  object name extends MappedPoliteString(this, 256) {
    override def setFilter = getSingleton.capify :: super.setFilter
    override def dbIndexed_? = true
  }

  def compare(other: MyType): Int = name.is.compare(other.name.is)
}

}
}
