/*
 * Copyright 2010-2011 WorldWide Conferencing, LLC
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
package mapper

import org.specs2.mutable.Specification

import common._
import json._
import util._
import Helpers._
import view._


/**
 * Systems under specification for ItemsList.
 */
object ItemsListSpec extends Specification  {
  "ItemsList Specification".title
  sequential

  val provider = DbProviders.H2MemoryProvider

  def init = {
    provider.setupDB
    Schemifier.destroyTables_!!(DefaultConnectionIdentifier, Schemifier.neverF _,  SampleItem)
    Schemifier.schemify(true, Schemifier.neverF _, SampleItem)
    new ItemsList[SampleItem] {
      def metaMapper = SampleItem
    }
  }
  
  "ItemsList" should {
    "buffer items to save" in {
      val il = init
      il.add
      il.add
      il.add
      il.current.length must_== 0
      il.added.length must_== 3

      il.save
      SampleItem.count must_== 3
      il.current.length must_== 3
    }

    "correctly handle removing an unsaved item" in {
      val il = init
      il.add
      il.add
      il.add
      il.save

      il.add
      il.add
      il.add
      il.remove(il.added(1))
      il.remove(il.added(0))
      il.save
      SampleItem.count must_== 4
      il.added.length must_== 0
      il.removed.length must_== 0
    }
  }
  
}

class SampleItem extends LongKeyedMapper[SampleItem] with IdPK {
  def getSingleton = SampleItem
  object field extends MappedInt(this)
}

object SampleItem extends SampleItem with LongKeyedMetaMapper[SampleItem] {
  var counter = 0
  override def create = {
    val x: SampleItem = super.create
    x.field(counter)
    counter += 1
    x
  }
}

