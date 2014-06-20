/*
 * Copyright 2006-2011 WorldWide Conferencing, LLC
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

import scala.collection.mutable.HashSet
import util.FatLazy
import common._

class HasManyThrough[From <: KeyedMapper[ThroughType, From],
                     To <: Mapper[To],
                     Through <: Mapper[Through],
                     ThroughType <: Any](
                      owner: From,
                      otherSingleton: MetaMapper[To],
                      through: MetaMapper[Through],
                      throughFromField: MappedField[ThroughType, Through],
                      throughToField: MappedField[ThroughType, Through])
  extends LifecycleCallbacks {
  private var theSetList: Seq[ThroughType] = Nil

  private val others = FatLazy[List[To]] {
    DB.use(owner.connectionIdentifier) { conn =>
      val query = "SELECT DISTINCT "+otherSingleton._dbTableNameLC+".* FROM "+otherSingleton._dbTableNameLC+","+
      through._dbTableNameLC+" WHERE "+
      otherSingleton._dbTableNameLC+"."+otherSingleton.indexedField(otherSingleton.asInstanceOf[To]).openOrThrowException("legacy code")._dbColumnNameLC+" = "+
      through._dbTableNameLC+"."+throughToField._dbColumnNameLC+" AND "+
      through._dbTableNameLC+"."+throughFromField._dbColumnNameLC+" = ?"
      DB.prepareStatement(query, conn) { st =>
        owner.getSingleton.indexedField(owner).map { indVal =>
          if (indVal.dbIgnoreSQLType_?)
            st.setObject(1, indVal.jdbcFriendly)
          else
            st.setObject(1, indVal.jdbcFriendly, indVal.targetSQLType)

          DB.exec(st) { rs =>
            otherSingleton.createInstances(owner.connectionIdentifier, rs, Empty, Empty)
          }
        } openOr Nil
      }
    }
  }

  def apply(): List[To] = others.get

  def get: List[To] = this()

  def reset = others.reset

  def set(what: Seq[ThroughType]): Seq[ThroughType] = {
    theSetList = what
    theSetList
  }

  override def beforeDelete {
    through.findAll(By(throughFromField, owner.primaryKeyField.get)).foreach {
      toDelete => toDelete.delete_!
    }
  }

  override def afterUpdate {
    val current = through.findAll(By(throughFromField, owner.primaryKeyField.get))

    val newKeys = new HashSet[ThroughType];

    theSetList.foreach(i => newKeys += i)
    val toDelete = current.filter(c => !newKeys.contains(throughToField.actualField(c).get))
    toDelete.foreach(_.delete_!)

    val oldKeys = new HashSet[ThroughType];
    current.foreach(i => oldKeys += throughToField.actualField(i).get)

    theSetList.toList.distinct.filter(i => !oldKeys.contains(i)).foreach { i =>
      val toCreate = through.createInstance
      throughFromField.actualField(toCreate).set(owner.primaryKeyField.get)
      throughToField.actualField(toCreate).set(i)
      toCreate.save
    }

    theSetList = Nil
    others.reset
    super.afterUpdate
  }

  override def afterCreate {
    theSetList.toList.distinct.foreach { i =>
      val toCreate = through.createInstance
      throughFromField.actualField(toCreate)(owner.primaryKeyField.get)
      throughToField.actualField(toCreate)(i)
      toCreate.save
    }
    theSetList = Nil
    others.reset
    super.afterCreate
  }
}

