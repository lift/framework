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
package squerylrecord

import net.liftweb.record.{Record, MetaRecord}
import net.liftweb.proto.Crudify
import org.squeryl._
import net.liftweb.squerylrecord.RecordTypeMode._
import net.liftweb.record.Field
import net.liftweb.common.{Box, Empty, Full}
import scala.xml.NodeSeq

trait CRUDify[K, T <: Record[T] with KeyedEntity[K]] extends Crudify {
  self: MetaRecord[T] =>

  type TheCrudType = T

  type FieldPointerType = Field[_, TheCrudType]

  def table: Table[TheCrudType]

  def idFromString(in: String): K

  override def calcPrefix = table.name :: Nil

  override def fieldsForDisplay: List[FieldPointerType] = metaFields.filter(_.shouldDisplay_?)

  override def computeFieldFromPointer(instance: TheCrudType, pointer: FieldPointerType): Box[FieldPointerType] = instance.fieldByName(pointer.name)

  override def findForParam(in: String): Box[TheCrudType] =
    inTransaction{
	  table.lookup(idFromString(in))
  	}

  override def findForList(start: Long, count: Int) = 
    inTransaction{
	  from(table)(t => select(t)).page(start.toInt, count).toList
  	}

  override def create = createRecord

  override def buildBridge(in: TheCrudType) = new SquerylBridge(in)

  protected class SquerylBridge(in: TheCrudType) extends CrudBridge {

    def delete_! = inTransaction {
      table.delete(in.id)
    }

    def save = {
      if (in.isPersisted) {
        inTransaction{
        	table.update(in)
        }
      }
      else {
        inTransaction {
        	table.insert(in)
        }
      }
      true
    }

    def validate = in.validate

    def primaryKeyFieldAsString = in.id.toString
  }

  def buildFieldBridge(from: FieldPointerType): FieldPointerBridge = new SquerylFieldBridge(from)

  protected class SquerylFieldBridge(in: FieldPointerType) extends FieldPointerBridge {
    def displayHtml: NodeSeq = in.displayHtml
  }

}
