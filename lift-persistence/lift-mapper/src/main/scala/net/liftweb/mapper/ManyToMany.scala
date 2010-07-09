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

/**
 * Add this trait to a Mapper to add support for many-to-many relationships
 * @author nafg
 */
trait ManyToMany extends BaseKeyedMapper {
  this: KeyedMapper[_, _] =>

  type K = TheKeyType
  type T = KeyedMapperType

  private var manyToManyFields: List[MappedManyToMany[_,_,_]] = Nil

  /**
   * An override for save to propagate the save to all children
   * of this parent.
   * Returns false as soon as the parent or a one-to-many field returns false.
   * If they are all successful returns true.
   */
  abstract override def save = {
    super.save &&
      manyToManyFields.forall(_.save)
  }

  /**
   * An override for delete_! to propogate the deletion to all children
   * of this parent.
   * Returns false as soon as the parent or a one-to-many field returns false.
   * If they are all successful returns true.
   */
  abstract override def delete_! = {
    super.delete_! &&
      manyToManyFields.forall( _.delete_!)
  }


  /**
   * This is the base class to use for fields that track many-to-many relationships.
   * @param joinMeta The singleton of the join table
   * @param thisField The foreign key in the join table that refers to this mapper's primaryKey.
   * @param otherField The foreign key in the join table that refers to the other mapper's primaryKey
   * @param otherMeta The singleton of the other mapper
   * @param qp Any QueryParams to limit entries in the join table (other than matching thisField to primaryKey)
   * To limit children based on fields in the other table (not the join table), it is currently necessary
   * to point the join mapper to a view which pulls the join table's fields as well as fields of the other table.
   */
  class MappedManyToMany[O<:Mapper[O], K2, T2 <: KeyedMapper[K2,T2]](
    val joinMeta: MetaMapper[O],
    thisField: MappedForeignKey[K,O,_ <: KeyedMapper[_,_]],
    val otherField: MappedForeignKey[K2, O, T2],
    val otherMeta: MetaMapper[T2],
    val qp: QueryParam[O]*) extends scala.collection.mutable.Buffer[T2] {

    def field(join: O): MappedForeignKey[K,O, _ <: KeyedMapper[_,_]] =
      thisField.actualField(join).asInstanceOf[MappedForeignKey[K,O, _<:KeyedMapper[_,_]]]

    protected def children: List[T2] = {
      joins.flatMap {
        otherField.actualField(_).asInstanceOf[MappedForeignKey[K2,O,T2]].obj
      }
    }

    protected var _joins: List[O] = _
    def joins = _joins // read only to the public
    protected var removedJoins: List[O] = Nil
    refresh
    manyToManyFields = this :: manyToManyFields

    protected def isJoinForChild(e: T2)(join: O) = otherField.actualField(join).is == e.primaryKeyField.is
    protected def joinForChild(e: T2): Option[O] =
      joins.find(isJoinForChild(e))

    protected def own(e: T2): O = {
      joinForChild(e) match {
        case None =>
          removedJoins.find { // first check if we can recycle a removed join
            otherField.actualField(_).is == e.primaryKeyField
          } match {
            case Some(removedJoin) =>
              removedJoins = removedJoins filter removedJoin.ne
              removedJoin // well, noLongerRemovedJoin...
            case None =>
              val newJoin = joinMeta.create
              field(newJoin).set(ManyToMany.this.primaryKeyField.is)
              otherField.actualField(newJoin).set(e.primaryKeyField.is)
              newJoin
          }
        case Some(join) =>
          join
      }
    }
    protected def unown(e: T2) = {
      joinForChild(e) match {
        case Some(join) =>
          removedJoins = join :: removedJoins
          val o = otherField.actualField(join)
          o.set(o.defaultValue)
          val f = field(join)
          f.set(f.defaultValue)
          Some(join)
        case None =>
          None
      }
    }

    def all = children

    // 2.7
    //def readOnly = all

    def length = children.length
    // 2.7
    //def elements = children.elements
    // 2.8
    def iterator = children.iterator

    protected def childAt(n: Int) = children(n)
    def apply(n: Int) = childAt(n)
    def indexOf(e: T2) =
      children.findIndexOf(e eq)

    // 2.7
    // def insertAll(n: Int, iter: Iterable[T2]) {
    // 2.8
    def insertAll(n: Int, traversable: Traversable[T2]) {
      val ownedJoins = traversable map own
      val n2 = joins.findIndexOf(isJoinForChild(children(n)))
      val before = joins.take(n2)
      val after = joins.drop(n2)

      _joins = before ++ ownedJoins ++ after
    }

    // 2.7
    // def +:(elem: T2) = {
    // 2.8
    def +=:(elem: T2) = {
      _joins ::= own(elem)
      this
    }

    def +=(elem: T2) = {
      _joins ++= List(own(elem))
      this
    }

    def update(n: Int, newelem: T2) {
      unown(childAt(n)) match {
        case Some(join) =>
          val n2 = joins.indexOf(join)
          val (before, after) = (joins.take(n2), joins.drop(n2+1))
          _joins = before ++ List(own(newelem)) ++ after
        case None =>
      }
    }

    def remove(n: Int) = {
      val child = childAt(n)
      unown(child) match {
        case Some(join) =>
          _joins = joins filterNot join.eq
        case None =>
      }
      child
    }


    def clear() {
      children foreach unown
      _joins = Nil
    }

    def refresh = {
      val by = new Cmp[O, TheKeyType](thisField, OprEnum.Eql, Full(primaryKeyField.is), Empty, Empty)

      _joins = joinMeta.findAll( (by :: qp.toList): _*)
      all
    }

    def save = {
      _joins = joins.filter { join =>
          field(join).is ==
            ManyToMany.this.primaryKeyField.is && {
              val f = otherField.actualField(join)
              f.is != f.defaultValue
          }
      }

      removedJoins.forall {_.delete_!} & ( // continue saving even if deleting fails
        children.forall(_.save) &&
          joins.forall(_.save)
      )
    }

    def delete_! = {
      removedJoins.forall(_.delete_!) &
        joins.forall(_.delete_!)
    }
  }
}

}
}
