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

import common._

import scala.annotation.tailrec
import scala.language.existentials

/**
 * Add this trait to a Mapper to add support for many-to-many relationships
 * @author nafg
 */
trait ManyToMany extends BaseKeyedMapper {
  this: KeyedMapper[_, _] =>

  private[this] type K = TheKeyType
  private[this] type T = KeyedMapperType

  private var manyToManyFields: List[MappedManyToMany[_,_,_]] = Nil

  /**
   * An override for save to propagate the save to all children
   * of this parent.
   * Returns false as soon as the parent or a one-to-many field returns false.
   * If they are all successful returns true.
   */
  abstract override def save: Boolean = {
    super.save && manyToManyFields.forall(_.save)
  }

  /**
   * An override for delete_! to propogate the deletion to all children
   * of this parent.
   * Returns false as soon as the parent or a one-to-many field returns false.
   * If they are all successful returns true.
   */
  abstract override def delete_! : Boolean = {
    super.delete_! &&
      manyToManyFields.forall( _.delete_!)
  }


  /**
   * This is the base class to extend for fields that track many-to-many relationships.
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

    def otherFK[A](join: O)(f: MappedForeignKey[K2,O,T2] => A): A =
      otherField.actualField(join) match { case mfk: MappedForeignKey[K2,O,T2] => f(mfk) }

    protected def children: List[T2] = joins.flatMap(otherFK(_)(_.obj))

    protected var _joins: List[O] = _

    /**
     * Get the list of instances of joinMeta
     */
    def joins: List[O] = _joins // read only to the public
    protected var removedJoins: List[O] = Nil

    refresh
    manyToManyFields ::= this

    protected def isJoinForChild(e: T2)(join: O): Boolean = otherField.actualField(join).get == e.primaryKeyField.get

    protected def joinForChild(e: T2): Option[O] = joins.find(isJoinForChild(e))

    protected def own(e: T2): O = {
      joinForChild(e).fold {
        removedJoins
          // first check if we can recycle a removed join
          .find(otherField.actualField(_).get == e.primaryKeyField)
          .fold{
            val newJoin = joinMeta.create
            thisField.actualField(newJoin) match {
              case mfk: MappedForeignKey[K, O, T] => mfk.set(primaryKeyField.get.asInstanceOf[K])
            }
            otherFK(newJoin)(_.apply(e))
            newJoin
          }{ removedJoin =>
            removedJoins = removedJoins filter removedJoin.ne
            removedJoin // well, noLongerRemovedJoin...
          }
      }(join => join)
    }

    protected def unown(e: T2): Option[O] =
      joinForChild(e).map{ join =>
        removedJoins = join :: removedJoins
        val o = otherField.actualField(join)
        o.set(o.defaultValue)
        thisField.actualField(join) match { case mfk => mfk set mfk.defaultValue }
        join
      }

    /**
     * Get the List backing this Buffer.
     */
    def all: List[T2] = children

    def length: Int = children.length

    def iterator: Iterator[T2] = children.iterator

    protected def childAt(n: Int): T2 = children(n)
    def apply(n: Int): T2 = childAt(n)
    def indexOf(e: T2): Int = children.indexWhere(e.eq)

    def insert(idx: Int, elem: T2): Unit = insertAll(idx, Seq(elem))

    def insertAll(n: Int, traversable: IterableOnce[T2]): Unit = {
      val ownedJoins = traversable.iterator.map(own)
      val n2 = joins.indexWhere(isJoinForChild(children(n)))
      val before = joins.take(n2)
      val after = joins.drop(n2)

      _joins = before ++ ownedJoins ++ after
    }

    def prepend(elem: T2): MappedManyToMany.this.type = {
      _joins ::= own(elem)
      this
    }

    def addOne(elem: T2): MappedManyToMany.this.type = {
      _joins ++= List(own(elem))
      this
    }

    def update(n: Int, newelem: T2): Unit = {
      unown(childAt(n)) match {
        case Some(join) =>
          val n2 = joins.indexOf(join)
          val (before, after) = (joins.take(n2), joins.drop(n2+1))
          _joins = before ++ List(own(newelem)) ++ after
        case None =>
      }
    }

    def remove(n: Int): T2 = {
      val child = childAt(n)
      unown(child).foreach(join => _joins = joins filterNot join.eq)
      child
    }

    override def remove(idx: Int, count: Int): Unit = {
      if (count > 0) {
        @tailrec
        def loop(c: Int, a: List[T2]): List[T2] =
          if (c == 0) childAt(idx) :: a
          else loop(c - 1, childAt(idx + c) :: a)

        val joins0 = loop(count - 1, Nil) flatMap unown
        _joins = joins filterNot joins0.contains
      }
    }

    override def patchInPlace(from: Int, patch: IterableOnce[T2], replaced: Int): MappedManyToMany.this.type = {
      remove(from, replaced)
      insertAll(from, patch)
      this
    }

    def clear(): Unit = {
      children foreach unown
      _joins = Nil
    }

    /**
     * Discard the cached state of this MappedManyToMany's children and reinitialize it from the database
     */
    def refresh: List[T2] = {
      val by = new Cmp[O, TheKeyType](thisField, OprEnum.Eql, Full(primaryKeyField.get.asInstanceOf[K]), Empty, Empty)

      _joins = joinMeta.findAll( (by :: qp.toList): _*)
      all
    }

    /**
     * Save the state of this MappedManyToMany to the database.
     * This will do the following:
     * 1) Prune join table instances whose "child" foreign key's value is its defaultValue, i.e., -1
     * 2) Set all join table instances' "parent" foreign key
     * 3) Delete all join table instances whose child instance was removed
     * 4) Save all child instances
     * 5) If step 3 succeeds save all join instances
     * 6) Return true if steps 2-4 all returned true; otherwise false
     */
    def save: Boolean = {
      _joins = joins.filter { join =>
        otherFK(join)(f => f.get != f.defaultValue)
      }
      _joins foreach {
        thisField.actualField(_).asInstanceOf[MappedForeignKey[K,O,X] forSome {type X <: KeyedMapper[K,X]}] set ManyToMany.this.primaryKeyField.get.asInstanceOf[K]
      }

      removedJoins.forall {_.delete_!} & ( // continue saving even if deleting fails
        children.forall(_.save) &&
          joins.forall(_.save)
      )
    }

    /**
     * Deletes all join rows, including those
     * marked for removal.
     * Returns true if both succeed, otherwise false
     */
    def delete_! : Boolean = {
      removedJoins.forall(_.delete_!) &
        joins.forall(_.delete_!)
    }
  }
}
