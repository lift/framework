/*
 * Copyright 2011 Lift Committers and Contributors
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
package common

import java.util.{List => JavaList, Iterator => JavaIterator, ArrayList,
                ListIterator, Collection => JavaCollection}

/**
 * An immutable singly linked list that uses the Scala List class as backing
 * store, but is Java-friendly as a `java.util.List`. Note however that since it
 * is immutable, you have to capture the results of addition/removal operations.
 *
 * The typical mutating methods like `add`, `set`, `clear`, and `remove` are all
 * unsupported, as are mutating methods on its iterators, since this collection
 * is immutable.
 */
final case class SimpleList[T](underlying: List[T]) extends JavaList[T] {

  /**
   * Construct an empty list.
   */
  def this() = this(Nil)

  def this(jl: JavaList[T]) = this(jl.toArray().toList.asInstanceOf[List[T]])

  /**
   * Append an item to this list. This operation is O(n) where `n` is the number
   * of items in the underlying `List`, and returns the updated list.
   */
  def append(item: T): SimpleList[T] = SimpleList(underlying :+ item)

  /**
   * Prepends an item to this list.  This operation is O(1) and returns the
   * updated list.
   */
  def prepend(item: T): SimpleList[T] = SimpleList(item :: underlying)

  def take(n: Int): SimpleList[T] = SimpleList(underlying.take(n))
  def drop(n: Int): SimpleList[T] = SimpleList(underlying.drop(n))
  def takeRight(n: Int): SimpleList[T] = SimpleList(underlying.takeRight(n))
  def dropRight(n: Int): SimpleList[T] = SimpleList(underlying.dropRight(n))
  def reverse(): SimpleList[T] = SimpleList(underlying.reverse)

  def headOption(): Option[T] = underlying.headOption

  def head(): T = underlying.head

  def tail(): SimpleList[T] = SimpleList(underlying.tail)

  def size(): Int = underlying.length

  def isEmpty(): Boolean = underlying.isEmpty

  /**
   * Returns true if this list contains the given `obj`.
   */
  def contains(obj: Object): Boolean = underlying.contains(obj)

  def iterator(): JavaIterator[T] = {
    val it = underlying.iterator
    new JavaIterator[T] {
      def hasNext() = it.hasNext
      def next(): T = it.next()
      override def remove() = throw new UnsupportedOperationException()
    }
  }

  def subList(from: Int, to: Int): JavaList[T] =
    SimpleList(underlying.drop(from).take(to - from))

  def listIterator(): ListIterator[T] =
    (new ArrayList(this)).listIterator()

  def listIterator(pos: Int): ListIterator[T] =
    (new ArrayList(this)).listIterator(pos)

  def indexOf(obj: Object): Int =
    underlying.indexOf(obj)

  def lastIndexOf(obj: Object): Int =
    underlying.lastIndexOf(obj)

  def add(x: T): Boolean = throw new UnsupportedOperationException()

  def add(after: Int, x: T): Unit = throw new UnsupportedOperationException()

  def set(after: Int, x: T): T = throw new UnsupportedOperationException()

  def clear(): Unit = throw new UnsupportedOperationException()

  def remove(pos: Int): T = throw new UnsupportedOperationException()

  def remove(obj: Object): Boolean = throw new UnsupportedOperationException()

  def get(pos: Int): T = underlying(pos)

  def toArray(): Array[Object] = {
    val len = underlying.length
    val ret = java.lang.reflect.Array.newInstance(classOf[Object], len).asInstanceOf[Array[Object]]

    var pos = 0
    var hd = underlying
    while (pos < len) {
      ret(pos) = hd.head.asInstanceOf[Object]
      hd = hd.tail
      pos += 1
    }

    ret
    }


  def toArray[X](in: Array[X with Object]): Array[X with Object] = {
    val clz = in.getClass.getComponentType()
    val len = underlying.length
    val ret = java.lang.reflect.Array.newInstance(clz, len).asInstanceOf[Array[X with Object]]

    var pos = 0
    var hd = underlying
    while (pos < len) {
      ret(pos) = clz.cast(hd.head).asInstanceOf[X with Object]
      hd = hd.tail
      pos += 1
    }


    ret
  }

  def retainAll(jc: JavaCollection[_]): Boolean = throw new UnsupportedOperationException()

  def removeAll(jc: JavaCollection[_]): Boolean = throw new UnsupportedOperationException()

  def addAll(jc: JavaCollection[_ <: T]): Boolean = throw new UnsupportedOperationException()

  def addAll(index: Int, jc: JavaCollection[_ <: T]): Boolean = throw new UnsupportedOperationException()

  def containsAll(jc: JavaCollection[_]): Boolean = {
    val it = jc.iterator()

    import scala.annotation._

    @tailrec def check(): Boolean = it.hasNext() match {
      case false => true
      case _ => contains(it.next().asInstanceOf[Object]) match {
        case false => false
        case _ => check()
      }
    }

    check()
  }


}

/**
 * An immutable vector that uses the Scala `[[scala.collection.immutable.Vector Vector]]`
 * class as backing store, but is Java-friendly as a `java.util.List`. Note however that
 * since it is immutable, you have to capture the results of addition/removal
 * operations.
 *
 * The typical mutating methods like `add`, `set`, `clear`, and `remove` are all
 * unsupported, as are mutating methods on its iterators, since this collection
 * is immutable.
 *
 * @see [[http://docs.scala-lang.org/overviews/collections/concrete-immutable-collection-classes.html#vectors "Scala's Collection Library overview"]]
 *      section on Vectors for more information.
 */
final case class SimpleVector[T](underlying: Vector[T]) extends JavaList[T] {

  /**
   * Construct an empty vector.
   */
  def this() = this(Vector())

  def this(jl: JavaList[T]) = this(Vector(jl.toArray().toList.asInstanceOf[List[T]] :_*))

  /**
   * Append an item to this vector. This operation is effectively O(1) and
   * returns the updated vector.
   */
  def append(item: T): SimpleVector[T] = SimpleVector(underlying :+ item)

  /**
   * Prepends an item to this vector.  This operation is effectively O(1) and
   * returns the updated vector.
   */
  def prepend(item: T): SimpleVector[T] = SimpleVector(item +: underlying)

  def take(n: Int): SimpleVector[T] = SimpleVector(underlying.take(n))
  def drop(n: Int): SimpleVector[T] = SimpleVector(underlying.drop(n))
  def takeRight(n: Int): SimpleVector[T] = SimpleVector(underlying.takeRight(n))
  def dropRight(n: Int): SimpleVector[T] = SimpleVector(underlying.dropRight(n))
  def reverse(): SimpleVector[T] = SimpleVector(underlying.reverse)

  def headOption(): Option[T] = underlying.headOption

  def head(): T = underlying.head

  def tail(): SimpleVector[T] = SimpleVector(underlying.tail)

  def size(): Int = underlying.length

  def isEmpty(): Boolean = underlying.isEmpty

  /**
   * Returns `true` if this vector contains the given `obj`.
   */
  def contains(obj: Object): Boolean = underlying.contains(obj)

  def iterator(): JavaIterator[T] = {
    val it = underlying.iterator
    new JavaIterator[T] {
      def hasNext() = it.hasNext
      def next(): T = it.next()
      override def remove() = throw new UnsupportedOperationException()
    }
  }

  def subList(from: Int, to: Int): JavaList[T] =
    SimpleVector(underlying.drop(from).take(to - from))

  def listIterator(): ListIterator[T] =
    (new ArrayList(this)).listIterator()

  def listIterator(pos: Int): ListIterator[T] =
    (new ArrayList(this)).listIterator(pos)

  def indexOf(obj: Object): Int =
    underlying.indexOf(obj)

  def lastIndexOf(obj: Object): Int =
    underlying.lastIndexOf(obj)

  def add(x: T): Boolean = throw new UnsupportedOperationException()

  def add(after: Int, x: T): Unit = throw new UnsupportedOperationException()

  def set(after: Int, x: T): T = throw new UnsupportedOperationException()

  def clear(): Unit = throw new UnsupportedOperationException()

  def remove(pos: Int): T = throw new UnsupportedOperationException()

  def remove(obj: Object): Boolean = throw new UnsupportedOperationException()

  def get(pos: Int): T = underlying(pos)

  def toArray(): Array[Object] = {
    val len = underlying.length
    val ret = java.lang.reflect.Array.newInstance(classOf[Object], len).asInstanceOf[Array[Object]]

    var pos = 0
    underlying.foreach {
      e =>
      ret(pos) = e.asInstanceOf[Object]
      pos += 1
    }

    ret
  }

  def toArray[X](in: Array[X with Object]): Array[X with Object] = {
    val clz = in.getClass.getComponentType()
    val len = underlying.length
    val ret = java.lang.reflect.Array.newInstance(clz, len).asInstanceOf[Array[X with Object]]

    var pos = 0
    underlying.foreach{
      e => ret(pos) = clz.cast(e).asInstanceOf[X with Object]
      pos += 1
    }

    ret
  }

  def retainAll(jc: JavaCollection[_]): Boolean = throw new UnsupportedOperationException()

  def removeAll(jc: JavaCollection[_]): Boolean = throw new UnsupportedOperationException()

  def addAll(jc: JavaCollection[_ <: T]): Boolean = throw new UnsupportedOperationException()

  def addAll(index: Int, jc: JavaCollection[_ <: T]): Boolean = throw new UnsupportedOperationException()

  def containsAll(jc: JavaCollection[_]): Boolean = {
    val it = jc.iterator()

    import scala.annotation._

    @tailrec def check(): Boolean = it.hasNext() match {
      case false => true
      case _ => contains(it.next().asInstanceOf[Object]) match {
        case false => false
        case _ => check()
      }
    }

    check()
  }


}

// vim: set ts=2 sw=2 et:
