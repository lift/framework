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
package util {

import _root_.net.liftweb.common._
  /**
   * A wiring Cell.  A Cell can be a ValueCell which holds
   * a value which can be set (and thus update the dependencies),
   * a FuncCell (a cell that is a function that depends on other cells),
   * or a DynamicCell which has a value that updates each time the cell is
   * accessed.
   */
  trait Cell[T] {
    /**
     * The cell's value and most recent change time
     */
    def currentValue: (T, Long)

    /**
     * Get the cell's value
     */
    def get: T = currentValue._1

    /**
     * Create a new Cell by applying the function to this cell
     */
    def lift[A](f: T => A): Cell[A] = FuncCell(this)(f)

    def lift[A,B](cell: Cell[B])(f: (T, B) => A) = FuncCell(this, cell)(f)
  }

  object Cell {
    def apply[T](v: T): ValueCell[T] = new ValueCell[T](v)
  }

  /**
   * A cell that changes value on each access.  This kind of cell
   * could be used to access an external resource.  <b>Warning</b>
   * the function may be accessed many times during a single wiring
   * recalculation, so it's best to use this as a front to a value
   * that's memoized for some duration.
   */
  final case class DynamicCell[T](f: () => T) extends Cell[T] {
    /**
     * The cell's value and most recent change time
     */
    def currentValue: (T, Long) = 
      f() -> System.nanoTime()
  }

  /**
   * The companion object that has a helpful constructor
   */
  object ValueCell {
    def apply[A](value: A): ValueCell[A] = new ValueCell(value)

    implicit def vcToT[T](in: ValueCell[T]): T = in.get
  }

  /**
   * A ValueCell holds a value that can be mutated.
   */
  final class ValueCell[A](initialValue: A) extends Cell[A] with LiftValue[A] {
    private var value: A = initialValue
    private var ct: Long = System.nanoTime()

    /**
     * The cell's value and most recent change time
     */
    def currentValue: (A, Long) = synchronized {
      (value, ct)
    }

    /**
     * Get the cell's value
     */
    def set(v: A): A = synchronized {
      value = v
      ct = System.nanoTime()
      value
    }

    override def toString(): String = synchronized {
      "ValueCell("+value+")"
    }

    override def hashCode(): Int = synchronized {
      if (null.asInstanceOf[Object] eq value.asInstanceOf[Object]) 0
      else value.hashCode()
    }

    override def equals(other: Any): Boolean = synchronized {
      other match {
        case vc: ValueCell[_] => value == vc.get
        case _ => false
      }
    }
  }

  /**
   * A collection of Cells og a given type
   */
  final case class SeqCell[T](cells: Cell[T]*) extends Cell[Seq[T]] {
    /**
     * The cell's value and most recent change time
     */
    def currentValue: (Seq[T], Long) = {
      val tcv = cells.map(_.currentValue)
      tcv.map(_._1) -> tcv.foldLeft(0L)((max, c) => if (max > c._2) max else c._2)
    }
  }

  /**
   * The companion object for FuncCell (function cells)
   */
  object FuncCell {
    /**
     * Construct a function cell based on a single parameter
     */
    def apply[A, Z](a: Cell[A])(f: A => Z): Cell[Z] = FuncCell1(a, f)

    /**
     * Construct a function cell based on two parameters
     */
    def apply[A, B, Z](a: Cell[A], b: Cell[B])(f: (A, B) => Z): Cell[Z] = FuncCell2(a, b, f)

    /**
     * Construct a function cell based on three parameters
     */
    def apply[A, B, C, Z](a: Cell[A], b: Cell[B], c: Cell[C])(f: (A, B, C) => Z): Cell[Z] = FuncCell3(a, b, c, f)

    /**
     * Construct a function cell based on four parameters
     */
    def apply[A, B, C, D, Z](a: Cell[A], b: Cell[B], c: Cell[C], d: Cell[D])(f: (A, B, C, D) => Z): Cell[Z] = FuncCell4(a, b, c, d, f)

    /**
     * Construct a function cell based on five parameters
     */
    def apply[A, B, C, D, E, Z](a: Cell[A], b: Cell[B], c: Cell[C], d: Cell[D], e: Cell[E])(f: (A, B, C, D, E) => Z): Cell[Z] = FuncCell5(a, b, c, d, e, f)
  }

  final case class FuncCell1[A, Z](a: Cell[A], f: A => Z) extends Cell[Z] {
    private var value: Z = _
    private var ct: Long = 0

    def currentValue: (Z, Long) = synchronized {
      val (v, t) = a.currentValue
      if (t > ct) {
        value = f(v)
        ct = t
      }
      (value -> ct)
    }
  }

  final case class FuncCell2[A, B, Z](a: Cell[A], b: Cell[B], f: (A, B) => Z) extends Cell[Z] {
    private var value: Z = _
    private var ct: Long = 0

    def currentValue: (Z, Long) = synchronized {
      val (v1, t1) = a.currentValue
      val (v2, t2) = b.currentValue
      val t = WiringHelper.max(t1, t2)
      if (t > ct) {
        value = f(v1, v2)
        ct = t
      }
      (value -> ct)
    }
  }
  
  final case class FuncCell3[A, B, C, Z](a: Cell[A], b: Cell[B], c: Cell[C], f: (A, B, C) => Z) extends Cell[Z] {
    private var value: Z = _
    private var ct: Long = 0

    def currentValue: (Z, Long) = synchronized {
      val (v1, t1) = a.currentValue
      val (v2, t2) = b.currentValue
      val (v3, t3) = c.currentValue
      val t = WiringHelper.max(t1, t2, t3)
      if (t > ct) {
        value = f(v1, v2, v3)
        ct = t
      }
      (value -> ct)
    }
  }
  
  final case class FuncCell4[A, B, C, D, Z](a: Cell[A], b: Cell[B], c: Cell[C], d: Cell[D], f: (A, B, C, D) => Z) extends Cell[Z] {
    private var value: Z = _
    private var ct: Long = 0

    def currentValue: (Z, Long) = synchronized {
      val (v1, t1) = a.currentValue
      val (v2, t2) = b.currentValue
      val (v3, t3) = c.currentValue
      val (v4, t4) = d.currentValue
      val t = WiringHelper.max(t1, t2, t3, t4)
      if (t > ct) {
        value = f(v1, v2, v3, v4)
        ct = t
      }
      (value -> ct)
    }
  }
  
  final case class FuncCell5[A, B, C, D, E, Z](a: Cell[A], b: Cell[B], c: Cell[C], d: Cell[D], e: Cell[E], f: (A, B, C, D, E) => Z) extends Cell[Z] {
    private var value: Z = _
    private var ct: Long = 0

    def currentValue: (Z, Long) = synchronized {
      val (v1, t1) = a.currentValue
      val (v2, t2) = b.currentValue
      val (v3, t3) = c.currentValue
      val (v4, t4) = d.currentValue
      val (v5, t5) = e.currentValue
      val t = WiringHelper.max(t1, t2, t3, t4, t5)
      if (t > ct) {
        value = f(v1, v2, v3, v4, v5)
        ct = t
      }
      (value -> ct)
    }
  }
  
  private object WiringHelper {
    def max(a: Long, b: Long*): Long = b.foldLeft(a){
      (v1, v2) =>
        if (v1 > v2) v1 else v2
    }
  }
}
}
