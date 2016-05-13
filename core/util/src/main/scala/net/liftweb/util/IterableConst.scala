package net.liftweb
package util

import common._
import xml._
import java.util.{List => JavaList}
// import scala.collection.JavaConverters._

/**
 * A trait that has some helpful implicit conversions from
 * Iterable[NodeSeq], Seq[String], Box[String], and Option[String]
 */
trait IterableConst {
  def constList(nodeSeq: NodeSeq): Seq[NodeSeq]
}

import scala.collection.JavaConversions._

/**
 * The implementation for a NodeSeq Iterable Const
 */
final case class NodeSeqIterableConst(it: Iterable[NodeSeq]) extends IterableConst {
  def this(it: JavaList[NodeSeq]) = this(it: Iterable[NodeSeq])

  def constList(nodeSeq: NodeSeq): Seq[NodeSeq] = it.toSeq
}

/**
 * The implementation for a NodeSeq => NodeSeq Iterable Const
 */
final case class NodeSeqFuncIterableConst(it: Iterable[NodeSeq => NodeSeq]) extends IterableConst {
  def this(it: JavaList[NodeSeq => NodeSeq]) = this(it: Iterable[NodeSeq => NodeSeq])

  def constList(nodeSeq: NodeSeq): Seq[NodeSeq] = Helpers.ensureUniqueId(it.map(_(nodeSeq)).toSeq)
}

/**
 * The implementation for a Box[NodeSeq => Node] Iterable Const
 */
final case class BoxNodeSeqFuncIterableConst(it: Box[NodeSeq => NodeSeq]) extends IterableConst {

  def constList(nodeSeq: NodeSeq): Seq[NodeSeq] = it.toList.map(_(nodeSeq))
}

/**
 * The implementation for a Option[NodeSeq => Node] Iterable Const
 */
final case class OptionNodeSeqFuncIterableConst(it: Option[NodeSeq => NodeSeq]) extends IterableConst {

  def constList(nodeSeq: NodeSeq): Seq[NodeSeq] = it.toList.map(_(nodeSeq))
}

/**
 * Sequence of String iterable const
 */
final case class SeqStringIterableConst(it: Iterable[String]) extends IterableConst {
  def this(it: JavaList[String]) = this(it: Iterable[String])

  def constList(nodeSeq: NodeSeq): Seq[NodeSeq] = it.map(a => Text(a)).toSeq
}

/**
 * Sequence of Bindable iterable const
 */
final case class SeqBindableIterableConst(it: Iterable[Bindable]) extends IterableConst {
  def this(it: JavaList[Bindable]) = this(it: Iterable[Bindable])

  def constList(nodeSeq: NodeSeq): Seq[NodeSeq] = it.map(_.asHtml).toSeq
}

/**
 * The companion object that does the helpful promotion of common
 * collection types into an IterableConst,
 * e.g. Iterable[NodeSeq], Seq[String], Box[String], and Option[String]
 */
object IterableConst {
  import scala.language.implicitConversions

  /**
   * Converts anything that can be converted into an Iterable[NodeSeq]
   * into an IterableConst.  This includes Seq[NodeSeq]
   */
  implicit def itNodeSeq(it: Iterable[NodeSeq]): IterableConst =
    NodeSeqIterableConst(it)

  /**
   * Converts anything that can be converted into an Box[NodeSeq]
   */
  implicit def boxNodeSeq(it: Box[NodeSeq]): IterableConst =
    NodeSeqIterableConst(it.toList)

  /**
   * Converts anything that can be converted into an Box[NodeSeq]
   */
  implicit def optionNodeSeq(it: Option[NodeSeq]): IterableConst =
    NodeSeqIterableConst(it.toList)

  /**
   * Converts anything that can be converted into an Iterable[NodeSeq]
   * into an IterableConst.  This includes Seq[NodeSeq], Option[NodeSeq],
   * and Box[NodeSeq]
   */
  implicit def itNodeSeq(it: JavaList[NodeSeq]): IterableConst =
    new NodeSeqIterableConst(it)

  implicit def itNodeSeqFunc(it: Iterable[NodeSeq => NodeSeq]): IterableConst =
    NodeSeqFuncIterableConst(it)

  implicit def itNodeSeqFunc(it: JavaList[NodeSeq => NodeSeq]): IterableConst =
    new NodeSeqFuncIterableConst(it)

  implicit def boxNodeSeqFunc(it: Box[NodeSeq => NodeSeq]): IterableConst =
    BoxNodeSeqFuncIterableConst(it)

  implicit def optionNodeSeqFunc(it: Option[NodeSeq => NodeSeq]): IterableConst =
    OptionNodeSeqFuncIterableConst(it)

  implicit def itStringPromotable(it: Iterable[String]): IterableConst =
    SeqStringIterableConst(it)

  implicit def javaListStringPromotable(it: JavaList[String]): IterableConst =
    new SeqStringIterableConst(it)

  implicit def boxString(it: Box[String]): IterableConst =
    SeqStringIterableConst(it.toList)

  implicit def optionString(it: Option[String]): IterableConst =
    SeqStringIterableConst(it.toList)

  implicit def itBindable(it: Iterable[Bindable]): IterableConst =
    SeqBindableIterableConst(it)

  implicit def itBindable(it: JavaList[Bindable]): IterableConst =
    new SeqBindableIterableConst(it)


  implicit def boxBindablePromotable(it: Box[Bindable]): IterableConst =
    SeqBindableIterableConst(it.toList)

  implicit def optionBindablePromotable(it: Option[Bindable]): IterableConst =
    SeqBindableIterableConst(it.toList)

  implicit def optionStringPromotable[T](o: Option[T])(implicit view:T=>StringPromotable) = optionString(o.map(view(_).toString))
}

