package net.liftweb
package util

import common._
import xml._
import collection.mutable.ListBuffer

/**
 * Created with IntelliJ IDEA.
 * User: dpp
 * Date: 6/25/12
 * Time: 3:34 PM
 *
 */

/**
 * This trait is both a NodeSeq => NodeSeq and has the ability
 * to chain CssSel instances so that they can be applied
 * en masse to incoming NodeSeq and do the transformation.
 */
trait CssSel extends Function1[NodeSeq, NodeSeq] {
  def &(other: CssSel): CssSel = (this, other) match {
    case (AggregatedCssBindFunc(a), AggregatedCssBindFunc(b)) =>
      AggregatedCssBindFunc(a ::: b)
    case (AggregatedCssBindFunc(a), o: CssBind) =>
      AggregatedCssBindFunc(a ::: List(o))
    case (t: CssBind, AggregatedCssBindFunc(a)) =>
      AggregatedCssBindFunc(t :: a)
    case (t: CssBind, o: CssBind) => AggregatedCssBindFunc(List(t, o))
  }

  /**
   * A Java callable aggregator
   */
  def and(that: CssSel): CssSel = this & that
}

/**
 * A passthrough function that does not change the nodes
 *
 * @tag CssFunction
 */
object PassThru extends Function1[NodeSeq, NodeSeq] {
  def apply(in: NodeSeq): NodeSeq = in
}

/**
 * Replaces the nodes with an Empty NodeSeq.  Useful
 * for removing unused nodes
 *
 * @tag CssFunction
 */
object ClearNodes extends Function1[NodeSeq, NodeSeq] {
  def apply(in: NodeSeq): NodeSeq = NodeSeq.Empty
}


private final case class AggregatedCssBindFunc(binds: List[CssBind]) extends CssSel {
  private lazy val (good, bad) = binds.partition {
    _.css.isDefined
  }
  private lazy val selectorMap = new SelectorMap(good)

  def apply(in: NodeSeq): NodeSeq = bad match {
    case Nil => selectorMap(in)
    case bv => bad.flatMap(_(in)) ++ selectorMap(in)
  }
}

/**
 * This CssBind will clear all nodes marked with the class
 * clearable.  Designers can mark extra nodes in markup with
 * class="clearable" and this Bind will make them go away
 */
class ClearClearable extends CssBindImpl(Full(".clearable"), CssSelectorParser.parse(".clearable")) {

  def calculate(in: NodeSeq): Seq[NodeSeq] = Nil
}

/**
 * This CssBind will clear all nodes marked with the class
 * clearable.  Designers can mark extra nodes in markup with
 * class="clearable" and this Bind will make them go away
 */
object ClearClearable extends ClearClearable

private class SelectorMap(binds: List[CssBind]) extends Function1[NodeSeq, NodeSeq] {

  // The KidsSubNode always has to go last or else we
  // get into an issue where we're trying to apply the whole
  // transform to the whole shooting match
  private def sortBinds(lst: List[CssBind]): List[CssBind] = {
    lst.sortWith {
      case (SubNode(me: EmptyBox), SubNode(_)) => true
      case (SubNode(_), SubNode(them: EmptyBox)) => false
      case (SubNode(Full(KidsSubNode())), SubNode(_)) => false
      case (SubNode(Full(PrependKidsSubNode())), SubNode(_)) => false
      case (SubNode(Full(AppendKidsSubNode())), SubNode(_)) => false
      case (SubNode(_), SubNode(Full(KidsSubNode()))) => true
      case (SubNode(_), SubNode(Full(PrependKidsSubNode()))) => true
      case (SubNode(_), SubNode(Full(SurroundKids()))) => true
      case (SubNode(_), SubNode(Full(AppendKidsSubNode()))) => true
      case _ => true
    }
  }

  private val (idMap, nameMap, clzMap, attrMap, elemMap,
  starFunc, selectThis: Box[CssBind]) = {
    var idMap: Map[String, List[CssBind]] = Map()
    var nameMap: Map[String, List[CssBind]] = Map()
    var clzMap: Map[String, List[CssBind]] = Map()
    var attrMap: Map[String, Map[String, List[CssBind]]] = Map()
    var elemMap: Map[String, List[CssBind]] = Map()
    var starFunc: Box[List[CssBind]] = Empty

    val selThis: Box[CssBind] = binds.flatMap {
      b =>
        b.css.openOrThrowException("Guarded with test before calling this method").subNodes match {
          case Full(SelectThisNode(_)) => List(b)
          case _ => Nil
        }
    }.headOption

    binds.foreach {
      case i@CssBind(IdSelector(id, _)) =>
        idMap += (id -> sortBinds(i :: idMap.getOrElse(id, Nil)))

      case i@CssBind(ElemSelector(id, _)) =>
        elemMap += (id -> sortBinds(i :: elemMap.getOrElse(id, Nil)))


      case i@CssBind(StarSelector(_, _)) => starFunc = Full(sortBinds(i :: starFunc.openOr(Nil)))

      case i@CssBind(NameSelector(name, _)) =>
        nameMap += (name -> sortBinds(i :: nameMap.getOrElse(name, Nil)))

      case i@CssBind(ClassSelector(clz, _)) =>
        clzMap += (clz -> sortBinds(i :: clzMap.getOrElse(clz, Nil)))

      case i@CssBind(AttrSelector(name, value, _)) => {
        val oldMap = attrMap.getOrElse(name, Map())
        attrMap += (name -> (oldMap + (value -> sortBinds(i :: oldMap.getOrElse(value, Nil)))))
      }
    }

    (idMap, nameMap, clzMap, attrMap, elemMap, starFunc, selThis)
  }

  private def findElemIfThereIsOne(in: NodeSeq): NodeSeq = in match {
    case e: Elem => e
    case ns if ns.length == 1 && ns(0).isInstanceOf[Elem] => ns(0)
    case ns => ns
  }

  private abstract class SlurpedAttrs(val id: Box[String], val name: Box[String]) {
    def attrs: Map[String, String]

    def classes: List[String]

    def removeId(in: MetaData) = in.filter {
      case up: UnprefixedAttribute => up.key != "id"
      case _ => true
    }

    private final def isSelThis(bind: CssBind): Boolean =
      bind.css.openOrThrowException("Guarded with test before calling this method").subNodes match {
        case Full(SelectThisNode(_)) => true
        case _ => false
      }

    final def applyRule(bindList: List[CssBind], realE: Elem, onlySelThis: Boolean, depth: Int): NodeSeq =
      bindList match {
        case Nil => realE

        // ignore selectThis commands outside the
        // select context
        case bind :: xs
          if onlySelThis && isSelThis(bind) => applyRule(xs, realE, onlySelThis, depth)

        case bind :: xs => {
          applyRule(bind, realE, depth) flatMap {
            case e: Elem => applyRule(xs, e, onlySelThis, depth + 1)
            case x => x
          }
        }
      }

    final def applyAttributeRules(bindList: List[CssBind], elem: Elem): Elem = {
      bindList.map(b => (b, b.css.openOrThrowException("Guarded with test before calling this method").
        subNodes.openOrThrowException("Guarded with test before calling this method"))).
        foldLeft(elem) {
        case (elem, (bind, AttrSubNode(attr))) => {
          val calced = bind.calculate(elem).map(findElemIfThereIsOne _)
          val filtered = elem.attributes.filter {
            case up: UnprefixedAttribute => up.key != attr
            case _ => true
          }

          val newAttr = if (calced.isEmpty) {
            filtered
          } else {
            val flat: NodeSeq = calced.flatMap(a => a)
            new UnprefixedAttribute(attr, flat, filtered)
          }

          elem.copy(attributes = newAttr)
        }

        case (elem, (bind, AttrAppendSubNode(attr))) => {
          val org: NodeSeq = elem.attribute(attr).getOrElse(NodeSeq.Empty)
          val calced = bind.calculate(elem).toList.map(findElemIfThereIsOne _)


          if (calced.isEmpty) {
            elem
          } else {
            val filtered = elem.attributes.filter {
              case up: UnprefixedAttribute => up.key != attr
              case _ => true
            }

            val flat: NodeSeq = if (attr == "class") {
              if (org.isEmpty) {
                calced.dropRight(1).flatMap(a => a ++ Text(" ")) ++
                  calced.takeRight(1).head
              } else {
                org ++ Text(" ") ++
                  calced.dropRight(1).flatMap(a => a ++ Text(" ")) ++
                  calced.takeRight(1).head
              }
            } else {
              org ++ (calced.flatMap(a => a): NodeSeq)
            }

            val newAttr = new UnprefixedAttribute(attr, flat, filtered)

            new Elem(elem.prefix,
              elem.label, newAttr,
              elem.scope, elem.minimizeEmpty, elem.child: _*)

          }
        }

        case (elem, (bind, AttrRemoveSubNode(attr))) => {
          val org: NodeSeq = elem.attribute(attr).getOrElse(NodeSeq.Empty)
          val calced = bind.calculate(elem).toList.map(findElemIfThereIsOne _)

          if (calced.isEmpty || org.isEmpty) {
            // if either is empty, then return the Elem unmodified
            elem
          } else {
            val filtered = elem.attributes.filter {
              case up: UnprefixedAttribute => up.key != attr
              case _ => true
            }

            val flat: Box[NodeSeq] = if (attr == "class") {
              val set = Set(calced.map(_.text): _*)
              SuperString(org.text).charSplit(' ').toList.
                filter(_.length > 0).filter(s => !set.contains(s)) match {
                case Nil => Empty
                case xs => Full(Text(xs.mkString(" ")))
              }
            } else {
              if (org.text == calced.flatMap(a => a).text) Empty else Full(org)
            }

            val newAttr = flat match {
              case Full(a) => new UnprefixedAttribute(attr, a, filtered)
              case _ => filtered
            }

            new Elem(elem.prefix,
              elem.label, newAttr,
              elem.scope, elem.minimizeEmpty, elem.child: _*)

          }
        }
      }
    }


    // This is where the rules are applied
    final def applyRule(bind: CssBind, realE: Elem, depth: Int): NodeSeq = {
      def uniqueClasses(cv: String*): String = {
        import Helpers._

        val ls: List[String] = cv.toList.flatMap(_.charSplit(' '))
        import scala.collection.mutable._
        val hs: HashSet[String] = new HashSet()
        val ret: ListBuffer[String] = new ListBuffer()
        ls.foreach {
          v =>
            if (!hs.contains(v)) {
              hs += v
              ret += v
            }
        }
        ret.mkString(" ")
      }

      def mergeAll(other: MetaData, stripId: Boolean, skipClassMerge: Boolean): MetaData = {
        var oldAttrs = attrs - (if (stripId) "id" else "")

        var builtMeta: MetaData = Null
        var pos = other

        while (pos != Null) {
          pos match {
            case up: UnprefixedAttribute if stripId && up.key == "id" =>
            // ignore the id attribute

            case up: UnprefixedAttribute if up.key == "class" => {
              oldAttrs.get("class") match {
                case Some(ca) if !skipClassMerge => {
                  oldAttrs -= "class"
                  builtMeta = new UnprefixedAttribute("class",
                    uniqueClasses(up.value.
                      text,
                      ca),
                    builtMeta)
                }

                case _ =>
                  oldAttrs -= "class"
                  builtMeta = up.copy(builtMeta)
              }
            }

            case up: UnprefixedAttribute => {
              oldAttrs -= up.key
              builtMeta = up.copy(builtMeta)
            }

            case pa: PrefixedAttribute => {
              oldAttrs -= (pa.pre + ":" + pa.key)
              builtMeta = pa.copy(builtMeta)
            }
            case _ =>
          }

          pos = pos.next
        }

        for {
          (k, v) <- oldAttrs
        } {
          import Helpers._
          k.charSplit(':') match {
            case p :: k :: _ =>
              builtMeta = new PrefixedAttribute(p, k, v, builtMeta)
            case k :: _ => builtMeta = new UnprefixedAttribute(k, v, builtMeta)
            case _ =>
          }
        }

        builtMeta
      }

      // we can do an open_! here because all the CssBind elems
      // have been vetted
      bind.css.openOrThrowException("Guarded with test before calling this method").subNodes match {
        case Full(SelectThisNode(kids)) => {
          throw new RetryWithException(if (kids) realE.child else realE)
        }

        case Full(todo: WithKids) => {
          val calced = bind.calculate(realE.child)
          calced.length match {
            case 0 => new Elem(realE.prefix, realE.label, realE.attributes, realE.scope, realE.minimizeEmpty)
            case 1 => new Elem(realE.prefix, realE.label,
              realE.attributes, realE.scope, realE.minimizeEmpty,
              todo.transform(realE.child, calced.head): _*)
            case _ if id.isEmpty =>
              calced.map(kids => new Elem(realE.prefix, realE.label,
                realE.attributes, realE.scope, realE.minimizeEmpty,
                todo.transform(realE.child, kids): _*))

            case _ => {
              val noId = removeId(realE.attributes)
              calced.toList.zipWithIndex.map {
                case (kids, 0) =>
                  new Elem(realE.prefix, realE.label,
                    realE.attributes, realE.scope, realE.minimizeEmpty,
                    todo.transform(realE.child, kids): _*)
                case (kids, _) =>
                  new Elem(realE.prefix, realE.label,
                    noId, realE.scope, realE.minimizeEmpty,
                    todo.transform(realE.child, kids): _*)
              }
            }
          }
        }

        case x if x.isInstanceOf[EmptyBox] || x == Full(DontMergeAttributes) => {
          val calced = bind.calculate(realE).map(findElemIfThereIsOne _)

          calced.length match {
            case 0 => NodeSeq.Empty
            case 1 => {
              calced.head match {
                case Group(g) => g
                case e: Elem => new Elem(e.prefix,
                  e.label, mergeAll(e.attributes, false, x == Full(DontMergeAttributes)),
                  e.scope, e.minimizeEmpty, e.child: _*)
                case x => x
              }
            }

            case n => {
              val calcedList = calced.toList
              val availableIds = (attrs.get("id").toList ++
                calcedList.collect({
                  case e: Elem => e.attribute("id")
                }).flatten.map(_.toString)).toSet
              val merged = calcedList.foldLeft((availableIds, Nil: List[Seq[xml.Node]])) {
                (idsAndResult, a) =>
                  val (ids, result) = idsAndResult
                  a match {
                    case Group(g) => (ids, g :: result)
                    case e: Elem => {
                      val targetId = e.attribute("id").map(_.toString) orElse (attrs.get("id"))
                      val keepId = targetId map {
                        id => ids.contains(id)
                      } getOrElse (false)
                      val newIds = targetId filter (_ => keepId) map (i => ids - i) getOrElse (ids)
                      val newElem = new Elem(e.prefix, e.label, mergeAll(e.attributes, !keepId, x == Full(DontMergeAttributes)), e.scope, e.minimizeEmpty, e.child: _*)
                      (newIds, newElem :: result)
                    }
                    case x => (ids, x :: result)
                  }
              }
              merged._2.reverse.flatten
            }
          }
        }
      }
    }


    final def forId(in: Elem, buff: ListBuffer[CssBind]) {
      for {
        rid <- id
        bind <- idMap.get(rid)
      } buff ++= bind
    }

    final def forElem(in: Elem, buff: ListBuffer[CssBind]) {
      for {
        bind <- elemMap.get(in.label)
      } buff ++= bind
    }

    final def forStar(buff: ListBuffer[CssBind], depth: Int) {
      for {
        binds <- starFunc
        bind <- binds if (bind match {
        case CssBind(StarSelector(_, topOnly)) => !topOnly || (depth == 0)
        case _ => true
      })
      } buff += bind
    }

    final def forName(in: Elem, buff: ListBuffer[CssBind]) {
      for {
        rid <- name
        bind <- nameMap.get(rid)
      } buff ++= bind
    }

    def findClass(clz: List[String], buff: ListBuffer[CssBind]) {
      clz match {
        case Nil => ()
        case x :: xs => {
          clzMap.get(x) match {
            case Some(cb) => buff ++= cb
            case _ =>
          }
          findClass(xs, buff)
        }
      }
    }

    def forClass(in: Elem, buff: ListBuffer[CssBind]) {
      findClass(classes, buff)
    }

    def forAttr(in: Elem, buff: ListBuffer[CssBind]) {
      if (attrMap.isEmpty || attrs.isEmpty) ()
      else {
        for {
          (key, map) <- attrMap
          v <- attrs.get(key)
          cb <- map.get(v)
        } buff ++= cb
      }
    }
  }

  private def slurpAttrs(in: MetaData): SlurpedAttrs = {
    var id: Box[String] = Empty
    var cur: MetaData = in
    var name: Box[String] = Empty
    var clzs: List[String] = Nil
    var theAttrs: Map[String, String] = Map()

    while (cur != Null) {
      cur match {
        case up: UnprefixedAttribute if (null ne up.value) => {
          val key = up.key
          val value = up.value.text
          import Helpers._
          key match {
            case "id" => id = Full(value)
            case "name" => name = Full(value)
            case "class" => clzs = value.charSplit(' ')
            case _ =>
          }

          theAttrs += key -> value
        }

        case pa: PrefixedAttribute if (null ne pa.value) => {
          theAttrs += ((pa.pre + ":" + pa.key) -> pa.value.text)
        }

        case _ =>
      }

      cur = cur.next
    }

    new SlurpedAttrs(id, name) {
      def attrs: Map[String, String] = theAttrs

      def classes: List[String] = clzs
    }
  }

  final private def treatElem(e: Elem, onlySel: Boolean, depth: Int): NodeSeq = {
    val slurp = slurpAttrs(e.attributes)
    val lb = new ListBuffer[CssBind]

    slurp.forId(e, lb)
    slurp.forName(e, lb)
    slurp.forClass(e, lb)
    slurp.forElem(e, lb)
    slurp.forAttr(e, lb)
    slurp.forStar(lb, depth)

    if (onlySel) {
      lb.toList.filter(_.selectThis_?) match {
        case Nil => {
          run(e.child, onlySel, depth + 1)
          NodeSeq.Empty
        }

        case csb :: _ =>
          throw new RetryWithException(if (csb.selectThisChildren_?)
            e.child
          else e)
      }
    } else {
      lb.toList.filterNot(_.selectThis_?) match {
        case Nil => new Elem(e.prefix, e.label,
          e.attributes, e.scope, e.minimizeEmpty, run(e.child, onlySel, depth + 1): _*)
        case csb =>
          // do attributes first, then the body
          csb.partition(_.attrSel_?) match {
            case (Nil, rules) => slurp.applyRule(rules, e, onlySel, depth)
            case (attrs, Nil) => {
              val elem = slurp.applyAttributeRules(attrs, e)
              new Elem(elem.prefix, elem.label,
                elem.attributes, elem.scope, e.minimizeEmpty, run(elem.child, onlySel, depth + 1): _*)
            }

            case (attrs, rules) => {
              slurp.applyRule(rules,
                slurp.applyAttributeRules(attrs, e),
                onlySel, depth)
            }
          }
        // slurp.applyRule(csb, e, onlySel)
      }
    }
  }

  final def apply(in: NodeSeq): NodeSeq = selectThis match {
    case Full(_) => {
      try {
        run(in, true, 0)
      } catch {
        case RetryWithException(newElem) =>
          run(newElem, false, 0)
      }
    }

    case _ => run(in, false, 0)
  }

  final private def run(in: NodeSeq, onlyRunSel: Boolean, depth: Int): NodeSeq =
    in flatMap {
      case Group(g) => run(g, onlyRunSel, depth)
      case e: Elem => treatElem(e, onlyRunSel, depth)
      case x => x
    }
}

private case class RetryWithException(e: NodeSeq) extends Exception()

trait CssBindImplicits {
  class CssBindPromoter(stringSelector: Box[String], cssSelector: Box[CssSelector]) {
    /**
     * Transform a DOM (NodeSeq) based on rules
     *
     * @param it the thing to use in the replacement rules
     * @param converter the implicit parameter that transforms T into a
     *        NodeSeq=>NodeSeq that will update things matched by the selector
     * @tparam T the type of it
     * @return the function that will transform an incoming DOM based on the transform rules
     */
    def #>[T](replacement: => T)(implicit converter: CanBind[T]): CssSel = {
      cssSelector.collect {
        case EnclosedSelector(a, b) =>
          new CssBindPromoter(stringSelector, Full(a)) #> nsFunc({ ns =>
            new CssBindPromoter(stringSelector, Full(b)).#>(replacement)(converter)(ns)
          }) // (CanBind.nodeSeqFuncTransform)
      } openOr {
        new CssBindImpl(stringSelector, cssSelector) {
          def calculate(in: NodeSeq): Seq[NodeSeq] = converter(replacement)(in)
        }
      }
    }

    /**
     * Transform a DOM (NodeSeq) based on rules
     *
     * @param it the thing to use in the replacement rules
     * @param computer the implicit parameter that transforms T into something that will make the correct changes
     * @tparam T the type of it
     * @return the function that will transform an incoming DOM based on the transform rules
     */
    def replaceWith[T](it: => T)(implicit computer: CanBind[T]): CssSel = {
      this.#>(it)(computer)
    }
  }
  implicit class StringToCssBindPromoter(stringSelector: String) extends CssBindPromoter(Full(stringSelector), CssSelectorParser.parse(stringSelector))
  implicit class CssSelectorToCssBindPromoter(cssSelector: CssSelector) extends CssBindPromoter(Empty, Full(cssSelector))
}

object CssBind {
  def unapply(in: CssBind): Option[CssSelector] = in.css
}

trait CssBind extends CssSel {
  def stringSelector: Box[String]

  def css: Box[CssSelector]

  override def toString(): String = "CssBind(" + stringSelector + ", " +
    css + ")"

  def apply(in: NodeSeq): NodeSeq = css match {
    case Full(c) => selectorMap(in)
    case _ => Helpers.errorDiv(
      <div>
        Syntax error in CSS selector definition:
        {stringSelector openOr "N/A"}
        .
        The selector will not be applied.
      </div>) openOr NodeSeq.Empty
  }

  /**
   * Is this CssBind a SelectThis bind?
   */
  private[util] def selectThis_? : Boolean = css match {
    case Full(sel) => {
      sel.subNodes match {
        case Full(SelectThisNode(_)) => true
        case _ => false
      }
    }

    case _ => false
  }

  /**
   * Is this an Attribute mutating node?
   */
  private[util] def attrSel_? : Boolean = css match {
    case Full(sel) => {
      sel.subNodes match {
        case Full(x: AttributeRule) => true
        case _ => false
      }
    }

    case _ => false
  }

  private[util] def selectThisChildren_? : Boolean = css match {
    case Full(sel) => {
      sel.subNodes match {
        case Full(SelectThisNode(children)) => children
        case _ => false
      }
    }

    case _ => false
  }

  private lazy val selectorMap: SelectorMap = new SelectorMap(List(this))

  def calculate(in: NodeSeq): Seq[NodeSeq]
}

/**
 * An abstract implementation of CssBind.  You can instantiate
 * this class and create a custom calculate method
 */
abstract class CssBindImpl(val stringSelector: Box[String], val css: Box[CssSelector]) extends CssBind {
  def calculate(in: NodeSeq): Seq[NodeSeq]
}

/**
 * Bridge from Java-land to Scala
 */

final class CssJBridge extends CssBindImplicits {
  def sel(selector: String, value: String): CssSel = selector #> value

  def sel(selector: String, value: NodeSeq): CssSel = selector #> value

  def sel(selector: String, value: NodeSeq => NodeSeq): CssSel = selector #> value

  def sel(selector: String, value: Bindable): CssSel = selector #> value

  /**
   * Inserts a String constant according to the CssSelector rules
   */
  def sel(selector: String, str: StringPromotable): CssSel = (selector #> str.toString)

  /**
   * Inserts a String constant according to the CssSelector rules
   */
  def sel(selector: String, str: IterableConst): CssSel = (selector #> str)

  /**
   * Inserts a String constant according to the CssSelector rules
   */
  def sel(selector: String, str: IterableFunc): CssSel = (selector #> str)

}

trait CanBind[-T] {
  def apply(it: => T)(ns: NodeSeq): Seq[NodeSeq]
}

object CanBind extends CssBindImplicits {
  import scala.language.higherKinds

  implicit def stringTransform: CanBind[String] = new CanBind[String] {
    def apply(str: => String)(ns: NodeSeq): Seq[NodeSeq] = {
      val s = str
      List(if (null eq s) NodeSeq.Empty else Text(s))
    }
  }

  implicit def bindableTransform: CanBind[Bindable] = new CanBind[Bindable] {
    def apply(str: => Bindable)(ns: NodeSeq): Seq[NodeSeq] = List(str.asHtml)
  }


  implicit def numberTransform[T <: java.lang.Number]: CanBind[T] = new CanBind[java.lang.Number] {
    def apply(str: => java.lang.Number)(ns: NodeSeq): Seq[NodeSeq] = {
      val num = str
      List(if (null eq num) NodeSeq.Empty else Text(num.toString))
    }
  }

  implicit def doubleTRansform: CanBind[Double] = new CanBind[Double] {
    def apply(str: => Double)(ns: NodeSeq): Seq[NodeSeq] = List(Text(str.toString))
  }

  implicit def jsCmdTransform: CanBind[ToJsCmd] = new CanBind[ToJsCmd] {
    def apply(str: => ToJsCmd)(ns: NodeSeq): Seq[NodeSeq] = List(Text(str.toJsCmd))
  }


  implicit def jsCmdPairTransform: CanBind[(_, ToJsCmd)] = new CanBind[(_, ToJsCmd)] {
    def apply(str: => (_, ToJsCmd))(ns: NodeSeq): Seq[NodeSeq] = List(Text(str._2.toJsCmd))
  }

  implicit def intTransform: CanBind[Int] = new CanBind[Int] {
    def apply(str: => Int)(ns: NodeSeq): Seq[NodeSeq] = List(Text(str.toString))
  }

  implicit def stringPromoteTransform: CanBind[StringPromotable] = new CanBind[StringPromotable] {
    def apply(str: => StringPromotable)(ns: NodeSeq): Seq[NodeSeq] = List(Text(str.toString))
  }

  implicit def symbolTransform: CanBind[Symbol] = new CanBind[Symbol] {
    def apply(str: => Symbol)(ns: NodeSeq): Seq[NodeSeq] = List(Text(str.name))
  }

  implicit def longTransform: CanBind[Long] = new CanBind[Long] {
    def apply(str: => Long)(ns: NodeSeq): Seq[NodeSeq] = List(Text(str.toString))
  }

  implicit def boolTransform: CanBind[Boolean] = new CanBind[Boolean] {
    def apply(str: => Boolean)(ns: NodeSeq): Seq[NodeSeq] = List(Text(str.toString))
  }

  class CanBindNodeSeqTransform[T](f: T => NodeSeq) extends CanBind[T] {
    def apply(param: => T)(ns: NodeSeq): Seq[NodeSeq] = List(f(param))
  }
  implicit def toNodeSeqTransform[T](implicit f: T => NodeSeq): CanBind[T] = new CanBindNodeSeqTransform[T](f)

  implicit def nodeSeqFuncTransform[A](implicit view: A => NodeSeq => NodeSeq): CanBind[A] = new CanBind[A] {
    def apply(func: =>A)(ns: NodeSeq): Seq[NodeSeq] = List(view(func)(ns))
  }

  implicit def nodeSeqSeqFuncTransform: CanBind[NodeSeq => Seq[Node]] = new CanBind[NodeSeq => Seq[Node]] {
    def apply(func: => NodeSeq => Seq[Node])(ns: NodeSeq): Seq[NodeSeq] = Helpers.ensureUniqueId(List(func(ns)))
  }

  implicit def nodeFuncTransform: CanBind[NodeSeq => Node] = new CanBind[NodeSeq => Node] {
    def apply(func: => NodeSeq => Node)(ns: NodeSeq): Seq[NodeSeq] = List(func(ns))
  }


  implicit def iterableNodeTransform[NST](implicit f2: NST => NodeSeq): CanBind[Iterable[NST]] =
    new CanBind[Iterable[NST]] {
      def apply(info: => Iterable[NST])(ns: NodeSeq): Seq[NodeSeq] =
      {
        val i = info
        i match {
          case ns: NodeSeq => List(ns)
          case x => Helpers.ensureUniqueId(x.toSeq.map(f2))
        }
      }
    }

  implicit def boxNodeTransform[NST](implicit f2: NST => NodeSeq): CanBind[Box[NST]] =
    new CanBind[Box[NST]] {
      def apply(info: => Box[NST])(ns: NodeSeq): Seq[NodeSeq] = info.toList.map(f2)
    }

  implicit def optionNodeTransform[NST](implicit f2: NST => NodeSeq): CanBind[Option[NST]] =
    new CanBind[Option[NST]] {
      def apply(info: => Option[NST])(ns: NodeSeq): Seq[NodeSeq] = info.toList.map(f2)
    }

  implicit def iterableStringTransform[T[_]](implicit f: T[String] => Iterable[String]): CanBind[T[String]] =
    new CanBind[T[String]] {
      def apply(info: => T[String])(ns: NodeSeq): Seq[NodeSeq] = f(info).toSeq.map(a => Text(a))
    }

  implicit def iterableNumberTransform[T[_], N <: java.lang.Number](implicit f: T[N] => Iterable[N]): CanBind[T[N]] =
    new CanBind[T[N]] {
      def apply(info: => T[N])(ns: NodeSeq): Seq[NodeSeq] = f(info).toSeq.flatMap(a =>
        if (a eq null) Nil else List(Text(a.toString)))
    }

  implicit def iterableDouble[T[Double]](implicit f: T[Double] => Iterable[Double]): CanBind[T[Double]] =
    new CanBind[T[Double]] {
      def apply(info: => T[Double])(ns: NodeSeq): Seq[NodeSeq] = f(info).toSeq.flatMap(a =>
        if (a equals null) Nil else List(Text(a.toString)))
    }

  implicit def iterableBindableTransform[T[_]](implicit f: T[Bindable] => Iterable[Bindable]): CanBind[T[Bindable]] =
    new CanBind[T[Bindable]] {
      def apply(info: => T[Bindable])(ns: NodeSeq): Seq[NodeSeq] = Helpers.ensureUniqueId(f(info).toSeq.map(_.asHtml))
    }


  implicit def iterableStringPromotableTransform[T[_], PM](implicit f: T[PM] => Iterable[PM],
                                                           prom: PM => StringPromotable): CanBind[T[PM]] =
    new CanBind[T[PM]] {
      def apply(info: => T[PM])(ns: NodeSeq): Seq[NodeSeq] = f(info).toSeq.map(a => Text(prom(a).toString))
    }

  implicit def iterableNodeFuncTransform[T[_], F <: NodeSeq => NodeSeq](implicit f: T[F] => Iterable[F]): CanBind[T[F]] =
    new CanBind[T[F]] {
      def apply(info: => T[F])(ns: NodeSeq): Seq[NodeSeq] = Helpers.ensureUniqueId(f(info).toSeq.map(_.apply(ns)))
    }

  implicit def funcIterableTransform[T[_], F <: NodeSeq](implicit f: T[F] => Iterable[F]): CanBind[NodeSeq => T[F]] =
    new CanBind[NodeSeq => T[F]] {
      def apply(info: => NodeSeq => T[F])(ns: NodeSeq): Seq[NodeSeq] = Helpers.ensureUniqueId(f(info(ns)).toSeq)
    }

  implicit def stringFuncTransform: CanBind[NodeSeq => String] =
    new CanBind[NodeSeq => String] {
      def apply(info: => NodeSeq => String)(ns: NodeSeq): Seq[NodeSeq] = List(Text(info(ns)))
    }

  implicit def stringIterFuncTransform[T[_]](implicit f: T[String] => Iterable[String]): CanBind[NodeSeq => T[String]] =
    new CanBind[NodeSeq => T[String]] {
      def apply(info: => NodeSeq => T[String])(ns: NodeSeq): Seq[NodeSeq] = f(info(ns)).toSeq.map(Text(_))
    }


  implicit def iterableConstFuncTransform: CanBind[IterableConst] =
    new CanBind[IterableConst] {
      def apply(info: => IterableConst)(ns: NodeSeq): Seq[NodeSeq] = info.constList(ns)
    }
}
