package net.liftweb {
package mapper {

import _root_.org.specs._
import _root_.org.specs.runner.JUnit3
import _root_.org.specs.runner.ConsoleRunner
import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import Helpers._
import _root_.net.liftweb.json._
import _root_.java.sql.{Connection, DriverManager}

import view._


class ItemsListSpecsAsTest extends JUnit3(ItemsListSpecs)
object ItemsListSpecsRunner extends ConsoleRunner(ItemsListSpecs)

object ItemsListSpecs extends Specification {

  val provider = DBProviders.H2MemoryProvider

  def init = {
    provider.setupDB
    Schemifier.schemify(true, Log.neverF _, SampleItem)
    new ItemsList[SampleItem] {
      val metaMapper = SampleItem
    }
  }

  /* FIXME: 280
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
      il.added.length must_== 0  // BUG EXPOSED!!!
      il.removed.length must_== 0
    }
  }
  */
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



}
}
