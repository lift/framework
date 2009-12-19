package webapptest.snippet

import _root_.scala.xml.NodeSeq
import _root_.net.liftweb.widgets.tablesorter.TableSorter

class TableSorterDemo {
  def render(xhtml: NodeSeq) :NodeSeq = {
    TableSorter("table")
  }
}