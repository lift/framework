package webapptest.snippet

import _root_.scala.xml.{NodeSeq, Text}
import _root_.net.liftweb.http._
import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import _root_.net.liftweb.widgets.autocomplete._

object posted extends RequestVar[Box[String]](Empty)

class AutoCompleteDemo {

  def render(xhtml: NodeSeq) :NodeSeq = {
    AutoComplete("", (current, limit) => {
      println("current = " + current)
      (1 to limit).map(v => "Value_" + v)
    }, s =>  posted(Full(s))) ++ (posted.map(t => <p>{"Submitted " + t}</p>) openOr Text(""))
  }

}
