package webapptest.snippet

import _root_.scala.xml.NodeSeq
import _root_.net.liftweb.widgets.rssfeed.RSSFeed

class RSSFeedDemo {
  def render(xhtml: NodeSeq) :NodeSeq = {
    val widget = new RSSFeed()

    <xml:group>
      {widget.render("http://www.praytothemachine.com/evil/index.php/feed/")}
    </xml:group>
  }
}
