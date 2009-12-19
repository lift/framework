package webapptest.snippet

import _root_.scala.xml.NodeSeq
import _root_.net.liftweb.widgets.gravatar.Gravatar

class GravatarDemo {

  def render(xhtml: NodeSeq) :NodeSeq = {
    // Gravatar("tyler.weir@gmail.com", 50)
    // Gravatar("tyler.weir@gmail.com", 48, "R")
    Gravatar("tyler.weir@gmail.com")
  }
}
