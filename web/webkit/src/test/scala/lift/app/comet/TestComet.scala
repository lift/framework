package lift.app.comet

import net.liftweb.common.Loggable
import net.liftweb.http.CometActor

import scala.xml.NodeSeq

class TestComet extends CometActor with Loggable {
  logger.info("TestComet created")
  def render = NodeSeq.Empty
  override def !(msg: Any) = {
    logger.info("Message received: " + msg)
  }
}

