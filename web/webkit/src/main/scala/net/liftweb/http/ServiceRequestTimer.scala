package net.liftweb.http

import net.liftweb.http.provider.HTTPResponse
import net.liftweb.util.Helpers

trait ServiceRequestTimer {
  def logTime(req: Req, resp: HTTPResponse)(doService: (Req, HTTPResponse) => Boolean): Boolean
}

object NoOpServiceTimer extends ServiceRequestTimer {
  override def logTime(req: Req, resp: HTTPResponse)(doService: (Req, HTTPResponse) => Boolean): Boolean = {
    doService(req, resp)
  }
}

object StandardServiceTimer extends ServiceRequestTimer {
  override def logTime(req: Req, resp: HTTPResponse)(doService: (Req, HTTPResponse) => Boolean): Boolean = {
    Helpers.logTime {
      val ret = doService(req, resp)
      val msg = "Service request (" + req.request.method + ") " + req.request.uri + " returned " + resp.getStatus + ","
      (msg, ret)
    }
  }
}
