package net.liftweb.http.rest

import net.liftweb.mockweb.{MockWeb, WebSpec}
import net.liftweb.mocks.MockHttpServletRequest
import net.liftweb.http._
import net.liftweb.http.RewriteRequest
import net.liftweb.http.ParsePath
import net.liftweb.common.Full


object RestHelperSpecBoot {
  def boot() {
    println("WebSpecSpec Booting up")

    LiftRules.dispatch.append(RestHelperSpecRest)

    println("WebSpecSpec Boot complete")
  }
}


class RestHelperSpec extends WebSpec(RestHelperSpecBoot.boot _) {
  sequential  // This is important for using SessionVars, etc.

  "RestHelper" should {
    val testUrl = "http://foo.com/api/info"
    val testInvalidUrl = "http://foo.com/api/invalid"

    val testReq = new MockHttpServletRequest(testUrl){
      method = "OPTIONS"
    }

    "set OPTIONS method" withReqFor testReq in { req =>
      req.options_? must_== true
    }

    "give the correct response" withReqFor testReq in { req =>
      RestHelperSpecRest(req)() match {
        case Full(OkResponse()) => success
        case other =>              failure("Invalid response : " + other)
      }
    }
  }
}

object RestHelperSpecRest extends RestHelper  {
  serve {
    case "api" :: "info" :: Nil Options req => OkResponse()
  }
}
