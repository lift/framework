package net.liftweb.http.rest

import net.liftweb.mockweb.WebSpec
import net.liftweb.mocks.MockHttpServletRequest
import net.liftweb.http._
import net.liftweb.common.Full


object RestHelperSpecBoot {
  def boot() {
    LiftRules.dispatch.append(RestHelperSpecRest)
  }
}


class RestHelperSpec extends WebSpec(RestHelperSpecBoot.boot _) {
  sequential  // This is important for using SessionVars, etc.

  "RestHelper" should {
    val testUrl = "http://foo.com/api/info"

    val testOptionsReq = new MockHttpServletRequest(testUrl){
      method = "OPTIONS"
    }

    "set OPTIONS method" withReqFor testOptionsReq in { req =>
      req.options_? must_== true
    }

    "give the correct response" withReqFor testOptionsReq in { req =>
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
