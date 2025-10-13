package net.liftweb.http.rest

import net.liftweb.mockweb.WebSpec
import net.liftweb.mocks.MockHttpServletRequest
import net.liftweb.common.Full

import net.liftweb.actor.LAFuture

import org.json4s._
import org.json4s.native._

import net.liftweb.http._

object RestHelperSpecBoot {
  def boot() : Unit = {
    LiftRules.dispatch.append(RestHelperSpecRest)
  }
}


class RestHelperSpec extends WebSpec(RestHelperSpecBoot.boot _) {
  sequential  // This is important for using SessionVars, etc.

  "RestHelper" should {
    val testOptionsUrl = "http://foo.com/api/info"
    val testPatchUrl = "http://foo.com/api/patched"
    val testFutureUrl = "http://foo.com/api/futured"

    val testOptionsReq = new MockHttpServletRequest(testOptionsUrl){
      method = "OPTIONS"
    }

    val testPatchReq = new MockHttpServletRequest(testPatchUrl){
      method = "PATCH"
    }

    val testFutureReq = new MockHttpServletRequest(testFutureUrl){
      method = "GET"
    }

    "set OPTIONS method" withReqFor testOptionsReq in { req =>
      req.options_? === true
    }

    "give the correct response" withReqFor testOptionsReq in { req =>
      RestHelperSpecRest(req)() must beLike {
        case Full(OkResponse()) => ok
      }
    }

    "set PATCH method" withReqFor testPatchReq in { req =>
      req.patch_? === true
    }

    "respond async with something that CanResolveAsync" withReqFor testFutureReq in { req =>
      val helper = FutureRestSpecHelper()

      try {
        helper(req)()

        failure("Failed to respond asynchronously.")
      } catch {
        case ContinuationException(_, _, resolverFunction) =>
          val result = new LAFuture[LiftResponse]

          resolverFunction({ response => result.satisfy(response) })

          helper.future.satisfy(JObject(Nil))

          result.get must beLike {
            case JsonResponse(_, _, _, code) =>
              code === 200
          }
      }
    }
  }
}

object RestHelperSpecRest extends RestHelper  {
  serve {
    case "api" :: "info" :: Nil Options req => OkResponse()
    case "api" :: "patched" :: Nil Patch req => OkResponse()
  }
}

case class FutureRestSpecHelper() extends RestHelper {
  val future = new LAFuture[JValue]

  serve {
    case "api" :: "futured" :: Nil Get _ =>
      future
  }
}
