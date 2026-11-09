/*
 * Copyright 2007-2011 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package bootstrap.liftweb

import net.liftweb.common._
import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.sitemap._
import net.liftweb.sitemap.Loc._
import Helpers._

/**
  * A class that's instantiated early and run.  It allows the application
  * to modify lift's environment
  */
class Boot {
  def boot {
    // where to search snippet
    LiftRules.addToPackages("net.liftweb.webapptest")

    LiftRules.dispatch.append(ContainerVarTests)
    LiftRules.dispatch.append(LongPollingTests.dispatch)
    LiftRules.dispatch.append(UploadEchoTests.dispatch)
  }
}

import rest._

case class Moose(str: String)

object ContainerVarTests extends RestHelper {
  object StrVar extends ContainerVar("Hello")
  object IntVar extends ContainerVar(45)
  // object CaseVar extends ContainerVar(Moose("dog"))

  serve {
    case "cv_int" :: Nil Get _ => <int>{IntVar.is}</int>
    case "cv_int" :: AsInt(i) :: _ Get _ => {
      IntVar.set(i)
      <int>{IntVar.is}</int>
    }
  }

  serve {
    case "cv_str" :: Nil Get _ => <str>{StrVar.is}</str>
    case "cv_str" :: str :: _ Get _ => {
      StrVar.set(str)
      <str>{StrVar.is}</str>
    }
  }
}

/**
 * Long-polling endpoints used by the servlet characterization tests.
 * Each endpoint answers through RestContinuation.async, the public entry
 * point for asynchronous responses, so the tests exercise the same
 * suspend/resume (or fallback) contract that Comet uses.
 *
 * Thread capture: the thread that executes the reply body is recorded in
 * `replyThread`, and the thread that ran the request handler in
 * `requestThread`. The servlet characterization tests read these to prove
 * that the reply body runs on a thread distinct from the request thread.
 */
object LongPollingTests {
  import java.util.concurrent.atomic.AtomicReference

  val requestThread = new AtomicReference[String]("unset")
  val replyThread = new AtomicReference[String]("unset")

  /**
   * Respond immediately (modulo the framework's 5ms scheduling delay) with
   * `<answer>{msg}</answer>`, recording the request and reply threads.
   */
  def echo: LiftRules.DispatchPF = {
    case req @ Req("lp" :: "echo" :: Nil, _, GetRequest) =>
      val msg = req.param("msg").headOption.getOrElse("none")
      () => {
        requestThread.set(Thread.currentThread().getName)
        RestContinuation.async { reply =>
          replyThread.set(Thread.currentThread().getName)
          reply(XmlResponse(<answer>{msg}</answer>))
        }
      }
  }

  /**
    * Respond after `delay` milliseconds with `<answer>done</answer>`.
    * Used to exercise the time budget: the client waits while the answer is
    * computed later, within or beyond the configured comet timeout budget.
    */
  def delayed(delayMs: Int): LiftRules.DispatchPF = {
    case Req("lp" :: "delay" :: Nil, _, GetRequest) =>
      () => {
        requestThread.set(Thread.currentThread().getName)
        RestContinuation.async { reply =>
          net.liftweb.util.Schedule.schedule(() => reply(XmlResponse(<answer>done</answer>)), TimeSpan(delayMs))
        }
      }
  }

  /**
    * Respond immediately, reporting whether the reply body observes request
    * state: the LiftSession loaned around the reply by the continuation
    * machinery.
    */
  def stateReport: LiftRules.DispatchPF = {
    case Req("lp" :: "state" :: Nil, _, GetRequest) =>
      () => {
        RestContinuation.async { reply =>
          val sessionDefined = S.session.isDefined
          reply(XmlResponse(<state>{sessionDefined}</state>))
        }
      }
  }

  def dispatch: LiftRules.DispatchPF = echo orElse delayed(0) orElse stateReport
}

/**
 * Upload endpoint used by the servlet characterization tests: echoes the
 * uploaded files as XML with field name, filename, mime type, byte size,
 * and a SHA-256 digest of the file contents, so a test can compare what the
 * server received with what the client sent, byte for byte.
 */
object UploadEchoTests {
  def dispatch: LiftRules.DispatchPF = {
    case Req("lp" :: "upload" :: Nil, _, PostRequest) =>
      () => {
        val files = S.request.map(_.uploadedFiles).openOr(Nil)
        val digest = java.security.MessageDigest.getInstance("SHA-256")
        val xml = files.map { f =>
          val sha = digest.digest(f.file).map(b => "%02x".format(b)).mkString
          <file>
            <field>{f.name}</field>
            <fileName>{f.fileName}</fileName>
            <mime>{f.mimeType}</mime>
            <size>{f.length}</size>
            <sha256>{sha}</sha256>
          </file>
        }
        Full(XmlResponse(<files>{xml}</files>))
      }
  }
}
