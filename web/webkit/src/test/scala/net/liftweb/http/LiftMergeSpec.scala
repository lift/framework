package net.liftweb
package http

import scala.xml._

import org.specs2._
  import mutable.Specification
  import matcher.XmlMatchers
  import mock.Mockito

import org.mockito.Mockito._

import common._

import js.JE.JsObj

class LiftMergeSpec extends Specification with XmlMatchers with Mockito {
  val mockReq = mock[Req]
  mockReq.contextPath returns "/context-path"

  val testSession = new LiftSession("/context-path", "underlying id", Empty)

  val testRules = new LiftRules()
  // Avoid extra appended elements by default.
  testRules.javaScriptSettings.default.set(() => () => Empty)
  testRules.autoIncludeAjaxCalc.default.set(() => () => (_: LiftSession) => false)
  testRules.excludePathFromContextPathRewriting.default
    .set(
      () => { in: String =>
        in.startsWith("exclude-me")
      }
    )

  "LiftMerge when doing the final page merge" should {
    "merge head segments in the page body in order into main head" in new WithRules(testRules) {
      val result =
        testSession.merge(
          <html>
            <head>
              <script src="testscript"></script>
            </head>
            <body>
              <head>
                <script src="testscript2"></script>
                <link href="testlink" />
              </head>
              <div>
                <p>
                  <head>
                    <link href="testlink2" />
                  </head>
                </p>
              </div>
            </body>
          </html>,
          mockReq
        )

      (result \ "head" \ "_") must_== (Seq(
        <script src="testscript"></script>,
        <script src="testscript2"></script>,
        <link href="testlink" />,
        <link href="testlink2" />
      ): NodeSeq)
    }

    "merge tail segments in the page body in order at the end of the body" in new WithRules(testRules) {
      val result =
        testSession.merge(
          <html>
            <head>
              <script src="testscript"></script>
            </head>
            <body>
              <tail>
                <script src="testscript2"></script>
                <link href="testlink" />
              </tail>
              <div>
                <p>
                  <tail>
                    <link href="testlink2" />
                  </tail>
                </p>
              </div>

              <p>Thingies</p>
              <p>More thingies</p>
            </body>
          </html>,
          mockReq
        )

      (result \ "body" \ "_").takeRight(3) must_== (Seq(
        <script src="testscript2"></script>,
        <link href="testlink" />,
        <link href="testlink2" />
      ): NodeSeq)
    }

    "not merge tail segments in the head" in new WithRules(testRules) {
      val result =
        testSession.merge(
          <html>
            <head>
              <tail>
                <script src="testscript"></script>
              </tail>
            </head>
            <body>
              <tail>
                <script src="testscript2"></script>
                <link href="testlink" />
              </tail>
              <div>
                <p>
                  <tail>
                    <link href="testlink2" />
                  </tail>
                </p>
              </div>

              <p>Thingies</p>
              <p>More thingies</p>
            </body>
          </html>,
          mockReq
        )

      (result \ "body" \ "_").takeRight(3) must_== (Seq(
        <script src="testscript2"></script>,
        <link href="testlink" />,
        <link href="testlink2" />
      ): NodeSeq)
    }

    "normalize absolute link hrefs everywhere" in new WithLiftContext(testRules, testSession) {
      val result =
        testSession.merge(
          <html>
            <head>
              <script src="testscript"></script>
              <link href="/testlink" />
            </head>
            <body>
              <head>
                <script src="testscript2"></script>
                <link href="/testlink2" />
              </head>
              <div>
                <p>
                  <tail>
                    <link href="/testlink3" />
                  </tail>
                </p>
              </div>

              <p>Thingies</p>
              <p>More thingies</p>
            </body>
          </html>,
          mockReq
        )

      (result \\ "link").map(_ \@ "href") must_==
        "/context-path/testlink" ::
        "/context-path/testlink2" ::
        "/context-path/testlink3" :: Nil
    }

    "normalize absolute script srcs everywhere" in new WithLiftContext(testRules, testSession) {
      val result =
        testSession.merge(
          <html>
            <head>
              <script src="/testscript"></script>
              <link href="testlink" />
            </head>
            <body>
              <head>
                <script src="/testscript2"></script>
                <link href="testlink2" />
              </head>
              <div>
                <p>
                  <tail>
                    <link href="testlink3" />
                  </tail>
                </p>
              </div>

              <p>Thingies</p>
              <p>More thingies</p>
            </body>
          </html>,
          mockReq
        )

      (result \\ "script").map(_ \@ "src") must_==
        "/context-path/testscript" ::
        "/context-path/testscript2" :: Nil
    }

    "normalize absolute a hrefs everywhere" in new WithLiftContext(testRules, testSession) {
      val result =
        testSession.merge(
          <html>
            <head>
              <a href="/testa1">Booyan</a>
            </head>
            <body>
              <a href="/testa2">Booyan</a>
              <head>
                <a href="testa3">Booyan</a>
              </head>
              <div>
                <a href="testa4">Booyan</a>
                <p>
                  <tail>
                    <a href="/testa5">Booyan</a>
                  </tail>
                </p>
              </div>

              <p>Thingies <a href="/testa6">Booyan</a></p>
              <p>More thingies</p>
            </body>
          </html>,
          mockReq
        )

      (result \\ "a").map(_ \@ "href") must_==
        "/context-path/testa1" ::
        "testa3" ::
        "/context-path/testa2" ::
        "testa4" ::
        "/context-path/testa6" ::
        "/context-path/testa5" :: Nil
    }

    "normalize absolute form actions everywhere" in new WithLiftContext(testRules, testSession) {
      val result =
        testSession.merge(
          <html>
            <head>
              <form action="/testform1">Booyan</form>
            </head>
            <body>
              <form action="/testform2">Booyan</form>
              <head>
                <form action="testform3">Booyan</form>
              </head>
              <div>
                <form action="testform4">Booyan</form>
                <p>
                  <tail>
                    <form action="/testform5">Booyan</form>
                  </tail>
                </p>
              </div>

              <p>Thingies <form action="/testform6">Booyan</form></p>
              <p>More thingies</p>
            </body>
          </html>,
          mockReq
        )

      (result \\ "form").map(_ \@ "action") must_==
        "/context-path/testform1" ::
        "testform3" ::
        "/context-path/testform2" ::
        "testform4" ::
        "/context-path/testform6" ::
        "/context-path/testform5" :: Nil
    }

    "not rewrite script srcs anywhere" in new WithLiftContext(testRules, testSession) {
      val result =
        URLRewriter.doWith((_: String) => "rewritten") {
          testSession.merge(
            <html>
              <head>
                <script src="testscript"></script>
              </head>
              <body>
                <head>
                  <script src="testscript2"></script>
                </head>
                <div>
                  <p>
                    <tail>
                      <script src="testscript3" />
                    </tail>
                  </p>
                </div>

                <p>Thingies</p>
                <p>More thingies</p>
              </body>
            </html>,
            mockReq
          )
        }

      (result \\ "script").map(_ \@ "src") must_==
        "testscript" ::
        "testscript2" ::
        "testscript3" :: Nil
    }

    "not rewrite link hrefs anywhere" in new WithLiftContext(testRules, testSession) {
      val result =
        URLRewriter.doWith((_: String) => "rewritten") {
          testSession.merge(
            <html>
              <head>
                <link href="testlink" />
              </head>
              <body>
                <head>
                  <link href="testlink2" />
                </head>
                <div>
                  <p>
                    <tail>
                      <link href="testlink3" />
                    </tail>
                  </p>
                </div>

                <p>Thingies</p>
                <p>More thingies</p>
              </body>
            </html>,
            mockReq
          )
        }

      (result \\ "link").map(_ \@ "href") must_==
        "testlink" ::
        "testlink2" ::
        "testlink3" :: Nil
    }

    "rewrite a hrefs everywhere" in new WithLiftContext(testRules, testSession) {
      val result =
        URLRewriter.doWith((_: String) => "rewritten") {
          testSession.merge(
            <html>
              <head>
                <a href="testa"></a>
              </head>
              <body>
                <head>
                  <a href="testa2"></a>
                </head>
                <div>
                  <p>
                    <tail>
                      <a href="testa3" />
                    </tail>
                  </p>
                </div>

                <p>Thingies</p>
                <p>More thingies</p>
              </body>
            </html>,
            mockReq
          )
        }

      (result \\ "a").map(_ \@ "href") must_==
        "rewritten" ::
        "rewritten" ::
        "rewritten" :: Nil
    }

    "rewrite form actions everywhere" in new WithLiftContext(testRules, testSession) {
      val result =
        URLRewriter.doWith((_: String) => "rewritten") {
          testSession.merge(
            <html>
              <head>
                <form action="testform" />
              </head>
              <body>
                <head>
                  <form action="testform2" />
                </head>
                <div>
                  <p>
                    <tail>
                      <form action="testform3" />
                    </tail>
                  </p>
                </div>

                <p>Thingies</p>
                <p>More thingies</p>
              </body>
            </html>,
            mockReq
          )
        }

      (result \\ "form").map(_ \@ "action") must_==
        "rewritten" ::
        "rewritten" ::
        "rewritten" :: Nil
    }
  }
}
