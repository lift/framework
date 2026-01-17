package net.liftweb
package http

import scala.xml._

import org.specs2._
  import execute.{Result, AsResult}
  import mutable.Specification
  import matcher.XmlMatchers

import common._

import js.JE.JsObj

class HtmlNormalizerSpec extends Specification with XmlMatchers {
  val eventAttributeMatcher = "(?s).*\\W(on[a-zA-Z]+)=.*".r

  "HtmlNormalizer when normalizing HTML and event handlers" should {
    "leave the HTML structure unchanged" in {
      val result =
        HtmlNormalizer.normalizeHtmlAndEventHandlers(
          <html>
            <head>
              <script src="testscript"></script>
              <link href="testlink" />
            </head>
            <body>
              <link href="testlink2" />
              <form action="booyan">
                <p>
                  <link href="testlink3" />
                </p>
              </form>

              <p>Thingies</p>
              <p>More thingies</p>
            </body>
          </html>,
          "/context-path",
          false,
          true
        ).nodes

      result must ==/(
        <html>
          <head>
            <script src="testscript"></script>
            <link href="testlink" />
          </head>
          <body>
            <link href="testlink2" />
            <form action="booyan">
              <p>
                <link href="testlink3" />
              </p>
            </form>

            <p>Thingies</p>
            <p>More thingies</p>
          </body>
        </html>
      )
    }

    "extract events from all elements at any depth" in {
      val NodesAndEventJs(html, js) =
        HtmlNormalizer.normalizeHtmlAndEventHandlers(
          <html onevent="testJs1">
            <head onresult="testJs2">
              <script src="testscript" onmagic="testJs3"></script>
              <link href="testlink" onthing="testJs4" />
            </head>
            <body onclick="testJs5">
              <link href="testlink2" onclick="testJs5" />
              <form action="booyan" onsubmit="testJs6">
                <p onmouseover="testJs7">
                  <link href="testlink3" onslippetydip="testJs8" />
                </p>
              </form>

              <p onkeyup="testJs9">Thingies</p>
              <p ondragstart="testJs10">More thingies</p>
            </body>
          </html>,
          "/context-path",
          false,
          true
        )

      List("testJs1",
           "testJs2",
           "testJs3",
           "testJs4",
           "testJs5",
           "testJs6",
           "testJs7",
           "testJs8",
           "testJs9",
           "testJs10")
        .foreach(js.toJsCmd must contain(_))

      html.toString must beLike {
        case eventAttributeMatcher(eventAttribute) =>
          ko(s"Contained unexpected event: ${eventAttribute}")
        case _ =>
          ok
      }
    }

    "reuse ids when they are already on an element with an event" in {
      val NodesAndEventJs(html, js) =
        HtmlNormalizer.normalizeHtmlAndEventHandlers(
          <myelement id="testid" onevent="doStuff" />,
          "/context-path",
          false,
          true
        )

      html must ==/(<myelement id="testid" />)
      js.toJsCmd === """lift.onEvent("testid","event",function(event) {doStuff;});"""
    }

    "generate ids for elements with events if they don't have one" in {
      val NodesAndEventJs(html, js) =
        HtmlNormalizer.normalizeHtmlAndEventHandlers(
          <myelement onevent="doStuff" />,
          "/context-path",
          false,
          true
        )

      val id = html \@ "id"

      id must not(beEmpty)
      js.toJsCmd === s"""lift.onEvent("$id","event",function(event) {doStuff;});"""
    }

    "extract event js correctly for multiple elements" in {
      val NodesAndEventJs(_, js) =
        HtmlNormalizer.normalizeHtmlAndEventHandlers(
          <div>
            <myelement onevent="doStuff" />
            <myelement id="hello" onevent="doStuff2" />
            <myelement onevent="doStuff3" />
          </div>,
          "/context-path",
          false,
          true
        )

      js.toJsCmd must beMatching("""(?s)\Qlift.onEvent("lift-event-js-\E[^"]+\Q","event",function(event) {doStuff;});
        |lift.onEvent("hello","event",function(event) {doStuff2;});
        |lift.onEvent("lift-event-js-\E[^"]+\Q","event",function(event) {doStuff3;});\E""".stripMargin('|').linesIterator.mkString("\n").r
      )
    }

    "extract events from hrefs and actions" in {
      val NodesAndEventJs(html, js) =
        HtmlNormalizer.normalizeHtmlAndEventHandlers(
          <div>
            <myelement href="javascript:doStuff" />
            <myelement id="hello" action="javascript:doStuff2" />
            <myelement id="hello2" href="javascript://doStuff3" />
            // Note here we have the same behavior as browsers: javascript:/
            // is *processed as JavaScript* but it is *invalid JavaScript*
            // (i.e., it corresponds to a JS expression that starts with `/`).
            <myelement action="javascript:/doStuff4" />
          </div>,
          "/context-path",
          false,
          true
        )

      (html \ "myelement").map(_ \@ "href").filter(_.nonEmpty) must beEmpty
      (html \ "myelement").map(_ \@ "action").filter(_.nonEmpty) must beEmpty
      js.toJsCmd must beMatching("""(?s)\Qlift.onEvent("lift-event-js-\E[^"]+\Q","click",function(event) {doStuff; event.preventDefault();});
        |lift.onEvent("hello","submit",function(event) {doStuff2; event.preventDefault();});
        |lift.onEvent("hello2","click",function(event) {doStuff3; event.preventDefault();});
        |lift.onEvent("lift-event-js-\E[^"]+\Q","submit",function(event) {/doStuff4; event.preventDefault();});\E""".stripMargin('|').linesIterator.mkString("\n").r
      )
    }

    "not extract events from hrefs and actions without the proper prefix" in {
      val NodesAndEventJs(html, js) =
        HtmlNormalizer.normalizeHtmlAndEventHandlers(
          <div>
            <myelement href="doStuff" />
            <myelement id="hello" action="javascrip:doStuff2" />
            <myelement id="hello2" href="javascrip://doStuff3" />
            <myelement action="doStuff4" />
          </div>,
          "/context-path",
          false,
          true
        )

      (html \ "myelement").map(_ \@ "href").filter(_.nonEmpty) === List("doStuff", "javascrip://doStuff3")
      (html \ "myelement").map(_ \@ "action").filter(_.nonEmpty) === List("javascrip:doStuff2", "doStuff4")
      js.toJsCmd.trim must beEmpty
    }

    "normalize absolute link hrefs everywhere" in {
      val result =
        HtmlNormalizer.normalizeHtmlAndEventHandlers(
          <html>
            <head>
              <script src="testscript"></script>
              <link href="/testlink" />
            </head>
            <body>
              <link href="/testlink2" />
              <div>
                <p>
                  <link href="/testlink3" />
                </p>
              </div>

              <p>Thingies</p>
              <p>More thingies</p>
            </body>
          </html>,
          "/context-path",
          false,
          true
        ).nodes

      (result \\ "link").map(_ \@ "href") ===
        "/context-path/testlink" ::
        "/context-path/testlink2" ::
        "/context-path/testlink3" :: Nil
    }

    "normalize absolute script srcs everywhere" in {
      val result =
        HtmlNormalizer.normalizeHtmlAndEventHandlers(
          <html>
            <head>
              <script src="/testscript"></script>
              <link href="testlink" />
            </head>
            <body>
              <script src="/testscript2"></script>
              <link href="testlink2" />
              <div>
                <p>
                  <link href="testlink3" />
                </p>
              </div>

              <p>Thingies</p>
              <p>More thingies</p>
            </body>
          </html>,
          "/context-path",
          false,
          true
        ).nodes

      (result \\ "script").map(_ \@ "src") ===
        "/context-path/testscript" ::
        "/context-path/testscript2" :: Nil
    }

    "normalize absolute a hrefs everywhere" in {
      val result =
        HtmlNormalizer.normalizeHtmlAndEventHandlers(
          <html>
            <head>
              <a href="/testa1">Booyan</a>
            </head>
            <body>
              <a href="/testa2">Booyan</a>
              <a href="testa3">Booyan</a>
              <div>
                <a href="testa4">Booyan</a>
                <p>
                  <a href="/testa5">Booyan</a>
                </p>
              </div>

              <p>Thingies <a href="/testa6">Booyan</a></p>
              <p>More thingies</p>
            </body>
          </html>,
          "/context-path",
          false,
          true
        ).nodes

      (result \\ "a").map(_ \@ "href") ===
        "/context-path/testa1" ::
        "/context-path/testa2" ::
        "testa3" ::
        "testa4" ::
        "/context-path/testa5" ::
        "/context-path/testa6" :: Nil
    }

    "normalize absolute form actions everywhere" in {
      val result =
        HtmlNormalizer.normalizeHtmlAndEventHandlers(
          <html>
            <head>
              <form action="/testform1">Booyan</form>
            </head>
            <body>
              <form action="/testform2">Booyan</form>
              <form action="testform3">Booyan</form>
              <div>
                <form action="testform4">Booyan</form>
                <p>
                  <form action="/testform5">Booyan</form>
                </p>
              </div>

              <p>Thingies <form action="/testform6">Booyan</form></p>
              <p>More thingies</p>
            </body>
          </html>,
          "/context-path",
          false,
          true
        ).nodes

      (result \\ "form").map(_ \@ "action") ===
        "/context-path/testform1" ::
        "/context-path/testform2" ::
        "testform3" ::
        "testform4" ::
        "/context-path/testform5" ::
        "/context-path/testform6" :: Nil
    }

    "not rewrite script srcs anywhere" in {
      val result =
        URLRewriter.doWith((_: String) => "rewritten") {
          HtmlNormalizer.normalizeHtmlAndEventHandlers(
            <html>
              <head>
                <script src="testscript"></script>
              </head>
              <body>
                <script src="testscript2"></script>
                <div>
                  <p>
                    <script src="testscript3" />
                  </p>
                </div>

                <p>Thingies</p>
                <p>More thingies</p>
              </body>
            </html>,
            "/context-path",
            false,
            true
          ).nodes
        }

      (result \\ "script").map(_ \@ "src") ===
        "testscript" ::
        "testscript2" ::
        "testscript3" :: Nil
    }

    "not rewrite link hrefs anywhere" in {
      val result =
        URLRewriter.doWith((_: String) => "rewritten") {
          HtmlNormalizer.normalizeHtmlAndEventHandlers(
            <html>
              <head>
                <link href="testlink" />
              </head>
              <body>
                <link href="testlink2" />
                <div>
                  <p>
                    <link href="testlink3" />
                  </p>
                </div>

                <p>Thingies</p>
                <p>More thingies</p>
              </body>
            </html>,
            "/context-path",
            false,
            true
          ).nodes
        }

      (result \\ "link").map(_ \@ "href") ===
        "testlink" ::
        "testlink2" ::
        "testlink3" :: Nil
    }

    "rewrite a hrefs everywhere" in {
      val result =
        URLRewriter.doWith((_: String) => "rewritten") {
          HtmlNormalizer.normalizeHtmlAndEventHandlers(
            <html>
              <head>
                <a href="testa"></a>
              </head>
              <body>
                <a href="testa2"></a>
                <div>
                  <p>
                    <a href="testa3" />
                  </p>
                </div>

                <p>Thingies</p>
                <p>More thingies</p>
              </body>
            </html>,
            "/context-path",
            false,
            true
          ).nodes
        }

      (result \\ "a").map(_ \@ "href") ===
        "rewritten" ::
        "rewritten" ::
        "rewritten" :: Nil
    }

    "rewrite form actions everywhere" in {
      val result =
        URLRewriter.doWith((_: String) => "rewritten") {
          HtmlNormalizer.normalizeHtmlAndEventHandlers(
            <html>
              <head>
                <form action="testform" />
              </head>
              <body>
                <form action="testform2" />
                <div>
                  <p>
                    <form action="testform3" />
                  </p>
                </div>

                <p>Thingies</p>
                <p>More thingies</p>
              </body>
            </html>,
            "/context-path",
            false,
            true
          ).nodes
        }

      (result \\ "form").map(_ \@ "action") ===
        "rewritten" ::
        "rewritten" ::
        "rewritten" :: Nil
    }
  }

  "HtmlNormalizer when normalizing HTML and event handlers with event extract disabled" should {
    "not extract events from any elements at any depth" in {
      val startingHtml =
        <html onevent="testJs1">
          <head onresult="testJs2">
            <script src="testscript" onmagic="testJs3"></script>
            <link href="testlink" onthing="testJs4" />
          </head>
          <body onclick="testJs5">
            <link href="testlink2" onclick="testJs5" />
            <form action="booyan" onsubmit="testJs6">
              <p onmouseover="testJs7">
                <link href="testlink3" onslippetydip="testJs8" />
              </p>
            </form>

            <p onkeyup="testJs9">Thingies</p>
            <p ondragstart="testJs10">More thingies</p>
          </body>
        </html>

      val NodesAndEventJs(html, js) =
        HtmlNormalizer.normalizeHtmlAndEventHandlers(
          startingHtml,
          "/context-path",
          false,
          false
        )

      html.toString === startingHtml.toString
      js.toJsCmd.length === 0
    }

    "not extract events from hrefs and actions" in {
      val startingHtml =
          <div>
            <myelement href="javascript:doStuff" />
            <myelement id="hello" action="javascript:doStuff2" />
            <myelement id="hello2" href="javascript://doStuff3" />
            // Note here we have the same behavior as browsers: javascript:/
            // is *processed as JavaScript* but it is *invalid JavaScript*
            // (i.e., it corresponds to a JS expression that starts with `/`).
            <myelement action="javascript:/doStuff4" />
          </div>

      val NodesAndEventJs(html, js) =
        HtmlNormalizer.normalizeHtmlAndEventHandlers(
          startingHtml,
          "/context-path",
          false,
          false
        )

      html.toString === startingHtml.toString
      js.toJsCmd.length === 0
    }

    "normalize absolute link hrefs everywhere" in {
      val result =
        HtmlNormalizer.normalizeHtmlAndEventHandlers(
          <html>
            <head>
              <script src="testscript"></script>
              <link href="/testlink" />
            </head>
            <body>
              <link href="/testlink2" />
              <div>
                <p>
                  <link href="/testlink3" />
                </p>
              </div>

              <p>Thingies</p>
              <p>More thingies</p>
            </body>
          </html>,
          "/context-path",
          false,
          false
        ).nodes

      (result \\ "link").map(_ \@ "href") ===
        "/context-path/testlink" ::
        "/context-path/testlink2" ::
        "/context-path/testlink3" :: Nil
    }

    "normalize absolute script srcs everywhere" in {
      val result =
        HtmlNormalizer.normalizeHtmlAndEventHandlers(
          <html>
            <head>
              <script src="/testscript"></script>
              <link href="testlink" />
            </head>
            <body>
              <script src="/testscript2"></script>
              <link href="testlink2" />
              <div>
                <p>
                  <link href="testlink3" />
                </p>
              </div>

              <p>Thingies</p>
              <p>More thingies</p>
            </body>
          </html>,
          "/context-path",
          false,
          false
        ).nodes

      (result \\ "script").map(_ \@ "src") ===
        "/context-path/testscript" ::
        "/context-path/testscript2" :: Nil
    }

    "normalize absolute a hrefs everywhere" in {
      val result =
        HtmlNormalizer.normalizeHtmlAndEventHandlers(
          <html>
            <head>
              <a href="/testa1">Booyan</a>
            </head>
            <body>
              <a href="/testa2">Booyan</a>
              <a href="testa3">Booyan</a>
              <div>
                <a href="testa4">Booyan</a>
                <p>
                  <a href="/testa5">Booyan</a>
                </p>
              </div>

              <p>Thingies <a href="/testa6">Booyan</a></p>
              <p>More thingies</p>
            </body>
          </html>,
          "/context-path",
          false,
          false
        ).nodes

      (result \\ "a").map(_ \@ "href") ===
        "/context-path/testa1" ::
        "/context-path/testa2" ::
        "testa3" ::
        "testa4" ::
        "/context-path/testa5" ::
        "/context-path/testa6" :: Nil
    }

    "normalize absolute form actions everywhere" in {
      val result =
        HtmlNormalizer.normalizeHtmlAndEventHandlers(
          <html>
            <head>
              <form action="/testform1">Booyan</form>
            </head>
            <body>
              <form action="/testform2">Booyan</form>
              <form action="testform3">Booyan</form>
              <div>
                <form action="testform4">Booyan</form>
                <p>
                  <form action="/testform5">Booyan</form>
                </p>
              </div>

              <p>Thingies <form action="/testform6">Booyan</form></p>
              <p>More thingies</p>
            </body>
          </html>,
          "/context-path",
          false,
          false
        ).nodes

      (result \\ "form").map(_ \@ "action") ===
        "/context-path/testform1" ::
        "/context-path/testform2" ::
        "testform3" ::
        "testform4" ::
        "/context-path/testform5" ::
        "/context-path/testform6" :: Nil
    }

    "not rewrite script srcs anywhere" in {
      val result =
        URLRewriter.doWith((_: String) => "rewritten") {
          HtmlNormalizer.normalizeHtmlAndEventHandlers(
            <html>
              <head>
                <script src="testscript"></script>
              </head>
              <body>
                <script src="testscript2"></script>
                <div>
                  <p>
                    <script src="testscript3" />
                  </p>
                </div>

                <p>Thingies</p>
                <p>More thingies</p>
              </body>
            </html>,
            "/context-path",
            false,
            false
          ).nodes
        }

      (result \\ "script").map(_ \@ "src") ===
        "testscript" ::
        "testscript2" ::
        "testscript3" :: Nil
    }

    "not rewrite link hrefs anywhere" in {
      val result =
        URLRewriter.doWith((_: String) => "rewritten") {
          HtmlNormalizer.normalizeHtmlAndEventHandlers(
            <html>
              <head>
                <link href="testlink" />
              </head>
              <body>
                <link href="testlink2" />
                <div>
                  <p>
                    <link href="testlink3" />
                  </p>
                </div>

                <p>Thingies</p>
                <p>More thingies</p>
              </body>
            </html>,
            "/context-path",
            false,
            false
          ).nodes
        }

      (result \\ "link").map(_ \@ "href") ===
        "testlink" ::
        "testlink2" ::
        "testlink3" :: Nil
    }

    "rewrite a hrefs everywhere" in {
      val result =
        URLRewriter.doWith((_: String) => "rewritten") {
          HtmlNormalizer.normalizeHtmlAndEventHandlers(
            <html>
              <head>
                <a href="testa"></a>
              </head>
              <body>
                <a href="testa2"></a>
                <div>
                  <p>
                    <a href="testa3" />
                  </p>
                </div>

                <p>Thingies</p>
                <p>More thingies</p>
              </body>
            </html>,
            "/context-path",
            false,
            false
          ).nodes
        }

      (result \\ "a").map(_ \@ "href") ===
        "rewritten" ::
        "rewritten" ::
        "rewritten" :: Nil
    }

    "rewrite form actions everywhere" in {
      val result =
        URLRewriter.doWith((_: String) => "rewritten") {
          HtmlNormalizer.normalizeHtmlAndEventHandlers(
            <html>
              <head>
                <form action="testform" />
              </head>
              <body>
                <form action="testform2" />
                <div>
                  <p>
                    <form action="testform3" />
                  </p>
                </div>

                <p>Thingies</p>
                <p>More thingies</p>
              </body>
            </html>,
            "/context-path",
            false,
            false
          ).nodes
        }

      (result \\ "form").map(_ \@ "action") ===
        "rewritten" ::
        "rewritten" ::
        "rewritten" :: Nil
    }
  }
}
