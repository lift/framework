package net.liftweb.scalate

import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import xml.NodeSeq

/**
 * A {@link LiftView} which uses a <a href="http://scalate.fusesource.org/">Scalate</a>
 * template engine to resolve a URI and render it as markup
 *
 * @version $Revision : 1.1 $
 */
class ScalateView(engine: LiftTemplateEngine = new LiftTemplateEngine()) extends LiftView with Logger {

  /**
   * Registers this view with Lift's dispatcher
   */
  def register: Unit = {
    val scalateView: ScalateView = this

    // TODO no idea why viewDispatch doesn't work, so lets just plugin the dispatcher instead
    LiftRules.dispatch.prepend(NamedPF("Scalate Dispatch") {
      case Req(path, ext, GetRequest) if (scalateView.canRender(path, ext)) => scalateView.render(path, ext)
    })

    // TODO view dispatch doesn't seem to do anything....
/*
    LiftRules.viewDispatch.prepend(NamedPF("Scalate View") {
      case Req(path, ext, GetRequest) =>
        info("scalate viewDispatch Path: " + path + " ext: " + ext)
        Right(scalateView)
    })
*/
  }


  def dispatch: PartialFunction[String, () => Box[NodeSeq]] = {
    case v if (canLoad(v)) =>
      () => {
        val template = engine.load(v)
        Full(engine.layoutAsNodes(template))
      }
  }


  def canRender(path: List[String], ext: String): Boolean = {
    debug("=== attempting to find: " + path + " ext: '" + ext + "'")

    if (ext == "") {
      canLoad(createUri(path, "scaml")) || canLoad(createUri(path, "ssp"))
    }
    else {
      val uri = createUri(path, ext)
      (uri.endsWith(".ssp") || uri.endsWith(".scaml")) && canLoad(uri)
    }
  }


  def render(path: List[String], ext: String): () => Box[LiftResponse] = {
    debug("attempting to render: " + path + " extension: " + ext)

    () => {
      val uri: String = if (ext != "") createUri(path, ext) else {
        List("scaml", "ssp").map(createUri(path, _)).find(engine.canLoad(_)).get
      }
      val template = engine.load(uri)
      Full(TextResponse(engine.layout(template)))
    }
  }


  protected def createUri(path: List[String], ext: String): String = path.mkString("/") +
          (if (ext.length > 0) "." + ext else "")

  protected def canLoad(v: String): Boolean = {
    engine.canLoad(v)
  }


  case class TextResponse(text: String, headers: List[(String, String)] = Nil, code: Int = 200, contentType: String = "text/html; charset=utf-8") extends LiftResponse {
    def toResponse = {
      val bytes = text.getBytes("UTF-8")
      InMemoryResponse(bytes, ("Content-Length", bytes.length.toString) :: ("Content-Type", contentType) :: headers, Nil, code)
    }
  }
}