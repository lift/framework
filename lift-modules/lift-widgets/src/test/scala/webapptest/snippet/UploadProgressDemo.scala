package webapptest.snippet

import _root_.scala.xml._
import _root_.net.liftweb.http._
import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.http.js._
import _root_.net.liftweb.http.js.JsCmds._
import _root_.net.liftweb.widgets.uploadprogress.UploadProgress

/**
 * This is a pure example class - nothing more.
 * Use this as a base for implementing the widget
 * in your own applications.
 */
object UploadProgressDemo extends DispatchSnippet {
  
  private object theUpload extends RequestVar[Box[FileParamHolder]](Empty)
  
  def dispatch = { 
    case "upload" => upload _ 
    case "script" => UploadProgress.head _
  }
  
  def upload(xhtml: NodeSeq): NodeSeq = {
    if (S.get_?){
      bind("ul", chooseTemplate("choose", "get", xhtml),
           "file_upload" -> SHtml.fileUpload(ul => theUpload(Full(ul))))
    } else {
      bind("ul", chooseTemplate("choose", "post", xhtml),
           "file_name" -> theUpload.is.map(v => Text(v.fileName)),
           "mime_type" -> theUpload.is.map(v => Box.legacyNullTest(v.mimeType).map(Text).openOr(Text("No mime type supplied"))),
           "length" -> theUpload.is.map(v => Text(v.file.length.toString)),
           "md5" -> theUpload.is.map(v => Text(hexEncode(md5(v.file))))
      )
    }  
  }
}
