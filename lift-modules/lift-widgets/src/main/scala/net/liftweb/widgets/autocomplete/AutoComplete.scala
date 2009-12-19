package net.liftweb.widgets.autocomplete


import _root_.scala.xml.{NodeSeq, Node, Elem, PCData, Text, Unparsed}
import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.js._
import JsCmds._
import JE._
import S._
import SHtml._
import Helpers._

object AutoComplete {

  def apply(start: String, 
            options: (String, Int) => Seq[String],
            onSubmit: String => Unit, 
            attrs: (String, String)*) = new AutoComplete().render(start, options, onSubmit, attrs:_*)

  def autocompleteObj[T](options: Seq[(T, String)], 
                         default: Box[T],
                         onSubmit: T => Unit): Elem = new AutoComplete().autocompleteObj(options, default, onSubmit)

    
    
  /**
   * register the resources with lift (typically in boot)
   */
  def init() {
    import net.liftweb.http.ResourceServer

    ResourceServer.allow({
        case "autocomplete" :: _ => true
     })
  }

}

class AutoComplete {
  
  /**
   * Create an autocomplete form based on a sequence.
   */
  def autocompleteObj[T](options: Seq[(T, String)], 
                         default: Box[T],
                         onSubmit: T => Unit): Elem = {
    val (nonces, defaultNonce, secureOnSubmit) = secureOptions(options, default, onSubmit)
    val defaultString = default.flatMap(d => options.find(_._1 == d).map(_._2))

    autocomplete_*(nonces, defaultString, defaultNonce, secureOnSubmit)
  }

  private def autocomplete_*(options: Seq[(String, String)], default: Box[String],
                     defaultNonce: Box[String], onSubmit: AFuncHolder): Elem = {
    val id = Helpers.nextFuncName

    fmapFunc(onSubmit){hidden =>
      
      val data = JsArray(options.map { 
        case (nonce, name) => JsObj("name" -> name, "nonce" -> nonce)
      } :_*)
      
      val autocompleteOptions = JsRaw("""{
      minChars: 0,
      matchContains: true,
      formatItem: function(row, i, max) { return row.name; },
    }""")

      val onLoad = JsRaw("""
      jQuery(document).ready(function(){
        var data = """+data.toJsCmd+""";
        jQuery("#"""+id+"""").autocomplete(data, """+autocompleteOptions.toJsCmd+""").result(function(event, dt, formatted) {
          jQuery("#"""+hidden+"""").val(formatted);
        });
      });""")

      <span>
        <head>
          <link rel="stylesheet" href={"/" + LiftRules.resourceServerPath +"/autocomplete/jquery.autocomplete.css"} type="text/css" />
          <script type="text/javascript" src={"/" + LiftRules.resourceServerPath +"/autocomplete/jquery.autocomplete.js"} />
          <script type="text/javascript">{Unparsed(onLoad.toJsCmd)}</script>
        </head>
        <input type="text" id={id} value={default.openOr("")} />
        <input type="hidden" name={hidden} id={hidden} value={defaultNonce.openOr("")} />
      </span>
     }
  }
  
  private def secureOptions[T](options: Seq[(T, String)], default: Box[T],
                                     onSubmit: T => Unit): (Seq[(String, String)], Box[String], AFuncHolder) = {
    val secure = options.map{case (obj, txt) => (obj, randomString(20), txt)}
    val defaultNonce = default.flatMap(d => secure.find(_._1 == d).map(_._2))
    val nonces = secure.map{case (obj, nonce, txt) => (nonce, txt)}
    def process(nonce: String): Unit = secure.find(_._2 == nonce).map(x => onSubmit(x._1))
    (nonces, defaultNonce, SFuncHolder(process))
  }
  
  /**
   * Render a text field with Ajax autocomplete support
   * 
   * @param start - the initial input string
   * @param option - the function to be called when user is typing text. The text and th options limit is provided to this functions
   * @param attrs - the attributes that can be added to the input text field 
   */
  def render(start: String, options: (String, Int) => Seq[String],
             onSubmit: String => Unit, attrs: (String, String)*): Elem = {
    
    val f = (ignore: String) => {
      val q = S.param("q").openOr("")
      val limit = S.param("limit").flatMap(asInt).openOr(10)
      PlainTextResponse(options(q, limit).map(s => s+"|"+s).mkString("\n"))
    }


    fmapFunc(SFuncHolder(f)){ func =>
      val what: String = encodeURL(S.contextPath + "/" + LiftRules.ajaxPath+"?"+func+"=foo")

      val id = Helpers.nextFuncName
      fmapFunc(SFuncHolder(onSubmit)){hidden =>

      val autocompleteOptions = JsRaw("""{
        minChars: 0,
        matchContains: true
      }""")
    
      val onLoad = JsRaw("""
      jQuery(document).ready(function(){
        var data = """+what.encJs+""";
        jQuery("#"""+id+"""").autocomplete(data, """+autocompleteOptions.toJsCmd+""").result(function(event, dt, formatted) {
          jQuery("#"""+hidden+"""").val(formatted);
        });
      });""")

      <span>
        <head>
          <link rel="stylesheet" href={"/" + LiftRules.resourceServerPath +"/autocomplete/jquery.autocomplete.css"} type="text/css" />
          <script type="text/javascript" src={"/" + LiftRules.resourceServerPath +"/autocomplete/jquery.autocomplete.js"} />
          <script type="text/javascript">{Unparsed(onLoad.toJsCmd)}</script>
        </head>
        {
          attrs.foldLeft(<input type="text" id={id} value={start} />)(_ % _)
        }
        <input type="hidden" name={hidden} id={hidden} value={start} />
      </span>
    }
   }
  }
}
