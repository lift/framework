/*
 * Copyright 2007-2010 WorldWide Conferencing, LLC
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

package net.liftweb {
package widgets {
package uploadprogress {

import _root_.scala.xml.{NodeSeq,Text}
import _root_.net.liftweb.http.{SessionVar,Req,GetRequest,PlainTextResponse,JsonResponse,
                                LiftRules,OnDiskFileParamHolder,S,ResourceServer,LiftResponse}
import _root_.net.liftweb.http.js.JsCmds._
import _root_.net.liftweb.http.js.JE._
import _root_.net.liftweb.common.{Box,Empty,Failure,Full}
import _root_.net.liftweb.util.{Log}

/**
 * A helper widget that makes it easy to do upload
 * progress bars using ajax polling.
 * 
 * @author Tim Perrett
 */
object UploadProgress {
  
  /**
   * Call UploadProgress.init from Boot.scala. This function sets up your
   * application to use Lift's streaming upload rather than memory based upload.
   * It also sets the maximum upload size to be 1GB... if you want a smaller number,
   * just call the LiftRules.maxMimeSize and LiftRules.maxMimeFileSize after calling
   * this objects init method in your Boot.scala.
   * 
   * <pre><code>
   *  import _root_.net.liftweb.widgets.uploadprogress.UploadProgress
   *  
   *  UploadProgress.init
   * </code></pre>
   */ 
  def init = {
    LiftRules.handleMimeFile = (fieldName, contentType, fileName, inputStream) =>
      OnDiskFileParamHolder(fieldName, contentType, fileName, inputStream)
    
    LiftRules.maxMimeSize = 1024 * 1024 * 1024
    LiftRules.maxMimeFileSize = LiftRules.maxMimeSize
    
    ResourceServer.allow({
      case "uploadprogress" :: "jquery.uploadprogress.0.3.js" :: Nil => true
      case "uploadprogress" :: "jquery.timers-1.1.2.js" :: Nil => true
      case "uploadprogress" :: "uploadprogress.js" :: Nil => true
    })
    
    LiftRules.dispatch.append {
      case Req("progress" :: Nil, "", GetRequest) => () => progressJsonResponse
    }
  }
  
  /**
   * If you want your own custom URL for the progress callbacks, but dont want
   * anything but the default response, just prepend your own DispatchPF in boot
   * that looks something like:
   * 
   * <pre><code>
   * LiftRules.dispatch.append {
   *  case Req("mycustomprogress" :: Nil, "", GetRequest) => () => UploadProgress.progressJsonResponse
   * }
   * </code></pre>
   */
  def progressJsonResponse: Full[LiftResponse] = {
    val recived: Double = StatusHolder.is.map(_._1.toDouble).openOr(0D)
    val size: Double = StatusHolder.is.map(_._2.toDouble).openOr(0D)
    val state: String = if(recived == size){ "completed" } else { "uploading" }
    Full(JsonResponse(
      JsObj(
        "state" -> state, 
        "percentage" -> Str(Math.floor((recived.toDouble / size.toDouble)*100).toString)
      )
    ))
  }
  
  /**
   * In order to get progress updating on a session by session basis, we have to
   * embed this function into the current users session and use a SessionVar
   * extension in order to keep a track of where we are too with the upload.
   */
  def sessionProgessListener =
    S.session.foreach(s => { 
      s.progressListener = Full((pBytesRead: Long, pBytesTotal: Long, pItem: Int) => {
        StatusHolder(Full((pBytesRead, pBytesTotal)))
      })
    })
  
  
  /**
   * Adds a default script to the page configurable by attributes passed to
   * the widget. If you want customer behaviour, just make your own JS based on
   * the avalible options:
   *  - dataType: "json",
   *  - interval: 1500,
   *  - progressBar: "#progressbar",
   *  - progressUrl: "/progress",
   *  - start: function() {},
   *  - uploading: function() {},
   *  - complete: function() {},
   *  - success: function() {},
   *  - error: function() {},
   *  - preloadImages: [],
   *  - uploadProgressPath: '/classpath/uploadprogress/uploadprogress.js',
   *  - jqueryPath: '/classpath/jquery.js',
   *  - timer: ""
   */
  def head(xhtml: NodeSeq): NodeSeq = {
    UploadProgress.sessionProgessListener
    Script(Run("""
    $(function() {
      $('""" + S.attr("formId").openOr("form") + """').uploadProgress({
        start:function(){ },
        uploading: function(upload){ $('#percents').text(upload.percentage+'%'); },
        progressBar: '#""" + S.attr("progressBar").openOr("progressbar") + """',
        progressUrl: '""" + S.attr("progressUrl").openOr("/progress") + """',
        success: function(upload){ $('#percents').text('Finishing...'); },
        interval: """ + S.attr("interval").openOr("200") + """
      });
    });
    """))
  }
}

/**
 * State holder for the number of bytes uploaded of the current upload
 * in the existing session.
 */
object StatusHolder extends SessionVar[Box[(Long, Long)]](Empty)

}
}
}
