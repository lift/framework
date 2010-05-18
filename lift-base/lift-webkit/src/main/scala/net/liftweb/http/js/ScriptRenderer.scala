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
package http {
package js {

import _root_.net.liftweb.http._
import _root_.net.liftweb.common._
import _root_.net.liftweb.util._

object ScriptRenderer {

  /**
   * Renders the default ajax script use by lift
   */
  def ajaxScript = JsCmds.Run("""

(function() {

  window.liftAjax = {
    lift_ajaxQueue: [],
    lift_ajaxInProcess: null,
    lift_ajaxShowing: false,
    lift_ajaxRetryCount: """ + (LiftRules.ajaxRetryCount openOr 3) + """,

    lift_ajaxHandler: function(theData, theSuccess, theFailure, responseType){
	  var toSend = {retryCnt: 0};
	  toSend.when = (new Date()).getTime();
	  toSend.theData = theData;
	  toSend.onSuccess = theSuccess;
	  toSend.onFailure = theFailure;
	  toSend.responseType = responseType;

	  liftAjax.lift_ajaxQueue.push(toSend);
	  liftAjax.lift_ajaxQueueSort();
	  liftAjax.lift_doAjaxCycle();
	  return false; // buttons in forms don't trigger the form

    },

    lift_uriSuffix: undefined,

    lift_ajaxQueueSort: function() {
      liftAjax.lift_ajaxQueue.sort(function (a, b) {return a.when - b.when;});
    },

    lift_defaultFailure: function() {
      """ + (LiftRules.ajaxDefaultFailure.map(_().toJsCmd) openOr "") + """
    },

    lift_startAjax: function() {
      liftAjax.lift_ajaxShowing = true;
      """ + (LiftRules.ajaxStart.map(_().toJsCmd) openOr "") + """
    },

    lift_endAjax: function() {
      liftAjax.lift_ajaxShowing = false;
      """ + (LiftRules.ajaxEnd.map(_().toJsCmd) openOr "") + """
    },

    lift_testAndShowAjax: function() {
      if (liftAjax.lift_ajaxShowing && liftAjax.lift_ajaxQueue.length == 0 && liftAjax.lift_ajaxInProcess == null) {
        liftAjax.lift_endAjax();
      } else if (!liftAjax.lift_ajaxShowing && (liftAjax.lift_ajaxQueue.length > 0 || liftAjax.lift_ajaxInProcess != null)) {
        liftAjax.lift_startAjax();
      }
    },

    lift_traverseAndCall: function(node, func) {
      if (node.nodeType == 1) func(node);
      var i = 0;
      var cn = node.childNodes;

      for (i = 0; i < cn.length; i++) {
        liftAjax.lift_traverseAndCall(cn.item(i), func);
      }
    },

    lift_successRegisterGC: function() {
      setTimeout("liftAjax.lift_registerGC()", """ + LiftRules.liftGCPollingInterval + """);
    },

    lift_failRegisterGC: function() {
      setTimeout("liftAjax.lift_registerGC()", """ + LiftRules.liftGCFailureRetryTimeout + """);
    },

    lift_registerGC: function() {
      var data = "__lift__GC=_"
      """ + LiftRules.jsArtifacts.ajax(AjaxInfo(JE.JsRaw("data"),
    "POST",
    LiftRules.ajaxPostTimeout,
    false, "script",
    Full("liftAjax.lift_successRegisterGC"), Full("liftAjax.lift_failRegisterGC"))) +
          """
       },

       lift_doAjaxCycle: function() {
         var queue = liftAjax.lift_ajaxQueue;
         if (queue.length > 0) {
           var now = (new Date()).getTime();
           if (liftAjax.lift_ajaxInProcess == null && queue[0].when <= now) {
             var aboutToSend = queue.shift();

             liftAjax.lift_ajaxInProcess = aboutToSend;

             var successFunc = function(data) {
               liftAjax.lift_ajaxInProcess = null;
               if (aboutToSend.onSuccess) {
                 aboutToSend.onSuccess(data);
               }
               liftAjax.lift_doAjaxCycle();
             };

             var failureFunc = function() {
               liftAjax.lift_ajaxInProcess = null;
               var cnt = aboutToSend.retryCnt;
               if (cnt < liftAjax.lift_ajaxRetryCount) {
               aboutToSend.retryCnt = cnt + 1;
                 var now = (new Date()).getTime();
                 aboutToSend.when = now + (1000 * Math.pow(2, cnt));
                 queue.push(aboutToSend);
                 liftAjax.lift_ajaxQueueSort();
               } else {
                 if (aboutToSend.onFailure) {
                   aboutToSend.onFailure();
                 } else {
                   liftAjax.lift_defaultFailure();
                 }
               }
               liftAjax.lift_doAjaxCycle();
             };

             if (aboutToSend.responseType != undefined &&
                 aboutToSend.responseType != null &&
                 aboutToSend.responseType.toLowerCase() === "json") {
               liftAjax.lift_actualJSONCall(aboutToSend.theData, successFunc, failureFunc);
             } else {
               var theData = aboutToSend.theData;
               if (liftAjax.lift_uriSuffix) {
                 theData += '&' + liftAjax.lift_uriSuffix;
                 liftAjax.lift_uriSuffix = undefined;
               }
               liftAjax.lift_actualAjaxCall(theData, successFunc, failureFunc);
             }
            }
         }

         liftAjax.lift_testAndShowAjax();
         setTimeout("liftAjax.lift_doAjaxCycle();", 200);
       },

       addPageName: function(url) {
         return """ + {
    if (LiftRules.enableLiftGC) {
      "url.replace('" + LiftRules.ajaxPath + "', '" + LiftRules.ajaxPath + "/'+lift_page);"
    } else {
      "url;"
    }
  } + """
    },

    lift_actualAjaxCall: function(data, onSuccess, onFailure) {
      """ +
          LiftRules.jsArtifacts.ajax(AjaxInfo(JE.JsRaw("data"),
            "POST",
            LiftRules.ajaxPostTimeout,
            false, "script",
            Full("onSuccess"), Full("onFailure"))) +
          """
        },

        lift_actualJSONCall: function(data, onSuccess, onFailure) {
          """ +
          LiftRules.jsArtifacts.ajax(AjaxInfo(JE.JsRaw("data"),
            "POST",
            LiftRules.ajaxPostTimeout,
            false, "json",
            Full("onSuccess"), Full("onFailure"))) +
          """
              }
            };

            window.liftUtils = {
              lift_blurIfReturn: function(e) {
                var code;
                if (!e) var e = window.event;
                if (e.keyCode) code = e.keyCode;
                else if (e.which) code = e.which;

                var targ;

                if (e.target) targ = e.target;
                else if (e.srcElement) targ = e.srcElement;
                if (targ.nodeType == 3) // defeat Safari bug
                  targ = targ.parentNode;
                if (code == 13) {targ.blur(); return false;} else {return true;};
              }
            };


          })();
          """ + LiftRules.jsArtifacts.onLoad(new JsCmd() {def toJsCmd = "liftAjax.lift_doAjaxCycle()"}).toJsCmd)


  /**
   * Renders the default JS comet script
   */
  def cometScript = JsCmds.Run("""
  (function() {
    window.liftComet = {
      lift_handlerSuccessFunc: function() {
        setTimeout("liftComet.lift_cometEntry();",100);
      },

      lift_unlistWatch : function(watchId) {
        var ret = [];
        for (item in lift_toWatch) {
          if (item !== watchId) {
            ret.push(item);
          }
        }
        lift_toWatch = ret;
      },

      lift_handlerFailureFunc: function() {
        setTimeout("liftComet.lift_cometEntry();",""" + LiftRules.cometFailureRetryTimeout + """);
      },

      lift_cometEntry: function() {
        var isEmpty = function(){for (var i in lift_toWatch) {return false} return true}();
        if (!isEmpty) {
          liftAjax.lift_uriSuffix = undefined;
        """ +
          LiftRules.jsArtifacts.comet(AjaxInfo(JE.JsRaw("lift_toWatch"),
            "GET",
            LiftRules.cometGetTimeout,
            false,
            "script",
            Full("liftComet.lift_handlerSuccessFunc"),
            Full("liftComet.lift_handlerFailureFunc"))) +
          """
              }
            }
          }})();
          """ +

          LiftRules.jsArtifacts.onLoad(new JsCmd() {
            def toJsCmd = "liftComet.lift_handlerSuccessFunc()"
          }).toJsCmd)
}

}
}
}
