/* 
 * Copyright 2008 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); 
 * you may not use this file except in compliance with the License. 
 * You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0 
 * Unless required by applicable law or agreed to in writing, software 
 * distributed under the License is distributed on an "AS IS" BASIS, 
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/**
 * Lift helpers. The YUI dependencies are: yahoo.js, dom.js, connection.js, json.js
 */
( function() {

	YAHOO.namespace("lift");

	YAHOO.lift.toggle = function(first, second) {
		this.lastToggle = 0 == this.lastToggle ? 1 : 0;
		switch (this.lastToggle) {
		case 1:
			YAHOO.util.Dom.setStyle(first, 'display', 'block');
			YAHOO.util.Dom.setStyle(second, 'display', 'none');
			break;
		case 0:
			YAHOO.util.Dom.setStyle(first, 'display', 'none');
			YAHOO.util.Dom.setStyle(second, 'display', 'block');
			break;
		}
	}

	/**
	 * Evaluates a JavaScript. Inspired from JQuery.
	 */
	YAHOO.lift.eval = function(text) {
		isJS = (text.getResponseHeader['Content-Type']).indexOf('/javascript') > -1;
		isJson = (text.getResponseHeader['Content-Type']).indexOf('/json') > -1;
		content = text.responseText;
		result = content;
		if (isJS) {
			head = document.getElementsByTagName("head")[0]
					|| document.documentElement;
			script = document.createElement("script");
			script.type = "text/javascript";
			if (script.text == undefined) {
				script.appendChild(document.createTextNode(content));
			} else {
				script.text = content;
			}
			head.appendChild(script);
			head.removeChild(script);
		} else if (isJSON) {
			result = YAHOO.lang.JSON.parse(content);
		}
		return result;
	}

	/**
	 * Converts a form object denominated by formId into a JSON object
	 */
	YAHOO.lift.formToJSON = function(formId) {
		qs = YAHOO.util.Connect.setForm(formId, false);
		parts = qs.split("&");
		json = "";
		for (i = 0; i < parts.length; i++) {
			nvp = parts[i].split("=");
			json += (i > 0) ? "," : "";
			json += "\"" + decodeURIComponent(nvp[0]) + "\": \""
					+ decodeURIComponent(nvp[1]) + "\"";
		}
		return YAHOO.lang.JSON.parse("{" + json + "}");
	}

	/**
	 * Converts a a simple JSON object into a URL query string
	 */
	YAHOO.lift.simpleJsonToQS = function(obj) {
		s = "";
		for (k in obj) {
			s += "&" + k + "=" + encodeURIComponent(obj[k]);
		}
		return s.substring(1);
	}

	YAHOO.lift.buildURI = function(uri, queryString) {
		hasParam = uri.indexOf("?") > -1;
		sep = (hasParam) ? "&" : "?";
		return uri + sep + queryString
	}
})();
