/*
 * Copyright 2008-2016 WorldWide Conferencing, LLC
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
 * Lift helpers. The Ext-Core dependency is: ext-core.js
 */
( function() {
	/* jshint ignore:start */
  Ext.namespace('Ext.lift');

	/**
	 * Evaluates a JavaScript. Inspired by JQuery.
	 */
	Ext.lift.eval = function(text) {
		head = document.getElementsByTagName("head")[0] || document.documentElement;
		script = document.createElement("script");
		script.type = "text/javascript";
		if (script.text == undefined) {
			script.appendChild( document.createTextNode( text ) );
		} else {
			script.text = text;
		}
		head.appendChild( script );
		head.removeChild( script );
	}
	/* jshint ignore:end */
})();
