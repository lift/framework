/**
 * Lift helpers. The Ext-Core dependency is: ext-core.js
 */
 (function() {
 
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
})();
