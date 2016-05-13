( function() {
	/*
	 * Copyright 2008-2013 WorldWide Conferencing, LLC
	 * 
	 * Licensed under the Apache License, Version 2.0 (the "License"); you may
	 * not use this file except in compliance with the License. You may obtain a
	 * copy of the License at http://www.apache.org/licenses/LICENSE-2.0 Unless
	 * required by applicable law or agreed to in writing, software distributed
	 * under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES
	 * OR CONDITIONS OF ANY KIND, either express or implied. See the License for
	 * the specific language governing permissions and limitations under the
	 * License.
	 */

	window.lift$ = {
		buildIndex : function(on, indexName) {
			on[indexName] = {};
			var index = on[indexName];
			var x = 0;
			for (x = 2; x < arguments.length; x++) {
				var arg = arguments[x];
				var keyField = arg[1];
				var table = arg[0];
				var ta = on[table];
				var y = 0;
				for (y = 0; y < ta.length; y++) {
					var item = ta[y];
					var id = item[keyField];
					if (id) {
						index[id] = item;
					}
				}
			}
		},

		foreach : function(array, func) {
			var realFunc = func;
			if (func.apply == null) {
				eval("realFunc = function(p1) {return " + func + ";}");
			}

			for ( var x = 0; x < array.length; x++) {
				realFunc(array[x]);
			}
		},

		fold : function(array, initVal, func) {
			var realFunc = func;
			if (func.apply == null) {
				eval("realFunc = function(p1, p2) {return " + func + ";}");
			}
			lift$.foreach(array, function(e) {
				initVal = realFunc(initVal, e);
			});
			return initVal;
		},

		filter : function(array, func) {
			var realFunc = func;
			if (func.apply == null) {
				eval("realFunc = function(p1) {return " + func + ";}");
			}
			var ret = [];
			lift$.foreach(array, function(e) {
				if (realFunc(e)) {
					ret.push(e);
				}
				;
			});
			return ret;
		},

		alt : function(val, func, altVal) {
			if (val == false || val == null) {
				if (altVal.apply)
					return altVal();
				return altVal;
			} else if (lift$.isArray(val)) {
				return lift$.map(val, func);
			} else {
				var realFunc = func;
				if (func.apply == null) {
					eval("realFunc = function(p1) {return " + func + ";}");
				}
				return realFunc(val)
			}
		},

		map : function(array, func) {
			var realFunc = func;
			if (func.apply == null) {
				eval("realFunc = function(p1) {return " + func + ";}");
			}
			var ret = [];
			lift$.foreach(array, function(e) {
				ret.push(realFunc(e));
			});
			return ret;
		},

		flatMap : function(array, func) {
			var realFunc = func;
			if (func.apply == null) {
				eval("realFunc = function(p1) {return " + func + ";}");
			}
			var ret = [];
			lift$.foreach(array, function(e) {
				lift$.foreach(realFunc(e), function(v) {
					ret.push(v);
				});
			});
			return ret;
		},

		sort : function(array) {
			var func = null;
			if (arguments.length > 1) {
				func = arguments[1];
			}
			var realArray = [].concat(array);
			if (func == null) {
				return realArray.sort();
			} else {
				var realFunc = func;
				if (func.apply == null) {
					eval("realFunc = function(p1, p2) {return " + func + ";}");
				}
				return realArray.sort(realFunc);
			}
		},

		take : function(array, num) {
			var ret = [];
			for ( var x = 0; x < array.length && x < num; x++) {
				ret.push(array[x]);
			}

			return ret;
		},

		magicUpdate : function(holder, field, idField, toUpdate) {
			var f = function(array, item) {
				var na = lift$.filter(array, function(i) {
					return i[idField] != item[idField];
				});
				na.push(item);
				return na;
			}

			var ta = holder[field];

			if (lift$.isArray(toUpdate)) {
				lift$.foreach(toUpdate, function(i) {
					ta = f(ta, i);
				});
				holder[field] = ta;
			} else {
				holder[field] = f(ta, toUpdate);
			}

		},

		isArray : function(obj) {
			if (obj.constructor.toString().indexOf("Array") == -1)
				return false;
			else
				return true;
		},

		randomId : function() {
			var ret = "i";

			for ( var x = 0; x < 10; x++) {
				ret = ret + (Math.floor(Math.random() * 1000));
			}

			return ret;
		},

		swappable : function(visible, hidden) {
			if (visible == null) {
				visible = "";
			}

			if (hidden == null) {
				hidden = "";
			}

			if (visible.nodeType == null) {
				visible = document.createTextNode(visible.toString());
			}

			if (visible.nodeType != 1) {
				var tmp = document.createElement("span");
				tmp.appendChild(visible);
				visible = tmp;
			}

			if (hidden.nodeType == null) {
				hidden = document.createTextNode(hidden.toString());
			}

			if (hidden.nodeType != 1) {
				var tmp = document.createElement("span");
				tmp.appendChild(hidden);
				hidden = tmp;
			}

			visible.onclick = function() {
                          jQuery(visible).hide();
                          jQuery(hidden).show();
                          if (jQuery(hidden).is("input"))
                            jQuery(hidden).focus();
                          else
                            jQuery(hidden).find("input")[0].focus();
                          return false;
 			};

			hidden.style.display = "none";

			var onblur = function() {
				jQuery(visible).show();
				jQuery(hidden).hide();
			}

			jQuery(hidden).blur(onblur);
			jQuery(hidden).children().blur(onblur);

			var df = document.createDocumentFragment();
			df.appendChild(visible);
			df.appendChild(hidden);
			return df;
		},

		formToJSON : function(formId) {
			qs = jQuery("#" + formId).serializeArray();
            ret = {};

            for (var i in qs) {
              var obj = qs[i];
              var cur = ret[obj.name];
              if (!cur) {
                ret[obj.name] = obj.value;
              } else if (lift$.isArray(cur)) {
                  cur.push(obj.value);
              } else {
                  ret[obj.name] = [cur, obj.value];
              }
            }
	    return ret;
	   }
	};

})();
