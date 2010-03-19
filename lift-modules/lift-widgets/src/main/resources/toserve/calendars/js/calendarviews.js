( function() {

	// commen functions, shared by most of the views
	function makeItem(parent, item, indent, isRTL) {

			size = item.end - item.start;
			cssClass = "calendarItem";
			
			if (item.cssClass) {
				cssClass = item.cssClass;
			}

			if (size > 1) {
				parent.append("<div id='" + item.id + "' title='"
						+ item.description + "' indent='" + indent
						+ "' class='" + cssClass
						+ "' onclick='itemClick(this, \"" + item.id
						+ "\")'><div class='" + cssClass + "Head'>"
						+ item.startTime + "</div><div class='" + cssClass
						+ "Body'>" + item.subject + "</div></div>");
			} else {
				parent.append("<div id='" + item.id + "' title='"
						+ item.description + "' indent='" + indent
						+ "' class='" + cssClass
						+ "' onclick='itemClick(this, \"" + item.id
						+ "\")'><div class='" + cssClass + "Head'>"
						+ item.startTime + " " + item.subject
						+ "</div><div class='" + cssClass
						+ "Body'></div></div>");
			}
			
			child = $("#" + item.id);
			if (item.description) {
				child.tooltip( {
					track :true,
					delay :0,
					showURL :false
				});
			}

			width = child.outerWidth();

			firstIndent = 0;
			for (k = 0; k < 8
					&& ($(parent.parent().children()[k]).attr("id") != $(
							parent).attr("id")); k++) {
				firstIndent += $(parent.parent().children()[k])
						.outerWidth();
			}

			if (isRTL) {
				child.css( {
					'right' :firstIndent + indent,
					'width' :width - indent
				});
			} else {
				child.css( {
					'left' :firstIndent + indent,
					'width' :width - indent
				});
			}

			child.css("position", "absolute");

			h = parent.outerHeight() * size;
			child.css("height", h);

			h1 = $(child.children()[0]).outerHeight();
			$(child.children()[1]).height(h - h1 - 2);

	};

	function makeIndent(items, index, indentCondition) {
		indent = 0;

		if (index > 0) {
			for (k = index - 1; k >= 0; k = k - 1) {
				if (indentCondition(items[index],items[k])) {
					indent = parseInt($("#" + items[k].id).attr(
							"indent")) + 40;
					break;
				}
			}
		}
		return indent;
	}

	function makeAllItems(items,selectParentFun, indentCondition) {
		dir = $("html").attr("dir");
		isRTL = false;
		if (dir) {
			isRTL = dir.toLowerCase() == "rtl";
		}

		for (i = 0; i < items.length; i++) {
			makeItem(selectParentFun(),items[i], makeIndent(items, i, indentCondition), isRTL);
		}
	}

	// The base view, has the implementation that each of 
	// the views share. 
	function baseView(){
		var obj = {};
		
		obj.removeAllItems = function(items) {
			jQuery.each(items, function() {
				$("#" + this.id).remove();
			});
		};

		obj.getItemById = function(items, itemId) {
			var res = jQuery.grep(items, function(item) {
				return item.id === itemId;
			});
			return (res.length > 0) ? res[0] : null;
		};
		
		obj.buildView = function(newItems){ }; // for each view to override
		
		obj.removeItem = function(id, items) {
			newItems = jQuery.grep(items, function(item) {
				return item.id != id;
			});
			obj.removeAllItems(items);
			obj.buildView(newItems);
		};
		
		return obj;
	};
	
	// create the standard views 
	var calendarMonthView = baseView(),
		calendarWeekView = baseView(),
		calendarDayView = baseView();
	
	// add a little view specific stuff to each view
	calendarMonthView.buildView = function(items){
		var buildItem = function(item, parent) {
			parent.append('<div title="' + item.description + '" class="calendarItem" id="' + item.id + '"><a href="#"><span>' + 
							item.startTime + item.subject + '</span></a></div>');
		};
		
		if (!items) { items = calendars.items; }
		
		for (i = 0; i < items.length; i++) {
			buildItem(items[i], $("#month_"+items[i].month+"_day_"+items[i].dayOfMonth));
		}
	};
	calendarMonthView.buildMonthViewCalendars = function(items){ calendarMonthView.buildView(items); };
	
	calendarWeekView.buildView = function(items){
		var makeIndentCondition = function(item1, item2) {
				return (item1.start >= item2.start
						&& item1.start < item2.end
						&& (item1.weekDay == item2.weekDay));
			},
			selectParentFunction = function(){
				return $("#wkhidx_" + items[i].weekDay + "_" + items[i].start);
			};

		if (!items) { items = calendars.items; } 
		
		makeAllItems(items, selectParentFunction, makeIndentCondition);
	};
	calendarWeekView.buildWeekViewCalendars = function(items){ calendarWeekView.buildView(items);};
	
	calendarDayView.buildView = function(items){
		var makeIndentCondition = function(item1, item2) {
				return (item1.start >= item2.start 
						&& item1.start < item2.end);
			},
			selectParentFun = function(){
				return $("#didx_" + items[i].start);
			};

		if (!items) { items = calendars.items; }
	
		makeAllItems(items,selectParentFun, makeIndentCondition);
	};
	calendarDayView.buildDayViewCalendars = function(items){ calendarDayView.buildView(items);};
	
	// now, hook it up to the window object for everyone to use
	window.CalendarMonthView = calendarMonthView;
	window.CalendarDayView = calendarDayView;
	window.CalendarWeekView = calendarWeekView;
	
})();
