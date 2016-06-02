/*
	APP.D
	-----
	Copyright (c) 2015-2016 eBay Software Foundation
	Written by Andrew Trotman
	Licensed under the 3-clause BSD license (see here:https://en.wikipedia.org/wiki/BSD_licenses)
*/

import core.stdc.stdlib;
import std.file;
import std.stdio;
import std.string;
import demangle;
import core.runtime;
import std.conv;
import std.algorithm;
import std.exception;
import std.demangle;

private File outstream; //Where we write the output

/*
	CLASS FUNCTION_EDGE
	-------------------
	There's one of these objects for each function in program being profiled.
*/
private class FunctionEdge
{
public:
    const char[] name; // the demangled name of the function
    const char[] mangled_name; // the mangled name
    ulong calls; // number of times the function is called

public:
    /*
		THIS()
		------
		Constructor
	*/
    this(const(char)[] mangled_name, ulong calls)
    {
        this.mangled_name = mangled_name;

		const (char) [] demangled_name;
        demangled_name = demangle.demangle(mangled_name);
		if (demangled_name[0] == '_')								// in the unlikely event that we fail to demangle, fall back to the phobos demangler
			demangled_name = std.demangle.demangle(cast(string)mangled_name);
		this.name = demangled_name;

        this.calls = calls;
    }
}

/*
	CLASS FUNCTION_NODE
	-------------------

*/
private class FunctionNode
{
public:
    FunctionEdge[string] called_by;
    FunctionEdge[string] calls_to;
    const char[] name;
    const char[] mangled_name;
    ulong number_of_calls;
    ulong function_and_descendant_time; // in cycles
    ulong function_time; // in cycles

private:
    /*
		PERCENT()
		---------
		Compute top/bottom to 2 decimal places
	*/
    double percent(double top, double bottom)
    {
        return cast(double)(cast(size_t)((top / bottom * 100_00.0))) / 100.0;
    }

    /*
		TO_US()
		-------
		Convert from ticks to micro-seconds
	*/
    size_t to_us(double ticks, double ticks_per_second)
    {
        return cast(size_t)(ticks / ticks_per_second * 1000 * 1000);
    }

public:
    /*
		THIS()
		------
	*/
    this(char[] mangled_name, ulong calls, ulong function_and_descendant_time,
        ulong function_time, FunctionEdge[string] called_by, FunctionEdge[string] calls_to)
    {
        this.mangled_name = mangled_name;

		const (char) [] demangled_name;
        demangled_name = demangle.demangle(mangled_name);
		if (demangled_name[0] == '_')								// in the unlikely event that we fail to demangle, fall back to the phobos demangler
			demangled_name = std.demangle.demangle(cast(string)mangled_name);
		this.name = demangled_name;

        this.number_of_calls = calls;
        this.function_and_descendant_time = function_and_descendant_time;
        this.function_time = function_time;
        this.called_by = called_by;
        this.calls_to = calls_to;
    }

    /*
		DUMP_JAVASCRIPT()
		-----------------
	*/
    static void dump_javascript()
    {
        /*
	This code is from Stuart Langridge, see: http://www.kryogenix.org/code/browser/sorttable/
	Its the JavaScript that sorts table columns when the user clicks on the column header
*/
        outstream.write("
	/*
	  SortTable
	  version 2
	  7th April 2007
	  Stuart Langridge, http://www.kryogenix.org/code/browser/sorttable/

	  Instructions:
	  Download this file
	  Add <script src=\"sorttable.js\">\\<\\/script> to your HTML
	  Add class=\"sortable\" to any table you'd like to make sortable
	  Click on the headers to sort

	  Thanks to many, many people for contributions and suggestions.
	  Licenced as X11: http://www.kryogenix.org/code/browser/licence.html
	  This basically means: do what you want with it.
	*/


	var stIsIE = /*@cc_on!@*/false;

	sorttable = {
	  init: function() {
		 // quit if this function has already been called
		 if (arguments.callee.done) return;
		 // flag this function so we don't do the same thing twice
		 arguments.callee.done = true;
		 // kill the timer
		 if (_timer) clearInterval(_timer);

		 if (!document.createElement || !document.getElementsByTagName) return;

		 sorttable.DATE_RE = /^(\\d\\d?)[\\/\\.-](\\d\\d?)[\\/\\.-]((\\d\\d)?\\d\\d)$/;

		 forEach(document.getElementsByTagName('table'), function(table) {
			if (table.className.search(/\\bsortable\\b/) != -1) {
			  sorttable.makeSortable(table);
			}
		 });

	  },

	  makeSortable: function(table) {
		 if (table.getElementsByTagName('thead').length == 0) {
			// table doesn't have a tHead. Since it should have, create one and
			// put the first table row in it.
			the = document.createElement('thead');
			the.appendChild(table.rows[0]);
			table.insertBefore(the,table.firstChild);
		 }
		 // Safari doesn't support table.tHead, sigh
		 if (table.tHead == null) table.tHead = table.getElementsByTagName('thead')[0];

		 if (table.tHead.rows.length != 1) return; // can't cope with two header rows

		 // Sorttable v1 put rows with a class of \"sortbottom\" at the bottom (as
		 // \"total\" rows, for example). This is B&R, since what you're supposed
		 // to do is put them in a tfoot. So, if there are sortbottom rows,
		 // for backwards compatibility, move them to tfoot (creating it if needed).
		 sortbottomrows = [];
		 for (var i=0; i<table.rows.length; i++) {
			if (table.rows[i].className.search(/\\bsortbottom\\b/) != -1) {
			  sortbottomrows[sortbottomrows.length] = table.rows[i];
			}
		 }
		 if (sortbottomrows) {
			if (table.tFoot == null) {
			  // table doesn't have a tfoot. Create one.
			  tfo = document.createElement('tfoot');
			  table.appendChild(tfo);
			}
			for (var i=0; i<sortbottomrows.length; i++) {
			  tfo.appendChild(sortbottomrows[i]);
			}
			delete sortbottomrows;
		 }

		 // work through each column and calculate its type
		 headrow = table.tHead.rows[0].cells;
		 for (var i=0; i<headrow.length; i++) {
			// manually override the type with a sorttable_type attribute
			if (!headrow[i].className.match(/\\bsorttable_nosort\\b/)) { // skip this col
			  mtch = headrow[i].className.match(/\\bsorttable_([a-z0-9]+)\\b/);
			  if (mtch) { override = mtch[1]; }
				if (mtch && typeof sorttable[\"sort_\"+override] == 'function') {
				  headrow[i].sorttable_sortfunction = sorttable[\"sort_\"+override];
				} else {
				  headrow[i].sorttable_sortfunction = sorttable.guessType(table,i);
				}
				// make it clickable to sort
				headrow[i].sorttable_columnindex = i;
				headrow[i].sorttable_tbody = table.tBodies[0];
				dean_addEvent(headrow[i],\"click\", sorttable.innerSortFunction = function(e) {

				 if (this.className.search(/\\bsorttable_sorted\\b/) != -1) {
					// if we're already sorted by this column, just
					// reverse the table, which is quicker
					sorttable.reverse(this.sorttable_tbody);
					this.className = this.className.replace('sorttable_sorted',
																		 'sorttable_sorted_reverse');
					this.removeChild(document.getElementById('sorttable_sortfwdind'));
					sortrevind = document.createElement('span');
					sortrevind.id = \"sorttable_sortrevind\";
					sortrevind.innerHTML = stIsIE ? '&nbsp<font face=\"webdings\">5</font>' : '&nbsp;&#x25B4;';
					this.appendChild(sortrevind);
					return;
				 }
				 if (this.className.search(/\\bsorttable_sorted_reverse\\b/) != -1) {
					// if we're already sorted by this column in reverse, just
					// re-reverse the table, which is quicker
					sorttable.reverse(this.sorttable_tbody);
					this.className = this.className.replace('sorttable_sorted_reverse',
																		 'sorttable_sorted');
					this.removeChild(document.getElementById('sorttable_sortrevind'));
					sortfwdind = document.createElement('span');
					sortfwdind.id = \"sorttable_sortfwdind\";
					sortfwdind.innerHTML = stIsIE ? '&nbsp<font face=\"webdings\">6</font>' : '&nbsp;&#x25BE;';
					this.appendChild(sortfwdind);
					return;
				 }

				 // remove sorttable_sorted classes
				 theadrow = this.parentNode;
				 forEach(theadrow.childNodes, function(cell) {
					if (cell.nodeType == 1) { // an element
					  cell.className = cell.className.replace('sorttable_sorted_reverse','');
					  cell.className = cell.className.replace('sorttable_sorted','');
					}
				 });
				 sortfwdind = document.getElementById('sorttable_sortfwdind');
				 if (sortfwdind) { sortfwdind.parentNode.removeChild(sortfwdind); }
				 sortrevind = document.getElementById('sorttable_sortrevind');
				 if (sortrevind) { sortrevind.parentNode.removeChild(sortrevind); }

				 this.className += ' sorttable_sorted';
				 sortfwdind = document.createElement('span');
				 sortfwdind.id = \"sorttable_sortfwdind\";
				 sortfwdind.innerHTML = stIsIE ? '&nbsp<font face=\"webdings\">6</font>' : '&nbsp;&#x25BE;';
				 this.appendChild(sortfwdind);

				  // build an array to sort. This is a Schwartzian transform thing,
				  // i.e., we \"decorate\" each row with the actual sort key,
				  // sort based on the sort keys, and then put the rows back in order
				  // which is a lot faster because you only do getInnerText once per row
				  row_array = [];
				  col = this.sorttable_columnindex;
				  rows = this.sorttable_tbody.rows;
				  for (var j=0; j<rows.length; j++) {
					 row_array[row_array.length] = [sorttable.getInnerText(rows[j].cells[col]), rows[j]];
				  }
				  /* If you want a stable sort, uncomment the following line */
				  //sorttable.shaker_sort(row_array, this.sorttable_sortfunction);
				  /* and comment out this one */
				  row_array.sort(this.sorttable_sortfunction);

				  tb = this.sorttable_tbody;
				  for (var j=0; j<row_array.length; j++) {
					 tb.appendChild(row_array[j][1]);
				  }

				  delete row_array;
				});
			 }
		 }
	  },

	  guessType: function(table, column) {
		 // guess the type of a column based on its first non-blank row
		 sortfn = sorttable.sort_alpha;
		 for (var i=0; i<table.tBodies[0].rows.length; i++) {
			text = sorttable.getInnerText(table.tBodies[0].rows[i].cells[column]);
			if (text != '') {
			  if (text.match(/^-?[Â£$Â¤]?[\\d,.]+%?$/)) {
				 return sorttable.sort_numeric;
			  }
			  // check for a date: dd/mm/yyyy or dd/mm/yy
			  // can have / or . or - as separator
			  // can be mm/dd as well
			  possdate = text.match(sorttable.DATE_RE)
			  if (possdate) {
				 // looks like a date
				 first = parseInt(possdate[1]);
				 second = parseInt(possdate[2]);
				 if (first > 12) {
					// definitely dd/mm
					return sorttable.sort_ddmm;
				 } else if (second > 12) {
					return sorttable.sort_mmdd;
				 } else {
					// looks like a date, but we can't tell which, so assume
					// that it's dd/mm (English imperialism!) and keep looking
					sortfn = sorttable.sort_ddmm;
				 }
			  }
			}
		 }
		 return sortfn;
	  },

	  getInnerText: function(node) {
		 // gets the text we want to use for sorting for a cell.
		 // strips leading and trailing whitespace.
		 // this is *not* a generic getInnerText function; it's special to sorttable.
		 // for example, you can override the cell text with a customkey attribute.
		 // it also gets .value for <input> fields.

		 if (!node) return \"\";

		 hasInputs = (typeof node.getElementsByTagName == 'function') &&
						  node.getElementsByTagName('input').length;

		 if (node.getAttribute(\"sorttable_customkey\") != null) {
			return node.getAttribute(\"sorttable_customkey\");
		 }
		 else if (typeof node.textContent != 'undefined' && !hasInputs) {
			return node.textContent.replace(/^\\s+|\\s+$/g, '');
		 }
		 else if (typeof node.innerText != 'undefined' && !hasInputs) {
			return node.innerText.replace(/^\\s+|\\s+$/g, '');
		 }
		 else if (typeof node.text != 'undefined' && !hasInputs) {
			return node.text.replace(/^\\s+|\\s+$/g, '');
		 }
		 else {
			switch (node.nodeType) {
			  case 3:
				 if (node.nodeName.toLowerCase() == 'input') {
					return node.value.replace(/^\\s+|\\s+$/g, '');
				 }
			  case 4:
				 return node.nodeValue.replace(/^\\s+|\\s+$/g, '');
				 break;
			  case 1:
			  case 11:
				 var innerText = '';
				 for (var i = 0; i < node.childNodes.length; i++) {
					innerText += sorttable.getInnerText(node.childNodes[i]);
				 }
				 return innerText.replace(/^\\s+|\\s+$/g, '');
				 break;
			  default:
				 return '';
			}
		 }
	  },

	  reverse: function(tbody) {
		 // reverse the rows in a tbody
		 newrows = [];
		 for (var i=0; i<tbody.rows.length; i++) {
			newrows[newrows.length] = tbody.rows[i];
		 }
		 for (var i=newrows.length-1; i>=0; i--) {
			 tbody.appendChild(newrows[i]);
		 }
		 delete newrows;
	  },

	  /* sort functions
		  each sort function takes two parameters, a and b
		  you are comparing a[0] and b[0] */
	  sort_numeric: function(a,b) {
		 aa = parseFloat(a[0].replace(/[^0-9.-]/g,''));
		 if (isNaN(aa)) aa = 0;
		 bb = parseFloat(b[0].replace(/[^0-9.-]/g,''));
		 if (isNaN(bb)) bb = 0;
		 return aa-bb;
	  },
	  sort_alpha: function(a,b) {
		 if (a[0]==b[0]) return 0;
		 if (a[0]<b[0]) return -1;
		 return 1;
	  },
	  sort_ddmm: function(a,b) {
		 mtch = a[0].match(sorttable.DATE_RE);
		 y = mtch[3]; m = mtch[2]; d = mtch[1];
		 if (m.length == 1) m = '0'+m;
		 if (d.length == 1) d = '0'+d;
		 dt1 = y+m+d;
		 mtch = b[0].match(sorttable.DATE_RE);
		 y = mtch[3]; m = mtch[2]; d = mtch[1];
		 if (m.length == 1) m = '0'+m;
		 if (d.length == 1) d = '0'+d;
		 dt2 = y+m+d;
		 if (dt1==dt2) return 0;
		 if (dt1<dt2) return -1;
		 return 1;
	  },
	  sort_mmdd: function(a,b) {
		 mtch = a[0].match(sorttable.DATE_RE);
		 y = mtch[3]; d = mtch[2]; m = mtch[1];
		 if (m.length == 1) m = '0'+m;
		 if (d.length == 1) d = '0'+d;
		 dt1 = y+m+d;
		 mtch = b[0].match(sorttable.DATE_RE);
		 y = mtch[3]; d = mtch[2]; m = mtch[1];
		 if (m.length == 1) m = '0'+m;
		 if (d.length == 1) d = '0'+d;
		 dt2 = y+m+d;
		 if (dt1==dt2) return 0;
		 if (dt1<dt2) return -1;
		 return 1;
	  },

	  shaker_sort: function(list, comp_func) {
		 // A stable sort function to allow multi-level sorting of data
		 // see: http://en.wikipedia.org/wiki/Cocktail_sort
		 // thanks to Joseph Nahmias
		 var b = 0;
		 var t = list.length - 1;
		 var swap = true;

		 while(swap) {
			  swap = false;
			  for(var i = b; i < t; ++i) {
					if ( comp_func(list[i], list[i+1]) > 0 ) {
						 var q = list[i]; list[i] = list[i+1]; list[i+1] = q;
						 swap = true;
					}
			  } // for
			  t--;

			  if (!swap) break;

			  for(var i = t; i > b; --i) {
					if ( comp_func(list[i], list[i-1]) < 0 ) {
						 var q = list[i]; list[i] = list[i-1]; list[i-1] = q;
						 swap = true;
					}
			  } // for
			  b++;

		 } // while(swap)
	  }
	}

	/* ******************************************************************
		Supporting functions: bundled here to avoid depending on a library
		****************************************************************** */

	// Dean Edwards/Matthias Miller/John Resig

	/* for Mozilla/Opera9 */
	if (document.addEventListener) {
		 document.addEventListener(\"DOMContentLoaded\", sorttable.init, false);
	}

	/* for Internet Explorer */
	/*@cc_on @*/
	/*@if (@_win32)
		 document.write(\"<script id=__ie_onload defer src=javascript:void(0)><\\/script>\");
		 var script = document.getElementById(\"__ie_onload\");
		 script.onreadystatechange = function() {
			  if (this.readyState == \"complete\") {
					sorttable.init(); // call the onload handler
			  }
		 };
	/*@end @*/

	/* for Safari */
	if (/WebKit/i.test(navigator.userAgent)) { // sniff
		 var _timer = setInterval(function() {
			  if (/loaded|complete/.test(document.readyState)) {
					sorttable.init(); // call the onload handler
			  }
		 }, 10);
	}

	/* for other browsers */
	window.onload = sorttable.init;

	// written by Dean Edwards, 2005
	// with input from Tino Zijdel, Matthias Miller, Diego Perini

	// http://dean.edwards.name/weblog/2005/10/add-event/

	function dean_addEvent(element, type, handler) {
		if (element.addEventListener) {
			element.addEventListener(type, handler, false);
		} else {
			// assign each event handler a unique ID
			if (!handler.$$guid) handler.$$guid = dean_addEvent.guid++;
			// create a hash table of event types for the element
			if (!element.events) element.events = {};
			// create a hash table of event handlers for each element/event pair
			var handlers = element.events[type];
			if (!handlers) {
				handlers = element.events[type] = {};
				// store the existing event handler (if there is one)
				if (element[\"on\" + type]) {
					handlers[0] = element[\"on\" + type];
				}
			}
			// store the event handler in the hash table
			handlers[handler.$$guid] = handler;
			// assign a global event handler to do all the work
			element[\"on\" + type] = handleEvent;
		}
	};
	// a counter used to create unique IDs
	dean_addEvent.guid = 1;

	function removeEvent(element, type, handler) {
		if (element.removeEventListener) {
			element.removeEventListener(type, handler, false);
		} else {
			// delete the event handler from the hash table
			if (element.events && element.events[type]) {
				delete element.events[type][handler.$$guid];
			}
		}
	};

	function handleEvent(event) {
		var returnValue = true;
		// grab the event object (IE uses a global event object)
		event = event || fixEvent(((this.ownerDocument || this.document || this).parentWindow || window).event);
		// get a reference to the hash table of event handlers
		var handlers = this.events[event.type];
		// execute each event handler
		for (var i in handlers) {
			this.$$handleEvent = handlers[i];
			if (this.$$handleEvent(event) === false) {
				returnValue = false;
			}
		}
		return returnValue;
	};

	function fixEvent(event) {
		// add W3C standard event methods
		event.preventDefault = fixEvent.preventDefault;
		event.stopPropagation = fixEvent.stopPropagation;
		return event;
	};
	fixEvent.preventDefault = function() {
		this.returnValue = false;
	};
	fixEvent.stopPropagation = function() {
	  this.cancelBubble = true;
	}

	// Dean's forEach: http://dean.edwards.name/base/forEach.js
	/*
		forEach, version 1.0
		Copyright 2006, Dean Edwards
		License: http://www.opensource.org/licenses/mit-license.php
	*/

	// array-like enumeration
	if (!Array.forEach) { // mozilla already supports this
		Array.forEach = function(array, block, context) {
			for (var i = 0; i < array.length; i++) {
				block.call(context, array[i], i, array);
			}
		};
	}

	// generic enumeration
	Function.prototype.forEach = function(object, block, context) {
		for (var key in object) {
			if (typeof this.prototype[key] == \"undefined\") {
				block.call(context, object[key], key, object);
			}
		}
	};

	// character enumeration
	String.forEach = function(string, block, context) {
		Array.forEach(string.split(\"\"), function(chr, index) {
			block.call(context, chr, index, string);
		});
	};

	// globally resolve forEach enumeration
	var forEach = function(object, block, context) {
		if (object) {
			var resolve = Object; // default
			if (object instanceof Function) {
				// functions have a \"length\" property
				resolve = Function;
			} else if (object.forEach instanceof Function) {
				// the object implements a custom forEach method so use that
				object.forEach(block, context);
				return;
			} else if (typeof object == \"string\") {
				// the object is a string
				resolve = String;
			} else if (typeof object.length == \"number\") {
				// the object is array-like
				resolve = Array;
			}
			resolve.forEach(object, block, context);
		}
	};
	");
    }

    /*
		HTML_RENDER_HEADER()
		--------------------
		Dump the HTML header
	*/
    static void html_render_header()
    {
        outstream.write("<a name=A_top>");
        outstream.write("<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">");
        outstream.write(
            "<body link=#000000 vlink=#000000 alink=#000000><style>A{text-decoration:none}</style>");
        outstream.write("<center><font size=+2><i><b>Overview</b></i></font></center>");
        outstream.write("<script>");
        dump_javascript;
        outstream.write("</script>");
        outstream.write("<table class=sortable>");

        outstream.write("<tr>");
        outstream.write("<th valign=bottom align=right>Calls</th>");
        outstream.write("<th valign=bottom align=right>F time</th>");
        outstream.write("<th valign=bottom align=right>F+D time</th>");
        outstream.write("<th valign=bottom align=right>F time %</th>");
        outstream.write("<th valign=bottom align=right>F+D time %</th>");
        outstream.write("<th valign=bottom align=right>Avg F time</th>");
        outstream.write("<th valign=bottom align=left>Function</th>");
        outstream.write("</tr>");
    }

    /*
		HTML_RENDER_FOOTER()
		--------------------
		Dump the close tags at the end of the HTML file
	*/
    static void html_render_footer()
    {
        outstream.writeln("</table><hr></body>");
    }

    /*
		HTML_RENDER_SUMMARY()
		---------------------
		Dump the summary information about a given function
	*/
    void html_render_summary(double total_time, double ticks_per_second)
    {
        outstream.write("<tr>");
        outstream.write("<td align=right>", number_of_calls, "</td>");
        outstream.write("<td align=right>", to_us(function_time, ticks_per_second),
            "</td>");
        outstream.write("<td align=right>", to_us(function_and_descendant_time,
            ticks_per_second), "</td>");
        outstream.write("<td align=right>", percent(function_time, total_time), "</td>");
        outstream.write("<td align=right>",
            percent(function_and_descendant_time, total_time), "</td>");
        outstream.write("<td align=right>",
            to_us(function_time / max(number_of_calls, 1), ticks_per_second), "</td>");
        outstream.write("<td><a href=#A_", mangled_name, ">", name, "</a></td>");
        outstream.write("</tr>");
    }

    /*
		HTML_RENDER()
		-------------
		Render the data about a single function
	*/
    void html_render(double total_time, double ticks_per_second, in FunctionNode[string] nodes)
    {
        outstream.write("<a name='A_", mangled_name, "'</a><br>");
        outstream.write(
            "<table cellspacing=0 cellpadding=0><tr><td><font size=+2><i><b><a href=#A_top>&uarr;</a><center>",
            name, "</center></b></i></font></td></tr><tr><td>");
        outstream.write("
	<script type=\"text/javascript\" src=\"https://www.google.com/jsapi\"></script>

	<script type=\"text/javascript\">
		google.load(\"visualization\", \"1\", {packages:[\"corechart\"]});
		google.setOnLoadCallback(drawChart);
		function drawChart()
		{
		var data = google.visualization.arrayToDataTable([
		['Method', 'time'],
		['", name, "', ", to_us(function_and_descendant_time,
            ticks_per_second), "],
		['others',  ",
            to_us(total_time - function_and_descendant_time, ticks_per_second),
            "],
		]);

		var options =
			{
			title: '% of Focus',
			legend: 'none',
			slices:
				{
				0: { color: '#DC3912' },
				1: { color: '#3366CC' }
				}
			};

		var chart = new google.visualization.PieChart(document.getElementById('",
            mangled_name, "'));
		chart.draw(data, options);
		}
	</script>
	");

        outstream.write("
	<div name=func>
		<table id=graph cellspacing=0 cellpadding=0>
			<tr>
				<td>
					<div id=\"", mangled_name,
            "\" style=\"width: 200px; height: 200px;\"></div>
				</td>
				<td>
					<table id=stats cellspacing=2 cellpadding=2>
						<tr><th align=right>Method:</th><td> </td><td>",
            name, "</td></tr>
<!--						<tr><th align=right>Mangled:</th><td> </td><td>", mangled_name, "</td></tr> -->
						<tr><th align=right>Calls:</th><td> </td><td>",
            number_of_calls, "</td></tr>
						<tr><th align=right>Function time:</th><td bgcolor='#000000'>&nbsp;</td><td>", to_us(function_time,
            ticks_per_second), "&micro;s (", percent(function_time, total_time),
            "% of Focus)</td></tr>
						<tr><th align=right>F+D time:</th><td bgcolor=#DC3912>&nbsp;</td><td>",
            to_us(function_and_descendant_time, ticks_per_second), "&micro;s (",
            percent(function_and_descendant_time, total_time), "% of Focus)</td></tr>
					</table>
				<td>
			<tr>
		</table>
	</div>
	");

        string[] colors = [
            "#3366CC", "#DC3912", "#FF9900", "#109618", "#990099", "#3B3EAC",
            "#0099C6", "#DD4477", "#66AA00", "#B82E2E", "#316395", "#994499",
            "#22AA99", "#AAAA11", "#6633CC", "#E67300", "#8B0707", "#329262",
            "#5574A6", "#3B3EAC"
        ];

        size_t total = 0;
        foreach (current; called_by)
            total += current.calls;

        outstream.write("
	<script type=\"text/javascript\">
		google.setOnLoadCallback(drawChart);
		function drawChart()
		{
		var data = google.visualization.arrayToDataTable([
		['Method', 'Calls'],");
        foreach (current; called_by)
            outstream.write("['", current.name, "',  ", current.calls, "],");
        outstream.write("
		]);

		var options =
			{
			title: 'Callers',
			legend: 'none',
			slices:
				{");
        for (size_t col = 0; col < min(colors.length, called_by.length); col++)
            outstream.writeln("\t\t\t\t", col, ":{color:'", colors[col], "'},");
        outstream.write("\t\t\t\t}
			};
		var chart = new google.visualization.PieChart(document.getElementById('",
            mangled_name, "_by'));
		chart.draw(data, options);
		}
	</script>
	");

        outstream.write("
	<div name=func>
		<table id=graph cellspacing=0 cellpadding=0>
			<tr>
				<td>
					<div id=\"",
            mangled_name, "_by\" style=\"width: 200px; height: 200px;\"></div>
				</td>
				<td>
					<table id=graphstats cellspacing=2 cellpadding=2 class=sortable>");
        if (called_by.length == 0)
            outstream.writeln("<tr><th></th><th align=left>No Caller</th><th></th><th></th></tr>");
        else
        {
            outstream.write("<tr>");
            outstream.write("<th>Calls</th>");
            outstream.write("<th>Percent</th>");
            outstream.write("<th></th>");
            outstream.write("<th align=left>Caller</th>");
            outstream.write("</tr>");
        }
        size_t count = 0;
        foreach (current; called_by)
        {
            outstream.write("\t\t\t\t\t\t");
            outstream.write("<tr>");
            outstream.write("<td align=right>", current.calls, "</td>");
            outstream.write("<td align=right>", percent(current.calls, total), "</td>");
            outstream.write("<td bgcolor=",
                colors[count < colors.length ? count : colors.length - 1], ">&nbsp;</td>");
            outstream.write("<td align=left><a href='#A_",
                current.mangled_name, "'>", current.name, "</a></td>");
            outstream.write("</tr>");
            count++;
        }
        outstream.write("
					</table>");
        outstream.write("
				<td>
			<tr>
		</table>
	</div>
	");

        total = 0;
        foreach (current; calls_to)
            total += current.calls;

        outstream.write("
	<script type=\"text/javascript\">
		google.setOnLoadCallback(drawChart);
		function drawChart()
		{
		var data = google.visualization.arrayToDataTable([
		['Method', 'Calls'],");
        outstream.write("['This Function',  ", to_us(function_time, ticks_per_second),
            "],");
        foreach (current; calls_to)
        {
            ulong descendant_time = cast(ulong)((
                nodes[current.mangled_name].function_and_descendant_time
                / cast(double)nodes[current.mangled_name].number_of_calls) * current.calls);
            outstream.write("['", current.name, "',  ", to_us(descendant_time,
                ticks_per_second), "],");
        }
        outstream.write("
		]);

		var options =
			{
			title: 'Estimated F+D Time',
			legend: 'none',
			slices:
				{");
        outstream.write("\t\t\t\t", 0, ":{color:'#000000'},");
        for (size_t col = 0; col < min(colors.length, calls_to.length); col++)
            outstream.write("\t\t\t\t", col + 1, ":{color:'", colors[col], "'},");
        outstream.write("\t\t\t\t}
			};
		var chart = new google.visualization.PieChart(document.getElementById('",
            mangled_name, "_to'));
		chart.draw(data, options);
		}
	</script>
	");

        outstream.write("
	<div name=func>
		<table id=graph cellspacing=0 cellpadding=0>
			<tr>
				<td>
					<div id=\"",
            mangled_name, "_to\" style=\"width: 200px; height: 200px;\"></div>
				</td>
				<td>
					<table id=graphstats cellspacing=2 cellpadding=2 class=sortable>");
        outstream.write("<tr>");
        if (calls_to.length != 0)
            outstream.write("<th>Calls</th>");
        else
            outstream.write("<th></th>");
        outstream.write("<th>&ap;Time</th>");
        outstream.write("<th>&ap;Percent</th>");
        outstream.write("<th></th>");
        outstream.write("<th align=left>Descendant</th>");
        outstream.write("</tr>");
        count = 0;
        ulong descendant_time_sum = function_time;
        foreach (current; calls_to)
            descendant_time_sum += cast(ulong)((
                nodes[current.mangled_name].function_and_descendant_time
                / cast(double)nodes[current.mangled_name].number_of_calls) * current.calls);

        outstream.write("\t\t\t\t\t\t");
        outstream.write("<tr>");
        outstream.write("<td></td>");
        outstream.write("<td align=right>", to_us(function_time, ticks_per_second),
            "&micro;s</td>");
        outstream.write("<td align=right>", percent(function_time, descendant_time_sum),
            "</td>");
        outstream.write("<td bgcolor='#000000'>&nbsp;</td>");
        outstream.write("<td align=left>This Function</a></td>");
        outstream.write("</tr>");

        foreach (current; calls_to)
        {
            ulong descendant_time = cast(ulong)((
                nodes[current.mangled_name].function_and_descendant_time
                / cast(double)nodes[current.mangled_name].number_of_calls) * current.calls);

            outstream.write("\t\t\t\t\t\t");
            outstream.write("<tr>");
            outstream.write("<td align=right>", current.calls, "</td>");
            outstream.write("<td align=right>", to_us(descendant_time,
                ticks_per_second), "&micro;s</td>");
            outstream.write("<td align=right>", percent(descendant_time,
                descendant_time_sum), "</td>");
            outstream.write("<td bgcolor=",
                colors[count < colors.length ? count : colors.length - 1], ">&nbsp;</td>");
            outstream.write("<td align=left><a href='#A_",
                current.mangled_name, "'>", current.name, "</a></td>");
            outstream.write("</tr>");
            count++;
        }
        outstream.write("
					</table>");
        outstream.write("
				<td>
			<tr>
		</table>
	</div>
	");

        outstream.write("</td></tr></table>");
        outstream.write("<hr>");
    }
}

/*
	BUFFER_TO_LIST()
	----------------
	Convert from a linear buffer into an array of lines.  This is used along with read() to convert a file into
	an array where each element in the array is a line from the file.
*/
private char[][] buffer_to_list(void[] file)
{
    char* start = cast(char*) file.ptr;
    char* end = cast(char*) file.ptr + file.length;
    char[][] answer;
    uint next = 0;

    for (auto ch = start; ch < end; ch++)
        if (*ch == '\n' || *ch == '\r')						// we need to check for '\n' and '\r' because they are defined differently on Mac / Linux / Windows (so one or the other with match)
        {
            if (next >= answer.length){
                answer.length += 100_000;
            }
            answer[next++] = start[0 .. ch - start];
            start = ch + 1;
        }
    return answer;
}

/*
	DRAW_PROFILE()
	--------------
*/
private int draw_profile()
{
    bool caller = true;
    FunctionNode[string] nodes;
    FunctionEdge[string] caller_graph;
    FunctionEdge[string] called_graph;
    ulong ticks_per_second;
    ulong function_times;
    ulong function_and_descendant;
    ulong function_only;
    char[] function_name;
    auto filename = "trace.log";
    void[] file;

    try
    {
        file = read(filename);
    }
    catch (Exception ex)
    {
        writeln("Cannot open input file trace.log");
        exit(0);
    }

    if (file.length == 0)
    {
        writeln("trace.log is empty");
        exit(0);
    }

    auto lines = buffer_to_list(file);

    try
    {
        outstream = File("trace.html", "wb");
    }
    catch (ErrnoException ex)
    {
        writeln("Cannot open output file");
        exit(0);
    }

    // parse the trace.log file.
    for (auto line = 0; line < lines.length; line++)
    {
        if (lines[line].length == 0)
        {
            continue; // Ignore blank lines
        }
        else if (lines[line][0] == '=') // Seperator between call graph and summary data
        {
            auto number = indexOfAny(lines[line], "1234567890");
            if (number < 0)
            {
                writeln(
                    "Corrupt trace.log (can't compute ticks per second), please re-profile and try again");
                exit(0);
            }
            auto space = indexOf(lines[line][number .. $], ' ') + number;
            ticks_per_second = to!ulong(lines[line][number .. space]);
            break;
        }
        else if (lines[line][0] == '-') //Seperator between each function call graph
        {
            caller = true;
            if (function_name.length != 0)
                nodes[text(function_name)] = new FunctionNode(function_name,
                    function_times, function_and_descendant, function_only,
                    caller_graph, called_graph);
            caller_graph = null;
            called_graph = null;
        }
		else if (lines[line][0] == '\t')
        {
            // A function either calling or called by this function
			/*
				We can't assume a name starts with an '_' because it might be an extern "C" which hasn't been mangled.
				We also can't assume the character encodin of what ever language that is so we look for the last tab
				and asusme the identifier starts on the next character.
			*/
//            auto pos = indexOfAny(lines[line], "_");
            auto pos = lastIndexOf(lines[line], '\t') + 1;
            auto start_pos = indexOfAny(lines[line], "1234567890");
            if (start_pos < 0 || pos < 0 || pos < start_pos)
            {
                writeln("Corrupt trace.log (call count is non-numeric), please re-profile and try again");
                exit(0);
            }
            immutable times = to!ulong(lines[line][start_pos .. pos - 1]);
            auto name = lines[line][pos .. $];
            if (caller)
            {
                caller_graph[text(name)] = new FunctionEdge(name, times);
            }
            else
            {
                called_graph[text(name)] = new FunctionEdge(name, times);
            }
        }
		/*
			In the case of a call to a non-D function, the identifier might not start with an '_' (e.g. extern "C").  But, we can't know
			how those identifiers are stored so we can't assume an encoding - and hence we must assume that what ever we have is correct.
		*/
//      else if (indexOf("_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ", lines[line][0]) >= 0) //The name of the function were're currently examining the call graph for (seperates callers from called)
        else //The name of the function were're currently examining the call graph for (seperates callers from called)
        {
            auto start_tab = indexOf(lines[line], '\t');
            auto middle_tab = indexOf(lines[line][start_tab + 1 .. $], '\t') + start_tab + 1;
            auto last_tab = indexOf(lines[line][middle_tab + 1 .. $], '\t') + middle_tab + 1;
            function_name = lines[line][0 .. start_tab];
            function_times = to!ulong(lines[line][start_tab + 1 .. middle_tab]);
            function_and_descendant = to!ulong(lines[line][middle_tab + 1 .. last_tab]);
            function_only = to!ulong(lines[line][last_tab + 1 .. $]);
            caller = false;
        }
    }
    if (function_name.length != 0)
    {
        nodes[text(function_name)] = new FunctionNode(function_name,
            function_times, function_and_descendant, function_only, caller_graph, called_graph);
    }

    auto dmain = nodes["_Dmain"];

    if (dmain is null)
    {
        writeln("Corrupt trace.log (there's no entry for _Dmain), please re-profile and try again");
        exit(0);
    }

    auto total_time = dmain.function_and_descendant_time;

    FunctionNode.html_render_header();
    foreach (current; nodes){
        current.html_render_summary(total_time, ticks_per_second);
    }
    FunctionNode.html_render_footer();

    foreach (current; nodes){
        current.html_render(total_time, ticks_per_second, nodes);
    }

    return 0;
}

/*
	MAIN()
	------
*/
int main()
{
	//In the off chance that this program is compiled to use the profiler
	//we'll change the name of the output to avoid destruction
    trace_setlogfilename("trace.log.d_profile_viewer");
    trace_setdeffilename("trace.def.d_profile_viewer");
    return draw_profile();
}
