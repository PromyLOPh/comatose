/*
 * jQuery throttle / debounce - v1.1 - 3/7/2010
 * http://benalman.com/projects/jquery-throttle-debounce-plugin/
 * 
 * Copyright (c) 2010 "Cowboy" Ben Alman
 * Dual licensed under the MIT and GPL licenses.
 * http://benalman.com/about/license/
 */
(function(b,c){var $=b.jQuery||b.Cowboy||(b.Cowboy={}),a;$.throttle=a=function(e,f,j,i){var h,d=0;if(typeof f!=="boolean"){i=j;j=f;f=c}function g(){var o=this,m=+new Date()-d,n=arguments;function l(){d=+new Date();j.apply(o,n)}function k(){h=c}if(i&&!h){l()}h&&clearTimeout(h);if(i===c&&m>e){l()}else{if(f!==true){h=setTimeout(i?k:l,i===c?e-m:e)}}}if($.guid){g.guid=j.guid=j.guid||$.guid++}return g};$.debounce=function(d,e,f){return f===c?a(d,e,false):a(d,f,e!==false)}})(this);

$(document).ready (function () {
	function sortproto (by) {
		var protolist = $('#protocols');
		var items = protolist.children ('.protocol');
		items.detach ().sort (function (nodeA, nodeB) {
			var a = $(nodeA).data (by);
			var b = $(nodeB).data (by);
			if (by == 'rank') {
				a = parseFloat (a);
				b = parseFloat (b);
				return a > b ? 1 : (a < b ? -1 : 0);
			} else {
				return a.localeCompare (b);
			}
		});
		protolist.append (items);
	}
	function filterproto (search) {
		search = search.toLowerCase ()
		var items = $('#protocols .protocol');
		for (var i = 0; i < items.length; i++) {
			var e = $(items[i]);
			var val = e.data ('name') + ' ' + e.data ('longname') + ' ' + e.data ('author');
			if (val.toLowerCase ().indexOf (search) >= 0) {
				e.show ();
			} else {
				e.hide ();
			}
		}
	}
	$('#sort').change (function () {
		sortproto ($(this).val ());
	});
	$('#filter').keyup ($.debounce (100, function () {
		filterproto ($(this).val ());
	}));
	$('#protosort').show ();
	sortproto ($('#sort').val ());
	filterproto ($('#filter').val ());
});
