$(document).ready (function () {
	function sortproto (by) {
		var protolist = $('#protocols');
		var items = protolist.children ('.protocol');
		items.detach ().sort (function (nodeA, nodeB) {
			var a = $(nodeA).data (by);
			var b = $(nodeB).data (by);
			if (typeof a == 'number' || typeof b == 'number') {
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
	$('#filter').keyup (function () {
		filterproto ($(this).val ());
	});
	$('#protosort').show ();
	sortproto ($('#sort').val ());
	filterproto ($('#filter').val ());
});
