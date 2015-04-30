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
	$('#sort').change (function () {
		sortproto ($(this).val ());
	});
	$('#filter').keyup (function () {
		var search = $(this).val ().toLowerCase ();
		var items = $('#protocols .protocol');
		for (var i = 0; i < items.length; i++) {
			var e = $(items[i]);
			if (e.data ('name').toLowerCase ().indexOf (search) >= 0) {
				e.show ();
			} else {
				e.hide ();
			}
		}
	});
	$('#protosort').show ();
	sortproto ('name');
});
