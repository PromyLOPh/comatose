$(document).ready (function () {
	function sortproto (by) {
		var protolist = $('#protocols');
		var items = protolist.children ('.protocol');
		items.detach ().sort (function (nodeA, nodeB) {
			var a = $(nodeA).data (by);
			var b = $(nodeB).data (by);
			if (a > b) {
				return 1;
			} else if (a < b) {
				return -1;
			} else {
				return 0;
			}
		});
		protolist.append (items);
	}
	$('#sort').change (function () {
		sortproto ($(this).val ());
	});
	sortproto ('name');
});
