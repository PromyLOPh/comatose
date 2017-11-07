$(document).ready (function () {
	var protocache = {};

	function extract (n) {
		var e = protocache[n.attr ('id')];
		if (e) {
			return e;
		}
		e = {
			'name': n.find ('.name').first ().text (),
			'rank': parseFloat (n.data ('rank')),
			}
		var year = n.find ('dl dd.ref * .year').first ();
		if (year.length == 1) {
			e['year'] = parseInt (year.text ());
		} else {
			e['year'] = 0;
		}
		e['features'] = n.find ('.features li').map (function (i, e) {
			return $(e).data ('id');
		}).get ();

		protocache[n.attr ('id')] = e;
		return e;
	}

	function sortproto (by) {
		var protolist = $('#protocols');
		var items = protolist.children ('.protocol');
		items.detach ().sort (function (nodeA, nodeB) {
			var a = extract ($(nodeA))[by];
			var b = extract ($(nodeB))[by];
			if (by == 'rank' || by == 'year') {
				return a > b ? 1 : (a < b ? -1 : 0);
			} else {
				/* ignore non-letter chars */
				a = a.replace (/\W+/, '');
				b = b.replace (/\W+/, '');
				return a.localeCompare (b);
			}
		});
		protolist.append (items);
	}

	function hasfeatures (item, features) {
		var itemfeatures = item['features'];
		for (i = 0; i < features.length; i++) {
			if (itemfeatures.indexOf (features[i]) == -1) {
				return false;
			}
		}
		return true;
	}

	function filterproto () {
		var sel = $('#filter-feature option');
		var features = [], search = '';
		for (i = 0; i < sel.length; i++) {
			if ($(sel[i]).is (':checked')) {
				let val = $(sel[i]).val ();
				if (val.startsWith ('tag:')) {
					features.push (val.substr (4));
				} else {
					search = val;
				}
			}
		}
		console.log (features);

		search = search.toLowerCase ()
		var items = $('#protocols .protocol');
		for (var i = 0; i < items.length; i++) {
			var domobj = $(items[i]);
			var e = extract (domobj);
			if (e['name'].toLocaleLowerCase ().indexOf (search) >= 0 &&
					(features.length == 0 || hasfeatures (e, features))) {
				domobj.show ();
			} else {
				domobj.hide ();
			}
		}
	}

	function selectedfeatures () {
		return features;
	}

	$('#sort').change (function () {
		sortproto ($(this).val ());
	});
	$('#filter-feature').change (function () {
		filterproto ();
	});
	sortproto ($('#sort').val ());
	filterproto ();

	/* enable popovers */
	$('[data-toggle="popover"]').popover({template: '<div class="popover" role="tooltip"><div class="arrow"></div><h3 class="popover-header"></h3><textarea readonly rows="5" class="popover-body"></textarea></div>'});

	$('select').selectize({create: true});
});

