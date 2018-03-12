/*
Copyright 2015–2018 comatose contributors

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

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

	var plt = Bokeh.Plotting;

	var p3 = Bokeh.Charts.bar (yearHistData, {
		axis_number_format: "0.[00]a",
		orientation: "vertical",
	});

	plt.show (p3, document.querySelector ('#pubHistogram div'));
});

