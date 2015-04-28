$(document).ready (function () {
	$('#algo').DataTable ({
		paging: false,
		"columnDefs": [],
	});
	$('#algo tr').click (function () {
		var data = $(this).data ('proto');
		$('#popup h2').text (data.name);
		$('#popup .longname').text (data.longname);

		$('#popup .title').text (data.title);
		$('#popup .year').text (data.year);
		$('#popup .doi').text ('doi:' + data.doi);
		$('#popup .doi').attr ('href', 'http://doi.org/' + data.doi);
		$('#popup .scholar').attr ('href', 'http://scholar.google.com/scholar?q=' + encodeURIComponent (data.title));

		$('#popup .description').text (data.description);

		$('#popup').fadeIn ('normal');
		$('#background').fadeIn ('normal');
	});
	/* hide popup window */
	function hide () {
		$('#popup').fadeOut ('normal');
		$('#background').fadeOut ('normal');
	}
	$('#background').click (function () {
		hide ();
	});
	$(document).keyup (function (e) {
		if (e.keyCode == 27) {
			hide ();
		}
	});
});
