$(document).ready (function () {
	$('#algo').DataTable ({
		paging: false,
		"columnDefs": [],
	});
	$('#algo tr').click (function () {
		var data = $(this).data ('proto');
		$('#popup h2').text (data.name);
		$('#popup .subtitle').text (data.longname);
		$('#popup .year').text (data.year);
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
