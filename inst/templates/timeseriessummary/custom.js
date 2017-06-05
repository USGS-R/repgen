$('#expand-all').on('click', function () {
    $('.accordion-toggle.collapsed.has-data').parent().siblings('.panel-collapse').collapse('show');
});

$('#collapse-all').on('click', function () {
    $('.accordion-toggle.has-data').parent().siblings('.panel-collapse').collapse('hide');
});