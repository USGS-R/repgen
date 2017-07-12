$(document).ready(function() {
  Sortable.init();
  
  //Initialize Tables
  for(var i = 0; i < $('table').length; i++){
    $($('table')[i]).attr("data-sortable-initialized", false);
    Sortable.initTable($('table')[i]);
    $($('table')[i]).attr("data-sortable-initialized", true);
  }
});

$('#expand-all').on('click', function () {
    $('.accordion-toggle.collapsed.has-data').parent().siblings('.panel-collapse').collapse('show');
});

$('#collapse-all').on('click', function () {
    $('.accordion-toggle.has-data').parent().siblings('.panel-collapse').collapse('hide');
});

$('.accordion-toggle').on('click', function (event){
  if($(event.target).hasClass('collapsed')){
    $(event.target).removeClass('collapsed')
  } else {
    $(event.target).addClass('collapsed')
  }
})
