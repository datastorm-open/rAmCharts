function hideInherited (cont) {
  console.log(cont);
  $('.'+cont+'-hide').hide();
  $('.'+cont+'-show').show();
  $('.'+cont+' .inherited').hide();
}

function showInherited (cont) {
  $('.'+cont+'-show').hide();
  $('.'+cont+'-hide').show();
  $('.'+cont+' .inherited').show();
}

$(document).ready(function () {
  $('.linkable').hover(function () {
    $('<div class="copy-url label label-info"><i class="icon-share icon-white" title="Link to this" /></div>').click(function () {
      var id = $(this).parents('.linkable:first').attr('id');
      var url = window.location.href.replace(/#.*/, '');
      window.prompt("Copy to clipboard: Ctrl+C, Enter", url+'#'+id);
    }).appendTo($(this));
  }, function () {
    $(this).find('.copy-url').remove();
  }).each(function () {
    //$(this).append('<a name="'+$(this).attr('name')+'"></a>');
    //$(this).append('<div class="copy-url label label-info"><i class="icon-share icon-white" title="Link to this" /></div>');
  });
  
  if (window.location.hash != '') {
    $.scrollTo(window.location.hash, 800, { offset: {top: -180}});
    $(window.location.hash).parent('tr').addClass('highlight');
    setTimeout(function () {
      $('.highlight').removeClass('highlight');
    }, 5000);
  }
  
  $('#header h2 a').click(function (e) {
    e.preventDefault();
    $.scrollTo($(this).attr('href'), 800, { offset: {top: -180}});
  });

});