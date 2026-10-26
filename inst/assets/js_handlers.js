
// Force buttons to another page to start on top
Shiny.addCustomMessageHandler('scroll_top', function(msg) {
  setTimeout(function() {
    window.scrollTo({top: 0, behavior: 'instant'});
  }, 50);
});
