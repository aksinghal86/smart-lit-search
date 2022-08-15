$(document).keyup(function(event) { 
  if ($("#query").is(":focus") && (event.key == "Enter")) { 
    $("#submit").click();
  }  
});

