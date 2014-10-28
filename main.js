var $line_left = $('#line_left');
$(document).scroll(function() {
  $line_left.css({display: $(this).scrollTop() > 20? "block":"none"});
});
var $line_right = $('#line_right');
$(document).scroll(function() {
  $line_right.css({display: $(this).scrollTop() > 20? "block":"none"});
});
var $line_left_long = $('#line_left_long');
$(document).scroll(function() {
  $line_left_long.css({display: $(this).scrollTop() > 20? "block":"none"});
});
var $line_right_long = $('#line_right_long');
$(document).scroll(function() {
  $line_right_long.css({display: $(this).scrollTop() > 20? "block":"none"});
}); 

// $('#discuss').tooltip();
// $('#submit').tooltip()

// toggle the More menu
// function toggleMore(){
// 	// var more = document.getElementById("more");
// 	var dropdown = document.getElementById("more-dropdown");
// 	if (dropdown.style.display == "none"){
// 		dropdown.style.display = "initial";
// 		// more.style.background = "#E9F6FB";
// 	} else {
// 		dropdown.style.display = "none";
// 		// more.style.background = "initial";
// 		// more.style.background.hover = "#E9F6FB";
// 	}
// }

// toggle the More menu
$('html').click(function() {
  $('#more-dropdown').hide(); 
});
$('nav').click(function(event){
  event.stopPropagation();
});
$('#more').click(function(event){
  $('#more-dropdown').toggle();
  // event.stopPropagation();
});