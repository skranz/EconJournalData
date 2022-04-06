
// Click on link
$(document).on("mousedown","a",function(evt) {
  // Remove list item
  var type = $(evt.currentTarget).attr('class');
  var parent = evt.currentTarget.closest("p");
  
  //var title = $(parent).find("span.title").html();
  var artid = parent.id;

	Shiny.onInputChange("linkClick",
	  {eventId: "linkClick",id: "linkClick", value: {id: artid, type: type},nonce: Math.random()}
	);
});

// Click on method tag
$(document).on("click",".mtag",function(evt) {
  // Remove list item
  var el = evt.currentTarget;
  
  //var title = $(parent).find("span.title").html();
  var tag = el.innerHTML;
  var searchterm = ($("#abs_keywords").val() + ' "'+tag+'"').trim();
  
  $("#abs_keywords").val(searchterm);
  
	Shiny.onInputChange("methodTagClick",
	  {eventId: "methodTagClick",id: "methodTagClick", value: searchterm,nonce: Math.random()}
	);
});

// Change of an edit tag input
$(document).on("change","span.edit_tags input",function(evt) {
  //alert("Edit Btn clicked!");
  
  // Remove list item
  var parent = evt.currentTarget.closest("span.edit_tags");
  
  var open_data = $(parent).find("span.open_data input:checked").val();
  var like = $(parent).find("span.like input:checked").val();
  var taken = $(parent).find("span.taken input:checked").val();
  var complex = $(parent).find("span.complex input:checked").val();
  var experiment = $(parent).find("span.experiment input:checked").val();
  
  var artid = $(parent).data("art");

	Shiny.onInputChange("editTagChange",
	  {eventId: "editTagChange",id: "editTagChange", value: {id: artid, open_data: open_data, like: like, taken: taken, complex: complex, experiment: experiment},nonce: Math.random()}
	);
});
