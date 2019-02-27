addCustomListElement = function(html) {
  $("#customListSortable").append(html);
  customListSortableCreate();
};

customListSortableChange = function(evt) {
  var IDs = [];
  $("#customListSortable").find("li p").each(function(){ IDs.push(this.id); });
	Shiny.onInputChange("customListChange",
	  {eventId: "customListChange",id: "customListChange", value: IDs,nonce: Math.random()}
	);
};

customListSortableCreate = function() {
  Sortable.create(customListSortable, {
    onSort: customListSortableChange
  });
};

$(document).on("click",".customListDelBtn",function(evt) {
  // Remove list item
  evt.currentTarget.closest("li").remove();
  // Send message to R
  customListSortableChange(evt);
});

// Change of an edit tag input
$(document).on("change","span.edit_tags input",function(evt) {
  //alert("Edit Btn clicked!");
  
  // Remove list item
  var parent = evt.currentTarget.closest("span.edit_tags");
  
  var open_data = $(parent).find("span.open_data input:checked").val();
  var like = $(parent).find("span.like input:checked").val();
  var taken = $(parent).find("span.taken input:checked").val();
  
  var artid = $(parent).data("art");

	Shiny.onInputChange("editTagChange",
	  {eventId: "editTagChange",id: "editTagChange", value: {id: artid, open_data: open_data, like: like, taken: taken},nonce: Math.random()}
	);
});
