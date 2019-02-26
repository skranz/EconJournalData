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
