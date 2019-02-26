init.list.handlers = function() {
  classEventHandler("articleAddBtn",event = "click",article.add.click)
  eventHandler("customListChange",fun = custom.list.change.event)

  setDownloadHandler("customListDownloadBtn", 
    filename = "customArticles.html",
    content = function(file) {
      restore.point("customListDownload")
      app = getApp()
      ids = app$list.ids
      dat = app$glob$dat
      rows = match(ids, dat$id)
      writeLines(shiny.articles.html(dat[rows,]),file)
    },
    contentType = "text/html"
  )
    

}


custom.list.change.event = function(value, ..., app=getApp()) {
  restore.point("custom.list.change.event")
  ids = unlist(value)
  app$list.ids = ids
  
}


article.add.click = function(id=NULL,..., app=getApp()) {
  args = list(...)
  restore.point("article.add.click")
  row = as.integer(str.right.of(id, "addBtn_"))
  art = app$adf[row,]
  if (art$id %in% app$list.ids) {
    showNotification(paste0("Article is already in list."), duration = 2, type="warning", closeButton = FALSE)
    return()
  }
  
  app$list.ids = c(app$list.ids,art$id)
  delBtn = paste0(' <button id="delBtn_',art$id,'" class="btn btn-default btn-xs customListDelBtn"><i class="fa fa-remove"></i></button>')
  html = simple_articles_html(art, postfix = delBtn)
  html = paste0('<li>',html,'</li>')
  callJS("addCustomListElement",html)
  showNotification(paste0("Add as article #", NROW(app$list.ids),"."), duration = 2, closeButton = FALSE)
  
  return()
  app$list.html = c(app$list.html, html)
  update.list.html()
}

update.list.html = function(html = app$list.html, app=getApp()) {
  restore.point("update.list.html")
  return()
  js = "customListSortableCreate();"

  html = paste0('<li>',html,'</li>')
  
  ui = tagList(
    #div(id="listWithHandle", class="list-group"),
    tags$ol(id="customListSortable", HTML(html)),
    #HTML(html),
    tags$script(HTML(js))
  )
  
  setUI("listHtml", ui)
  dsetUI("listHtml", ui)
  
}
