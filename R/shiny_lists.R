custom.list.panel.ui = function(ids = app$list.ids, app=NULL,glob=app$glob) {
  restore.point("custom.list.panel.ui")
  
  html = NULL
  if (!is.null(ids)) {
    html = HTML(article.list.html.from.ids(ids, app=app))
  }
  
  tabPanel("Your List",
    p("You can add search results to your custom list of articles. Use drag-and-drop to reorder the list."),
    if (!is.null(glob$userid)) {
      simpleButton("saveCustomListBtn","Save", class.add="btn-xs")
    },
    downloadButton("customListDownloadBtn","Download HTML",class = "btn-xs"),
    hr(),
    tags$ol(id="customListSortable", html),
    tags$script(HTML("customListSortableCreate();"))
  )
}

init.list.handlers = function() {
  classEventHandler("articleAddBtn",event = "click",article.add.click)
  eventHandler("customListChange",fun = custom.list.change.event)
  buttonHandler("saveCustomListBtn", save.custom.list)
  
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

save.custom.list = function(...,app=getApp()) {
  restore.point("save.custom.list")
  
  userid = app$glob$userid
  if (is.null(userid)) {
    cat("\nNo userid. Cannot save custom list.")
    return()
  }
  cdb = get.custom.db()
  entry = list(userid=userid, listid=userid,title="", descr="", ids=paste0(app$list.ids, collapse=","), public=FALSE)
  dbInsert(cdb, "lists", entry, mode="replace")
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
  html = shiny.articles.html(art,add.btn=FALSE, del.btn=TRUE)
  callJS("addCustomListElement",html)
  showNotification(paste0("Add as article #", NROW(app$list.ids),"."), duration = 2, closeButton = FALSE)
  
  return()
  app$list.html = c(app$list.html, html)
  update.list.html()
}

article.list.html.from.ids = function(ids,dat=app$glob$dat, app=getApp()) {
  restore.point("article.list.html.from.ids")
  rows = match(ids,dat$id)
  df = dat[rows,]
  shiny.articles.html(df, add.btn=FALSE, del.btn=TRUE,app = app)
}

set.custom.list = function(ids=app$list.ids, app=getApp()) {
  restore.point("update.list.html")

  rows = match(ids)
  df = app$glob$dat[rows,]
  html = shiny.articles.html(df, add.btn=FALSE, del.btn=TRUE)
  callJS("addCustomListElement",html)
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

load.custom.list = function(listid=NULL, cdb=get.custom.db()) {
  if (is.null(listid)) return(NULL)
  res = dbGet(cdb,"lists", list(listid=listid))
  res = as.list(res)
  res$ids = strsplit(res$ids,",",fixed = TRUE)[[1]]
  res
}
