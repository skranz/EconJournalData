examples.articlesApp = function() {
  library(EconJournalData)
  setwd("D:/libraries/EconJournalData/")
  restore.point.options(display.restore.point = TRUE)
  init.ejd()
  opts=get.ejd.opts()
  db=get.articles.db()
  app = articlesApp(use.lists=TRUE, log.file="log.csv", userid = "sebkranz", edit.tags=TRUE, show.likes=TRUE)
  app = articlesApp(log.file="log.csv")
  viewApp(app)
}

articlesApp = function(opts=get.ejd.opts(), db=get.articles.db(), summary.file = "articles_summary.RDS", readme.base.url = "http://econ.mathematik.uni-ulm.de/ejd/readme_files/", use.lists=FALSE, log.file=NULL, userid = NULL, edit.tags=FALSE, show.tags=edit.tags, show.likes=FALSE) {
  restore.point("articlesApp")
  library(shinyEvents)
  library(shinyBS)
  library(dplyrExtras)
  
  jis = opts$jis

  addResourcePath('EconJournalData', system.file('www', package='EconJournalData'))  

  app = eventsApp()
  glob = app$glob
  glob$readme.url = readme.base.url
  glob$use.lists = use.lists
  glob$userid = userid
  glob$edit.tags = edit.tags
  glob$show.tags = show.tags
  glob$show.likes = show.likes
  if (!is.null(log.file)) {
    glob$log.file = log.file
    file = tools::file_path_sans_ext(log.file)
    glob$login.log.file = paste0(file,"_logins.csv")
    glob$link.log.file = paste0(file,"_links.csv")
  }
  
  articles = dbGet(db,"article") %>%
    filter(has_data)
  
  dat = articles %>%
    add_code_and_data_str(file = summary.file, overwrite=FALSE)


  if (edit.tags | show.tags) {
    cdb = get.custom.db()
    atags = dbGet(cdb, "article_tags")
    atags$has_tags = TRUE
    dat = left_join(dat, atags, by="id")
    dat$has_tags[is.na(dat$has_tags)] = FALSE
  } else {
    dat$has_tags = FALSE
  }
  if (!file.exists("authors_summary.RDS")) {
    authors = make.authors.summary.rds()
  } else {
    authors = readRDS("authors_summary.RDS")
  }
  dat = left_join(dat, authors, by="id")
  
  dat$search_contents = tolower(paste0(dat$title," ",dat$title," ",dat$title," ",dat$title," ",dat$abstract, " ", dat$authors))

  
  app$opt = list(
    max_articles = 200,
    ignore_without_data = TRUE,
    journals = names(jis),
    start_date = min(dat$date,na.rm=TRUE),
    end_date = max(dat$date,na.rm=TRUE),
    sort_by = c("desc(year)"),
    file_types = NULL,
    #sort_by = c("desc(data_mb)"),
    edit = FALSE,
    open_data = "all",
    has_tags ="all",
    like = 0
  )
  app$list.ids = NULL
  app$list.html = NULL
  
  if (!is.null(userid)) {
    app$list.ids = load.custom.list(userid)$ids
  }
  
  sort_fields = c("data_mb","date","title", "journ")
  sort_fields = c(paste0("desc(",sort_fields,")"),sort_fields)
  
#  art_jel = dbGet(db,"jel")
  
  app$adf = arrange(dat, desc(date)) %>%
    filter(is.na(data_mb) | data_mb>0 | archive_mb >0) %>%
    slice(1:200)
  
  app$glob$dat = dat
  app$glob$sort_fields = sort_fields

  help.html = merge.lines(readLines(system.file("html/help.html",package = "EconJournalData")))
  about.html = merge.lines(readLines(system.file("html/about.html",package = "EconJournalData")))
  
  
  panels = list(
    tabPanel("Search Results",uiOutput("searchHtml")),
    if (use.lists) custom.list.panel.ui(app=app),
    tabPanel("Help",HTML(help.html)),
    tabPanel("About",HTML(about.html))    
  )
  if (!use.lists) panels = panels[-2]
  
  app$ui = fluidPage(
    titlePanel("Find Economic Articles with Data"),
    sidebarLayout(
      sidebarPanel(
        #h4("Find Economic Articles with Datasets and Code"),
        uiArticleSelectors()
      ),
      mainPanel(do.call(tabsetPanel, panels))
    ),
    if (use.lists) HTML('<script src="EconJournalData/Sortable.min.js"></script>'),
    if (use.lists) HTML('<script src="EconJournalData/lists.js"></script>'),
    HTML('<script src="EconJournalData/ejd.js"></script>')
  )
  
  if (!app$glob$use.lists) {
    addBtn = ""
  } else {
    addBtn = paste0(' <button title="Add article to your list." id="addBtn_',seq_len(NROW(app$adf)),'" class="btn btn-default btn-xs articleAddBtn"><i class="fa fa-plus"></i></button>')
  }
  
  html = paste0(shiny.articles.html(app$adf,postfix=addBtn), collapse="\n")
  html = paste0("<h4>",NROW(app$adf), " newest economic articles in data base (total ",NROW(app$glob$dat),") </h4>","<ol>",html,"</ol>")
  setUI("searchHtml",HTML(html))
  buttonHandler("searchBtn",search.btn.click)
  setDownloadHandler("searchDownloadBtn", 
    filename = "foundArticles.html",
    content = function(file) {
      restore.point("downLoadAsRmdHandler")
      app = getApp()
      writeLines(shiny.articles.html(app$adf,edit.tags = FALSE, add.btn=FALSE,add.li = FALSE, show.tags="complex"),file)
    },
    contentType = "text/html"
  )
  
  if (use.lists) {
    init.list.handlers()
  }
  
  if (edit.tags) {
    eventHandler("editTagChange","editTagChange",change.edit.tag)  }

  if (!is.null(log.file)) {
     eventHandler("linkClick","linkClick",fun=link.click)
  }
  
  shinyEvents::appInitHandler(function(session,app=getApp(),...) {
    app$id = random.string()
    str = paste0(Sys.time(),",",app$id)
    write.log(str, glob$login.log.file)
  })
  
  app
}

link.click = function(value, ..., app=getApp()) {
   restore.point("link.click")
   rank = match(value$id, app$adf$id)
   if (is.na(rank)) return()
   title = app$adf$title[rank]
   title = gsub(",", " ",title, fixed=TRUE)
   type = str.left.of(value$type, "_link")
   str = paste0(list(as.character(Sys.time()),app$id, value$id,type,rank, title),collapse=",")
   write.log(str, app$glob$link.log.file)
  
}

# Change edit tags
change.edit.tag = function(value,..., app=getApp()) {
  restore.point("change.edit.tag")
  cu = value
  if (is.null(cu$open_data) | isTRUE(cu$open_data == "U")) cu$open_data = NA
  if (is.null(cu$like)) cu$like = NA
  if (is.null(cu$complex)) cu$complex = NA
  if (is.null(cu$experiment)) cu$experiment = NA
  cu$like = as.integer(cu$like)
  cu$complex = as.integer(cu$complex)
  cu$comment = NA
  cu$taken = cu$taken == "yes"
  
  # Update DB
  cdb = get.custom.db()
  dbInsert(cdb,"article_tags", cu, mode="replace")
  
  # Update data in memory
  cu$has_tags = TRUE
  tcu = as_tibble(cu)
  dat.row = match(cu$id, app$glob$dat$id)
  if (!is.na(dat.row))
    app$glob$dat[dat.row,names(cu)] = tcu
  adf.row = match(cu$id, app$adf)  
  if (!is.na(adf.row))
    app$adf[adf.row,names(cu)] = tcu
  
  
  invisible(cu)  
}

search.btn.click = function(app,session,...) {
  progress <- shiny::Progress$new(session, min=0, max=100)
  on.exit(progress$close())

  progress$set(message = 'Searching may take a while...')
  restore.point("search.btn.click")
  opt = app$opt
  adf = app$glob$dat
  fields = c("abs_keywords", "ignore_without_data", "max_articles","journals","sort_by", "start_date","end_date","file_types","open_data","has_tags","like")
  for (f in fields) {
    opt[[f]] = getInputValue(f)
  }
  app$opt = opt
  restore.point("search.btn.click")
  
  if (!is.null(opt$journals)) {
    adf = filter(adf, journ %in% opt$journals)
  }
 
  if (isTRUE(opt$ignore_without_data)) {
    adf = filter(adf,is.na(data_mb) | data_mb>0 | archive_mb >0)
  }
  
  if (!is.null(opt$start_date)) {
    adf = filter(adf, date >= opt$start_date)
  }
  
  if (!is.null(opt$end_date)) {
    adf = filter(adf, date <= opt$end_date)
  }
  
  if (!is.null(opt$abs_keywords)) {
    if(nchar(str.trim(opt$abs_keywords))>0)
      adf = abstracts.keyword.search(opt$abs_keywords, adf, adf$search_content)
  }

  if (isTRUE(opt$like>0)) {
    if (opt$like == 0.5) {
      adf = filter(adf,is.na(like) | vec.is.true(like>0))
    } else {
      adf = filter(adf,vec.is.true(like>=opt$like))
    }
  }
  
  if (!is.null(opt$file_types)) {
    fs = app$glob[["fs"]]
    if (is.null(fs)) {
      db = get.articles.db()
      fs=app$glob$fs = as_tibble(dbGet(db,"files_summary"))
    }
    cur.fs = unique(fs[fs$file_type %in% opt$file_types, "id"])
    adf = semi_join(adf, cur.fs, by="id")
  }
  
  if (isTRUE(opt$has_tags!="all")) {
    if (opt$has_tags == "has_tags") {
      adf = adf[adf$has_tags,,drop=FALSE]
    } else if (opt$has_tags == "no_tags") {
      adf = adf[!adf$has_tags,,drop=FALSE]
    }
  }
  
  
  
  if (length(opt$sort_by)>0) 
    adf = s_arrange(adf, opt$sort_by)
  
  
  
  if (!is.na(max_art <- as.integer(opt$max_articles)) & NROW(adf)>0) {
    adf = adf[1:min(max_art, NROW(adf)),,drop=FALSE]
  } 
  app$adf = adf

  
  
  html = shiny.articles.html(app$adf, app=app)
  app$html = paste0(html, collapse="\n\n")
  
  ui = tagList(
    h4(paste0("Found ",NROW(app$adf), " articles (from ",NROW(app$glob$dat),") "),  downloadButton("searchDownloadBtn","Download",class = "btn-xs")),
    tags$ol(HTML(html))
  )

  setUI("searchHtml",ui)
  
  log.file = app$glob$log.file
  if (!is.null(log.file)) {
    query = opt$abs_keywords
    query = gsub(",","",query,fixed = TRUE)
    str = paste0(as.character(Sys.time()),",",opt$abs_keywords,",", NROW(adf), ",", app$id)
    write.log(str, log.file)
  }
}

write.log = function(str, log.file) {
  restore.point("write.log")
  if (is.null(log.file)) return()
  con = file(log.file,"at")
  try(writeLines(str,con))
  close(con)
}

uiArticleSelectors = function(app=getApp()) {
  restore.point("uiArticleSelectors")
  cat("uiArticleSelectors")

  #jel.codes = app$glob$jel.codes

  opt = app$opt
  
  uiSortBy = selectizeInput("sort_by","Sort by",app$glob$sort_fields, selected=opt$sort_by, multiple=TRUE)      
  uiMaxArticles = numericInput("max_articles","Max. shown Articles",value = opt$max_articles)
  
  li = as.list(opt$journals); names(li) = opt$journals
  uiJournal = selectizeInput("journals", "Journals:",li,selected=li, multiple=TRUE,
    options = list(
      maxItems=100
    )
  )

  ejd.opt = get.ejd.opts()
  li = ejd.opt$file_types$code_ext
  uiFileTypes = selectizeInput("file_types", "Require code of type",li, multiple=TRUE,
    options = list(
      maxItems=100
    )
  )

  uiStartDate = dateInput("start_date","Date from:",value="2005-01-01",format= "mm/yyyy")
  uiEndDate = dateInput("end_date","Date to:",value=as.Date(Sys.time()+1e6),format= "mm/yyyy")

  uiIgnoreWithoutData = checkboxInput("ignore_without_data","Only articles with data or code supplement",value = TRUE)
  
  about = HTML('
    <span>created by</span>
    <br>
    <span><a href="https://www.uni-ulm.de/mawi/mawi-wiwi/institut/mitarbeiter/skranz/" class="footer-link" target="_blank">Sebastian Kranz</a></span>
    <br>
    <span><a href="https://www.uni-ulm.de/en/mawi/faculty/" class="footer-link" target="_blank">Ulm University</a></span>
    <br>
  ')

  uiTags = tagList(
    if (app$glob$show.tags)
      selectInput("open_data", "Articles with Data marked as Confidential / Propietary", choices=list("Show all"="all","Ommit"="no_R","Require explicit flag that all data is open."="only_O")),
    if (app$glob$show.likes)
      selectInput("like", "Recommended", choices=list("Show all"=0,"Only recommended or unrated"=0.5, "Only recommended"=1,"Only strongly recommended"=2)),
    if (app$glob$show.tags)
      selectInput("experiments", "Tagged as Experiment", choices=list("Show all"="all","Only Experiments"="exp","Only Field Experiments"="F","Only Lab Experiments"="L", "No experiments"="no")),
    if (app$glob$edit.tags)
      selectInput("has_tags", "Manually Tagged?", choices=list("Show all"="all","Is tagged"="has_tags","Is not tagged"="no_tags"))
    
  )
  
  ui = verticalLayout(
    #div(class = "form-group shiny-input-container", tags$label("Keywords in Title and Abstract", title='Keywords', `for` = "abs_keywords"), tags$input(id = "abs_keywords", type = "text", class = "form-control", value = "", placeholder = NULL)),
    textInput("abs_keywords",label = "Keywords in Title and Abstract",value = ""),
    simpleButton("searchBtn","Search"),
    br(),
    bsCollapse(bsCollapsePanel(value="advancedFilterCollapse",
    #wellPanel(skCollapsePanel(
      title="Avdanced Options",
      uiIgnoreWithoutData,
      uiJournal,
      uiStartDate,uiEndDate,
      uiSortBy,
      uiFileTypes,
      uiMaxArticles,
      uiTags
    )),
    about
  )
  return(ui)
}


skCollapsePanel = function(title, ..., titleUI=NULL, id = NULL, value = NULL) 
{
  content <- list(...)
  if (is.null(id)) 
      id <- paste0("cpanel", sprintf("%07i", as.integer(stats::runif(1, 
          1, 1e+06))))
  if (is.null(value)) {
      value = title
  }
  tags$div(class = "accordion-group",
        
    tags$div(class = "accordion-heading", 
      HTML("<table><tr><td>"),
      titleUI,
      HTML("</td><td>"),
      tags$a(`data-toggle` = "collapse", href = paste0("#", id), title),
      HTML("</td></tr></table>")    
    ),
    tags$div(class = "accordion-body collapse", id = id, `data-value` = value,
      tags$div(class = "accordion-inner", content)
    )
  )
}

shiny.articles.html = function(adf=app$adf, add.btn=isTRUE(app$glob$use.lists),del.btn=FALSE, edit.tags=isTRUE(app$glob$edit.tags),add.li=TRUE,...,no.add.rows=NA, show.tags=NULL, app=getApp()) {
  restore.point("shiny.articles.html")
  postfix = rep("", NROW(adf))
  if (add.btn) {
    if (identical(no.add.rows,NA)) {
      no.add.rows = match(app$list.ids, adf$id)
    }
    addBtn = paste0(' <button id="addBtn_',seq_len(NROW(adf)),'" title="Add article to your list." class="btn btn-default btn-xs articleAddBtn"><i class="fa fa-plus"></i></button>')
    addBtn[no.add.rows] = ""
    postfix=paste0(postfix, addBtn)
  }
  if (del.btn) {
    delBtn = paste0(' <button id="delBtn_',adf$id,'" class="btn btn-default btn-xs customListDelBtn"><i class="fa fa-remove"></i></button>')
    postfix=paste0(postfix, delBtn)
  }
  if (edit.tags) {
    n = NROW(adf)
    
    # Open Data
    str = radioGroupButtonsVector("open_data",values = adf$open_data,
      choices = c("O","R","P","U"), labels = c("O","R","P","?"),size="xs")
    ed = str
    # Like Button
    str = radioGroupButtonsVector("like",values = as.character(adf$like),
      choices = c("2", "1","0","U"), labels = c("+2","+1", "0","?"),size="xs")
    ed = paste0(ed, " ", str)
    str = radioGroupButtonsVector("complex",values = ifelse(vec.is.true(adf$complex==1),"1","0"),
      choices = c("1","0"), labels = c("complex","?"),size="xs")
    ed = paste0(ed, " ", str)
    str = radioGroupButtonsVector("experiment",values = adf$experiment, choices = c("E","F","O"), labels = c("E","F","-"),size="xs")
    ed = paste0(ed, " ", str)
    str = radioGroupButtonsVector("taken",values = ifelse(vec.is.true(adf$taken),"yes","no"),
      choices = c("no","yes"), labels = c("Free","Taken"),size="xs")
    ed = paste0(ed, " ", str)
    ed = paste0('<span class="edit_tags" id="edit_tags_', adf$id,'"  data-art="', adf$id,'">', ed,'</span>')
    
    postfix = paste0(postfix," ", ed)
  }
  if ("complex" %in% show.tags) {
    postfix = paste0(postfix, ifelse(vec.is.true(adf$complex)," <em>complex</em>",""))
  }
  
  
  html = simple_articles_html(adf,postfix=postfix)
  if (add.li) {
    html = paste0("<li>", html,"</li>")
  }
  html
}



make.authors.summary.rds = function(db=get.articles.db(), authors = dbGet(db,"author"), file="authors_summary.RDS") {
  asum = authors %>% 
    group_by(id) %>%
    summarise(authors = paste0(author, collapse="; ")) %>%
    ungroup()
  saveRDS(asum, file) 
  invisible(asum)
}