examples.articlesApp = function() {
  library(EconJournalData)
  setwd("D:/libraries/EconJournalData/")
  restore.point.options(display.restore.point = TRUE)
  init.ejd()
  opts=get.ejd.opts()
  db=get.articles.db()
  app = articlesApp(use.lists=TRUE, log.file="log.csv")
  viewApp(app)
}

articlesApp = function(opts=get.ejd.opts(), db=get.articles.db(), summary.file = "articles_summary.RDS", readme.base.url = "http://econ.mathematik.uni-ulm.de/ejd/readme_files/", use.lists=TRUE, log.file=NULL) {
  restore.point("articlesApp")
  library(shinyEvents)
  library(shinyBS)
  library(dplyrExtras)
  
  jis = opts$jis

  addResourcePath('EconJournalData', system.file('www', package='EconJournalData'))  

  app = eventsApp()
  app$glob$readme.url = readme.base.url
  app$glob$use.lists = use.lists
  app$glob$log.file = log.file
  
  articles = dbGet(db,"article") %>%
    filter(has_data)
  
  dat = articles %>%
    add_code_and_data_str(file = summary.file, overwrite=FALSE)

  dat$search_contents = tolower(paste0(dat$title," ",dat$title," ",dat$title," ",dat$title," ",dat$abstract))

  
  app$opt = list(
    max_articles = 200,
    ignore_without_data = TRUE,
    journals = names(jis),
    start_date = min(dat$date,na.rm=TRUE),
    end_date = max(dat$date,na.rm=TRUE),
    sort_by = c("desc(year)"),
    file_types = NULL,
    #sort_by = c("desc(data_mb)"),
    edit = FALSE
  )
  app$list.ids = NULL
  app$list.html = NULL
  
  sort_fields = c("data_mb","date","title", "journ")
  sort_fields = c(sort_fields, paste0("desc(",sort_fields,")"))
  
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
    if (use.lists) tabPanel("Your List",
      p("You can add search results to your custom list of articles. Use drag-and-drop to reorder the list."),
      downloadButton("customListDownloadBtn","Download HTML",class = "btn-xs"),
      hr(),
      tags$ol(id="customListSortable"),
      tags$script(HTML("customListSortableCreate();"))
      #uiOutput("listHtml")
    ),
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
    if (use.lists) HTML('<script src="EconJournalData/ejd.js"></script>')
  )
  
  if (!app$glob$use.lists) {
    addBtn = ""
  } else {
    addBtn = paste0(' <button title="Add article to your list." id="addBtn_',seq_len(NROW(app$adf)),'" class="btn btn-default btn-xs articleAddBtn"><i class="fa fa-plus"></i></button>')
  }
  
  html = paste0(shiny.articles.html(app$adf,postfix=addBtn), collapse="\n")
  html = paste0("<h4>",NROW(app$adf), " newest economic articles in data base (total ",NROW(app$glob$dat),") </h4>",html)
  setUI("searchHtml",HTML(html))
  buttonHandler("searchBtn",search.btn.click)
  setDownloadHandler("searchDownloadBtn", 
    filename = "foundArticles.html",
    content = function(file) {
      restore.point("downLoadAsRmdHandler")
      app = getApp()
      writeLines(shiny.articles.html(app$adf),file)
    },
    contentType = "text/html"
  )
  
  if (use.lists) {
    init.list.handlers()
  }

  app
}

search.btn.click = function(app,session,...) {
  progress <- shiny::Progress$new(session, min=0, max=100)
  on.exit(progress$close())

  progress$set(message = 'Searching may take a while...')
  restore.point("search.btn.click")
  opt = app$opt
  adf = app$glob$dat
  fields = c("abs_keywords", "ignore_without_data", "max_articles","journals","sort_by", "start_date","end_date","file_types")
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

  if (!is.null(opt$file_types)) {
    fs = app$glob[["fs"]]
    if (is.null(fs)) {
      db = get.articles.db()
      fs=app$glob$fs = as_tibble(dbGet(db,"files_summary"))
    }
    cur.fs = unique(fs[fs$file_type %in% opt$file_types, "id"])
    adf = semi_join(adf, cur.fs, by="id")
  }
  
  if (length(opt$sort_by)>0) 
    adf = s_arrange(adf, opt$sort_by)
  app$adf = adf

  
  if (!app$glob$use.lists) {
    addBtn = ""
  } else {
    addBtn = paste0(' <button id="addBtn_',seq_len(NROW(adf)),'" title="Add article to your list." class="btn btn-default btn-xs articleAddBtn"><i class="fa fa-plus"></i></button>')
  }
  
  html = shiny.articles.html(app$adf, app=app, postfix=addBtn)
  app$html = paste0(html, collapse="\n\n")
  
  ui = tagList(
    h4(paste0("Found ",NROW(app$adf), " articles (from ",NROW(app$glob$dat),") "),  downloadButton("searchDownloadBtn","Download",class = "btn-xs")),
    HTML(html)
  )

  setUI("searchHtml",ui)
  
  log.file = app$glob$log.file
  if (!is.null(log.file)) {
    query = opt$abs_keywords
    query = gsub(",","",query,fixed = TRUE)
    con = file(log.file,"at")
    str = paste0(as.character(Sys.time()),",",opt$abs_keywords,",", NROW(adf))
    try(writeLines(str,con))
    close(con)
  }
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
      uiMaxArticles
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

shiny.articles.html = function(adf=app$adf,..., app=getApp()) {
  restore.point("shiny.articles.html")
  simple_articles_html(adf,...)
}


