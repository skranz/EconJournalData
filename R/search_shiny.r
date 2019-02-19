examples.articlesApp = function() {
  library(EconJournalData)
  setwd("D:/libraries/EconJournalData/")
  restore.point.options(display.restore.point = TRUE)
  init.ejd()
  opts=get.ejd.opts()
  db=get.articles.db()
  app = articlesApp(use.lists=FALSE)
  viewApp(app)
}

articlesApp = function(opts=get.ejd.opts(), db=get.articles.db(), summary.file = "articles_summary.RDS", readme.base.url = "http://econ.mathematik.uni-ulm.de/ejd/readme_files/", use.lists=TRUE) {
  restore.point("articlesApp")
  library(shinyEvents)
  library(shinyBS)
  library(dplyrExtras)
  
  jis = opts$jis

  addResourcePath('EconJournalData', system.file('www', package='EconJournalData'))  
  
#  jel = opts$jel.codes
#  jel$digit1  = substring(jel$code,1,1)
#  jel$digit12 = substring(jel$code,1,2)
#  jel$digit2 = substring(jel$code,2,2)
  
  app = eventsApp()
  app$glob$readme.url = readme.base.url
  app$glob$use.lists = use.lists
  
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
#  app$glob$jel = jel
#  app$glob$jel.codes = setdiff(LETTERS,c("S","T","U","V","W","X"))
  app$glob$sort_fields = sort_fields
#  app$glob$art_jel = art_jel
  
  about.html = merge.lines(readLines(system.file("html/help.html",package = "EconJournalData")))
  app$ui = fluidPage(
    titlePanel("Find Economic Articles with Data"),
    sidebarLayout(
      sidebarPanel(
        #h4("Find Economic Articles with Datasets and Code"),
        uiArticleSelectors()
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Search Results",uiOutput("searchHtml")),
          # if (use.lists) tabPanel("Your List",
          #   p("You can add search results to your selected list of articles. Use drag-and-drop to reorder the list."),
          #   downloadButton("listDownloadBtn","Download list as html",class = "btn-xs"),
          #   hr(),
          #   uiOutput("listHtml")),
          tabPanel("About",HTML(about.html))
        )
      )
    ),
    HTML('<script src="EconJournalData/Sortable.min.js"></script>')
  )
  html = paste0(shiny.articles.html(app$adf), collapse="\n")
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

  
  app
}


search.btn.click = function(app,session,...) {
  progress <- shiny::Progress$new(session, min=0, max=100)
  on.exit(progress$close())

  progress$set(message = 'Searching may take a while...')
  restore.point("search.btn.click")
  opt = app$opt
  adf = app$glob$dat
  fields = c("abs_keywords", "ignore_without_data", "max_articles","journals","with_tags","without_tags","sort_by", "start_date","end_date")
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

  # combine = "and"
  # if (!is.null(opt$jel)) {
  #   if(nchar(str.trim(opt$jel))>0) {
  #     jel = str.trim(opt$jel)
  #     jel = gsub(" ",",",jel,fixed=TRUE)
  #     jel = strsplit(jel,split = ",",fixed=TRUE)[[1]]
  #     jel.dt = app$glob$jel.dt
  #     id = adf$id
  #     for (j in jel) {
  #       new.id = unique(jel.dt$id[substring(jel.dt$jel,1,nchar(j))==j])
  #       id = intersect(id, new.id)
  #     }
  #     adf = adf[adf$id %in% id,]  
  #   }
  # }

  if (length(opt$sort_by)>0) 
    adf = s_arrange(adf, opt$sort_by)
  app$adf = adf

  
  # <button id="id" style="" type="button" class="btn btn-default action-button ">name</button>
  addBtn = paste0(' <button id="addBtn_',seq_len(NROW(adf)),'" class="btn btn-default btn-xs articleAddBtn"><i class="fa fa-plus"></i></button>')
  if (!app$glob$use.lists) addBtn = ""
  
  html = shiny.articles.html(app$adf, app=app, postfix=addBtn)
  app$html = paste0(html, collapse="\n\n")
  
  ui = tagList(
    h4(paste0("Found ",NROW(app$adf), " articles (from ",NROW(app$glob$dat),") "),  downloadButton("searchDownloadBtn","Download",class = "btn-xs")),
    HTML(html)
  )
  buttonHandler("showSourceBtn",function(app=getApp(),...){
    ui = pre(style="white-space: pre-wrap;",app$html)
    setUI("searchHtml",ui)
  })
  classEventHandler("articleAddBtn",event = "click",article.add.click)
  
  setUI("searchHtml",ui)
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
  
  app$list.ids = unique(c(app$list.ids,art$id))
  delBtn = paste0(' <button id="delBtn_',app$list.ids,'" class="btn btn-default btn-xs articleDelBtn"><i class="fa fa-remove"></i></button>')
  html = simple_articles_html(art, postfix = delBtn)
  app$list.html = c(app$list.html, html)
  update.list.html()
  showNotification(paste0("Add as article #", NROW(app$list.ids),"."), duration = 2, closeButton = FALSE)
}

update.list.html = function(html = app$list.html, app=getApp()) {
  restore.point("update.list.html")
  js = "
    // List with handle
    Sortable.create(articleListSortable, {});
  "

  html = paste0('<li>',html,'</li>')
  
  ui = tagList(
    #div(id="listWithHandle", class="list-group"),
    tags$ol(id="articleListSortable", HTML(html)),
    #HTML(html),
    tags$script(HTML(js))
  )
  
  setUI("listHtml", ui)
  dsetUI("listHtml", ui)
  
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

  #uiJel = textInput("jel","Only following JEL Codes:")
  #uiJelLink = HTML('<a href="https://www.aeaweb.org/econlit/jelCodes.php?view=jel" target="blank_">List of JEL codes</a>') 
        
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
    textInput("abs_keywords",label = "Keywords in Title and Abstract",value = ""),
    simpleButton("searchBtn","Search"),
    br(),
    bsCollapse(bsCollapsePanel(value="advancedFilterCollapse",title="Avdanced Options",
      uiIgnoreWithoutData,
      uiJournal,
      uiStartDate,uiEndDate,
      uiSortBy,
      #uiJel,uiJelLink,
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


