examples.articlesApp = function() {
  library(EconJournalData)
  setwd("D:/libraries/EconJournalData/")
  restore.point.options(display.restore.point = TRUE)
  init.ejd()
  opts=get.ejd.opts()
  db=get.articles.db()
  app = articlesApp(show.google=FALSE)
  viewApp(app)
}

articlesApp = function(show.google=TRUE, opts=get.ejd.opts(), db=get.articles.db(), summary.file = "articles_summary.RDS") {
  restore.point("articlesApp")
  library(shinyEvents)
  library(shinyBS)
  library(dplyrExtras)
  
  jis = opts$jis
  
  
#  jel = opts$jel.codes
#  jel$digit1  = substring(jel$code,1,1)
#  jel$digit12 = substring(jel$code,1,2)
#  jel$digit2 = substring(jel$code,2,2)
  
  app = eventsApp()
  
  articles = dbGet(db,"article") %>%
    filter(has_data)
  
  dat = articles %>%
    add_code_and_data_str(file = summary.file, overwrite=FALSE)
  dat = dat[order(-dat$data_mb),]
  dat$search_contents = tolower(paste0(dat$title," ",dat$abstract))

  
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

  
  app$ui = fluidPage(
    #titlePanel("Find Economic Articles with Data",
    sidebarLayout(
      sidebarPanel(
        uiArticleSelectors()
      ),
      mainPanel(
        uiOutput("articlesHtml") 
      )
    )
  )
  html = paste0(shiny.articles.html(app$adf), collapse="\n")
  html = paste0("<h4>",NROW(app$adf), " selected articles of ",NROW(app$glob$dat)," </h4>",html)
  setUI("articlesHtml",HTML(html))
  buttonHandler("showBtn",show.btn.click)
  app
}


show.btn.click = function(app,session,...) {

  progress <- shiny::Progress$new(session, min=0, max=100)
  on.exit(progress$close())

  progress$set(message = 'Searching may take a while...')
  opt = app$opt
  adf = app$glob$dat
  fields = c("keywords", "ignore_without_data", "max_articles","journals","with_tags","without_tags","sort_by", "start_date","end_date")
  for (f in fields) {
    opt[[f]] = getInputValue(f)
  }
  app$opt = opt
  restore.point("show.btn.click")
  
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
  
  if (!is.null(opt$keywords)) {
    if(nchar(str.trim(opt$keywords))>0)
      adf = abstracts.keyword.search(opt$keywords, adf, adf$search_content)
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

  

  html = paste0(shiny.articles.html(app$adf, app=app), collapse="\n")
  html = paste0("<h4> Found ",NROW(app$adf), " articles (from ",NROW(app$glob$dat),") </h4>",html)

  setUI("articlesHtml",HTML(html))
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

  uiIgnoreWithoutData = checkboxInput("ignore_without_data","Only articles with data",value = TRUE)
  
  about = HTML('
    <span>created by</span>
    <br>
    <span>Sebastian Kranz</span>
    <br>
    <span><a href="http://www.uni-ulm.de/mawi/mawi-wiwi/mitarbeiter/skranz.html" class="footer-link" target="_blank">Ulm University</a></span>
    <br>
  ')
  about2 = bsCollapse(bsCollapsePanel(title="About", HTML('
  <p> The database contains information about recent articles and their data appendixes from the AEA journals, RESTUD and RESTAT. This app can help to find an interesting article for an <a href="https://github.com/skranz/RTutor" target="_blank">RTutor</a> problem set.</a>.</p>
    '))) 

  ui = verticalLayout(
    textInput("keywords",label = "Keywords in Abstract",value = ""),
    actionButton("showBtn","Search"),
    br(),
    bsCollapse(bsCollapsePanel(value="advancedFilterCollapse",title="Search Options",
      uiIgnoreWithoutData,
      uiJournal,
      uiStartDate,uiEndDate,
      #uiJel,uiJelLink,
      uiMaxArticles
    )),
    uiSortBy,
    about2,
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

shiny.articles.html = function(adf=app$adf, app=getApp()) {
  restore.point("shiny.articles.html")
  simple_articles_html(adf)
}


