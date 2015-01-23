examples.articlesApp = function() {
  library(shinyEvents)
  library(shinyBS)
  library(EconJournalData)
  
  set.restore.point.options(display.restore.point = TRUE)
  init.journal.scrapper()
  app = articlesApp()
  runEventsApp(app)
}

articlesApp = function() {
  library(shinyEvents)
  library(shinyBS)
  library(dplyrExtras)

  

  jel = jel.codes
  jel$digit1  = substring(jel$code,1,1)
  jel$digit12 = substring(jel$code,1,2)
  jel$digit2 = substring(jel$code,2,2)
  
  
  
  
  app = eventsApp()
  
  dt = as.data.frame(read.complete.data())
  dt$code.str = make.code.str(dt)
  dt = dt[order(-dt$data.size, dt$has.data),]
  
  app$opt = list(
    max.articles = 200,
    journals = names(jis),
    start.date = min(dt$date,na.rm=TRUE),
    end.date = max(dt$date,na.rm=TRUE),
    with.tags = NULL,
    without.tags = NULL,
    sort.by = c("desc(data.size)"),
    edit = FALSE
  )
  
  #rows = is.na(adt$data.size) | adt$data.size >0
  #adt = adt[rows,]
  #adt = as.data.frame(dt)
  #if (NROW(adt)>app$opt$max.articles)
  #  adt = adt[1:app$opt$max.articles,]
  
  dfav = read.favorite.articles()
  app$adt = dfav

  sort.fields = c("data.size","date","title", "journal")
  sort.fields = c(sort.fields, paste0("desc(",sort.fields,")"))
  
  app$glob$dt = dt
  app$glob$jel = as.data.frame(jel)
  app$glob$jel.codes = setdiff(LETTERS,c("S","T","U","V","W","X"))
  app$glob$sort.fields = sort.fields
  app$glob$jel.dt = as.data.frame(read.articles.jel.csv())
  
  
  app$ui = shinyUI(navbarPage("Find Economic Articles with Data",
    tabPanel("Articles", sidebarLayout(
      sidebarPanel(
        uiArticleSelectors()
      ),
      mainPanel(
        uiOutput("articlesHtml") 
      )
    ))
  ))
  html = paste0(shiny.articles.html(app$adt), collapse="\n")
  html = paste0("<h4>",NROW(app$adt), " selected articles of ",NROW(app$glob$dt)," </h4>",html)
  setUI("articlesHtml",HTML(html))
  buttonHandler("showBtn",show.btn.click)
  app
}


show.btn.click = function(app,session,...) {
  
  progress <- shiny::Progress$new(session, min=0, max=100)
  on.exit(progress$close())

  progress$set(message = 'Searching may take a while...')

  
  opt = app$opt
  adt = app$glob$dt
  fields = c("google", "max.articles","jel","journals","with.tags","without.tags","sort.by")
  for (f in fields) {
    opt[[f]] = getInputValue(f)
  }
  app$opt = opt
  restore.point("show.btn.click")
  
  if (!is.null(opt$journals)) {
    adt = filter(adt, journal %in% opt$journals)
  }
 
  if (!is.null(opt$google)) {
    if(nchar(str.trim(opt$google))>0)
      adt = google.journals(opt$google, adt)
  }

  combine = "and"
  if (!is.null(opt$jel)) {
    if(nchar(str.trim(opt$jel))>0) {
      jel = str.trim(opt$jel)
      jel = gsub(" ",",",jel,fixed=TRUE)
      jel = strsplit(jel,split = ",",fixed=TRUE)[[1]]
      jel.dt = app$glob$jel.dt
      id = adt$id
      for (j in jel) {
        new.id = unique(jel.dt$id[substring(jel.dt$jel,1,nchar(j))==j])
        id = intersect(id, new.id)
      }
      adt = adt[adt$id %in% id,]  
    }
  }

  if (length(opt$sort.by)>0) 
    adt = s_arrange(adt, opt$sort.by)
  app$adt = adt

  

  html = paste0(shiny.articles.html(app$adt, app=app), collapse="\n")
  html = paste0("<h4> Found ",NROW(app$adt), " articles (from ",NROW(app$glob$dt),") </h4>",html)


  setUI("articlesHtml",HTML(html))
}

get.selected.jel.or = function(jel1=app$opt$jel1, jel2=app$opt$jel2, jel3=app$opt$jel3, jel=app$glob$jel, app=getApp()) {
  restore.point("get.selected.jel")
  
  if (!is.null(jel1)) {
    jel = filter(jel,digit1 %in% jel1)
  }
  if (!is.null(jel2)) {
    d1 = substring(opt$jel2,1,1)
    free.jel1 = setdiff(opt$jel1,d1)
    jel = filter(jel,digit12 %in% opt$jel2 | digit1 %in% free.jel1)
  }
  if (!is.null(jel3)) {
    jel = filter(jel,code %in% jel3)
  }
  jel
}

filter.articles = function(adt,ajel.dt,journals=NULL,jel1=NULL,...) {
  restore.point("filter.articles")
  dt = as.data.table(adt)
  changed=FALSE
  if (!is.null(jel1)) {
    library(dplyr)
    id = unique(ajel.dt[jel1]$id)
    setkey(dt,id)
    dt = dt[id]
    changed=TRUE
  }
  if (changed) {
    ord = order(-dt$data.size)
    dt = dt[ord,]
  }
  dt
}


uiArticleSelectors = function(app=getApp()) {
  restore.point("uiArticleSelectors")
  cat("uiArticleSelectors")

  jel.codes = app$glob$jel.codes

  opt = app$opt
  
  uiGoogle = textInput("google","Google", "")
  
  uiSortBy = selectizeInput("sort.by","Sort by",app$glob$sort.fields, selected=opt$sort.by, multiple=TRUE)      
  uiMaxArticles = numericInput("max.articles","Max. shown Articles",value = opt$max.articles)
  
  li = as.list(opt$journals); names(li) = opt$journals
  uiJournal = selectizeInput("journals", "Journals:",li,selected=li, multiple=TRUE,
    options = list(
      maxItems=100
    )
  )

  uiJel = textInput("jel","JEL Codes:")
      
  uiDateRange = dateRangeInput("daterange", "Date range:",
               start = "2000-01-01",
               end   = NULL,
               format= "mm/yyyy")

  
  
  tags = read.csv(paste0(main.dir,"/tags.csv"),stringsAsFactors=FALSE)
  tags.li = tags$code
  names(tags.li) = tags$label
  
  uiWithTags=selectizeInput("with.tag",label="With Tag",choices=tags.li, multiple=TRUE)
  uiWithoutTags=selectizeInput("without.tag",label="Without Tag",choices=tags.li, multiple=TRUE)

  about = HTML('
    <span>created by</span>
    <br>
    <span>Sebastian Kranz</span>
    <br>
    <span><a href="http://www.uni-ulm.de/mawi/mawi-wiwi/mitarbeiter/skranz.html" class="footer-link" target="_blank">Ulm University</a></span>
    <br>
  ')
  about2 = bsCollapse(bsCollapsePanel(title="About", HTML('
  <p> The database contains information about recent articles and their data appendixes from the journals of the American Economic Association and the Review of Economic Studies. This app can help to find an interesting article for an <a href="https://github.com/skranz/RTutor" target="_blank">RTutor</a> problem set.</a>.</p>
    '))) 

  ui = verticalLayout(
    fluidRow(
      actionButton("showBtn","Search"),
      actionButton("updateBtn","Update")
    ),
    uiGoogle,
    uiJel,
    bsCollapse(bsCollapsePanel(id="advancedFilterCollapse",title="Advanced Search",
      uiMaxArticles,
      uiJournal,
      uiDateRange,
      uiWithTags,
      uiWithoutTags        
    )),
    uiSortBy,
    about2,
    about
  )
  return(ui)
}

uiJEL = function(jel) {
  restore.point("uiStagesAccordion")
  
  jel1 = jel[jel$digits==1,] 
  panels.html = lapply(1:NROW(jel1), function(i){
    restore.point("hfjfj")
    je = jel1[i,,drop=FALSE]
    name = je$code
    button <- paste0(
      '<input type="checkbox" id="',paste0("checkJEL_",name),'" checked="checked">',
       actionButton(paste0("onlyJEL_",name),bsGlyph("icon-arrow-right"))
    )
    #button = checkboxInput(inputId=paste0("checkJEL_",name), label="", value = TRUE)
    title = paste0("<h4> ",name,': ', je$label,"</h4>")
    txt = paste0('<div>\n', "Hi",'\n</div>')
    skCollapsePanel(HTML(title),HTML(txt),titleUI=HTML(button), id=name)
  })
  panels.html[[1]]
  args = c(list(multiple = FALSE, open = NULL, id = "jelAccordion"), panels.html)
  do.call(bsCollapse,args)
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

shiny.articles.html.noedit = function(d=app$adt, app=getApp(), zip.link=isTRUE(app$glob$zip.link)) {
  local.url = paste0(data.dir,"/",d$journal,"_vol_",d$vol,"_issue_",d$issue,"_article_",d$articleNum,".zip")
  
  if (zip.link) {
    data.url = ifelse(nchar(d$data.url)>0,
      paste0('<a href="file://',local.url,'">  (downloaded zip) </a>'),
      "")
  } else {
    data.url = ""
  }
  
  str = paste0('<p><a href="', d$url,'" target="_blank">',d$title,'</a>',
    ' (', signif(d$data.size,4),' MB, ' , d$journal,', ', d$publication.dat,') <BR>', d$JEL,
    '<BR> ', d$code.str, data.url,  
    '</p>') 
  str
}

shiny.articles.html = function(adt=app$adt, app=getApp()) {
  restore.point("shiny.articles.html")
  if (!isTRUE(app$opt$edit)) {
    return(shiny.articles.html.noedit(d = adt))
  }
  
  d = adt
  
  local.url = paste0("file:///",data.dir,"/",d$journal,"_vol_",d$vol,"_issue_",d$issue,"_article_",d$articleNum,".zip")

  data.url = ifelse(nchar(d$data.url)>0,
    paste0('<a href="',local.url,'">  (downloaded zip) </a>'),
    "")

  txt = paste0(
    '<p><h5><a href="', d$url,'">',d$title,'</a>',
    ' (', signif(d$data.size,4),' MB, ' , d$journal,', ', d$publication.dat,')</h5>',
    '', d$code.str, data.url ,
    '<br>', articles.tag.checkboxes(d,tags.csv),
    '&nbsp;<input id="comment_',d$id,'" type="text" value=""/>', 
    '</p>'
  ) 

  content.txt =  merge.lines(txt, collapse="\n")
  body.txt = paste0('<body>', wrap.in.bootstrap.div(content.txt),"</body>")
  header.txt = paste0("<header>", html.css.imports(),'\n', html.script.imports(), "</header>", sep="\n")
  txt = paste0(header.txt, body.txt, sep="\n")
  invisible(txt)  
}

articles.tag.checkboxes = function(d, tags) {
  restore.point("articles.tag.checkboxes")
  input.value = paste0(d$id)
  li = lapply(1:NROW(tags), function(i) {
    input.id = paste0(tags$code[i],'_',1:NROW(d))
    checked.str = ifelse(is.true(d[[tags$code[i]]]),' checked="checked"',"")
    paste0('<input name="',tags$code[i],'",id="',input.id,
           '" type="checkbox"',checked.str,'value="',input.value, '"/>', tags$label[i])
  })
  txt = do.call("paste", c(li, list(sep="\n")))
  txt
}


