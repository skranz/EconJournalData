examples.articlesApp = function() {
  set.restore.point.options(display.restore.point = TRUE)
  app = articlesApp()
  runEventsApp(app)
}

articlesApp = function() {
  library(shinyEvents)
  library(shinyBS)
  library(dplyrExtras)
  library(shinyTree)
  
  init.journal.scrapper()
  jel = jel.codes
  jel$digit1  = substring(jel$code,1,1)
  jel$digit12 = substring(jel$code,1,2)
  jel$digit2 = substring(jel$code,2,2)
  
  
  
  
  app = eventsApp()
  
  dt = read.complete.data()
  dt$code.str = make.code.str(dt)
  dt = dt[order(-dt$data.size, dt$has.data),]
  
  app$opt = list(
    max.articles = 100,
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
  adt = as.data.frame(dt)
  if (NROW(adt)>app$opt$max.articles)
    adt = adt[1:app$opt$max.articles,]
  app$adt = as.data.frame(adt)

  sort.fields = c("data.size","date","title", "journal")
  sort.fields = c(sort.fields, paste0("desc(",sort.fields,")"))
  
  app$glob$dt = dt
  app$glob$jel = as.data.frame(jel)
  app$glob$jel.codes = setdiff(LETTERS,c("S","T","U","V","W","X"))
  app$glob$sort.fields = sort.fields
  app$glob$jel.dt = as.data.frame(read.articles.jel.csv())
  
  
  app$ui = shinyUI(navbarPage("Reproducible Econonomics",
    tabPanel("Articles", sidebarLayout(
      sidebarPanel(
        uiArticleSelectors()
      ),
      mainPanel(
        uiOutput("articlesHtml") 
      )
    ))
  ))
  
  setUI("articlesHtml",HTML(shiny.articles.html(app$adt,app=app)))
  buttonHandler("showBtn",show.btn.click)
  app
}


show.btn.click = function(app,...) {
  opt = app$opt
  adt = app$glob$dt
  fields = c("google", "max.articles","jel1","jel2","jel3", "journals","with.tags","without.tags","sort.by")
  for (f in fields) {
    opt[[f]] = getInputValue(f)
  }
  app$opt = opt
  restore.point("show.btn.click")
  
  if (!is.null(opt$journals)) {
    adt = filter(adt, journal %in% opt$journals)
  }
  if (!is.null(opt$google)) {
    adt = google.journals(opt$google, adt)
  }

  combine = "and"
  if (combine == "or") {
    jel = get.selected.jel.or(app=app)
    jel.dt = semi_join(app$glob$jel.dt, jel, by=c("jel"="code"))
    adt = semi_join(adt, jel.dt, by="id")
    
  } else if (combine=="and") {
    jel.dt = app$glob$jel.dt
    id = adt$id
    if (!is.null(opt$jel3)) {
      for (j in opt$jel3) {
        new.id = unique(jel.dt$id[jel.dt$jel==j])
        id = intersect(id, new.id)
      }      
    } else {
      if (!is.null(opt$jel2)) {
        for (j in opt$jel2) {
          new.id = unique(jel.dt$id[substring(jel.dt$jel,1,2)==j])
          id = intersect(id, new.id)
        }
      } else {
        if (!is.null(opt$jel1)) {          
          for (j in opt$jel1) {
            new.id = unique(jel.dt$id[substring(jel.dt$jel,1,1)==j])
            id = intersect(id, new.id)
          }
        }
      }
    }
    adt = adt[adt$id %in% id,]  
  }

  if (length(opt$sort.by)>0) 
    adt = s_arrange(adt, opt$sort.by)
  app$adt = adt
  html = HTML(shiny.articles.html(app$adt,app=app))
  setUI("articlesHtml",html)
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

  jel = app$glob$jel
  jel1 = jel[jel$digits==1,] 
  #jel1 = jel[order(jel$digits,jel$code),]
  jel2 = jel[jel$digits==2 | (jel$digits==3 & jel$digit2 == "0"),] 
  jel3 = jel[jel$digits==3,] 

  li = as.list(jel1$code); names(li) = jel1$name
  uiJel1 = selectizeInput("jel1", "JEL (Area):",li,selected=NULL, multiple=TRUE,
    options = list(maxItems=30)
  )
  li = as.list(jel2$code); names(li) = jel2$name
  uiJel2 = selectizeInput("jel2", "JEL (2-Digits):",li,selected=NULL, multiple=TRUE,
    options = list(
      maxItems=30
    )
  )
  li = as.list(jel3$code); names(li) = jel3$name
  uiJel3 = selectizeInput("jel3", "JEL (3-Digits):",li,selected=NULL, multiple=TRUE,
    options = list(
      maxItems=30
    )
  )

  changeHandler("jel1", function(app,session,...) {
    cat("\n*****changeHandler.jel1****")
    jel = app$glob$jel
    value = getInputValue("jel1")
    jel2.choice = getInputValue("jel2")
    jel3.choice = getInputValue("jel2")

    restore.point("changeHandler.jel1")
    jel2 = jel[jel$digits==2 & (jel$digit1 %in% value),] 
    jel3 = jel[jel$digits==3 & (jel$digit1 %in% value),] 
    #new.jel2 = c(jel2.choice, jel2$code)
    
    
    li = as.list(jel2$code); names(li) = jel2$name
    updateSelectizeInput(session,"jel2", choices=li,selected=intersect(jel2.choice, unlist(li)))
    li = as.list(jel3$code); names(li) = jel3$name
    updateSelectizeInput(session,"jel3", choices=li,selected=intersect(jel3.choice, unlist(li)))
    
    show.btn.click(app = app)
  })
    
  
  uiDateRange = dateRangeInput("daterange", "Date range:",
               start = "2000-01-01",
               end   = NULL,
               format= "mm/yyyy")

  
  
  tags = read.csv(paste0(main.dir,"/tags.csv"),stringsAsFactors=FALSE)
  tags.li = tags$code
  names(tags.li) = tags$label
  
  uiWithTags=selectizeInput("with.tag",label="With Tag",choices=tags.li, multiple=TRUE)
  uiWithoutTags=selectizeInput("without.tag",label="Without Tag",choices=tags.li, multiple=TRUE)

  
  ui = verticalLayout(
    fluidRow(
      actionButton("showBtn","Show"),
      actionButton("updateBtn","Update")
    ),
    uiGoogle,
    uiJel1,uiJel2,uiJel3,
    bsCollapse(bsCollapsePanel(id="advancedFilterCollapse",title="Advanced filters",
      uiMaxArticles,
      uiJournal,
      uiDateRange,
      uiWithTags,
      uiWithoutTags        
    )),
    uiSortBy
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


shiny.articles.html = function(adt=app$adt, app=getApp()) {
  restore.point("shiny.articles.html")
  if (!isTRUE(app$opt$edit)) {
    return(articles.html(d = adt))
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


