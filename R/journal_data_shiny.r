filter.articles = function(adt,ajel.dt,journals=NULL,jel1=NULL,...) {
  restore.point("filter.articles")
  dt = as.data.table(adt)
  changed=FALSE
  if (!is.null(journals)) {
    if (length(journals)<length(jis)) {
      setkey(dt,journal)
      dt = dt[journals]
      changed=TRUE
    }
  }
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


uiArticleSelectors = function(jel) {

  restore.point("uiArticleSelectors")
  cat("uiArticleSelectors")
  journals = names(jis)
  jel.codes = setdiff(LETTERS,c("S","T","U","V","W","X"))

  li = as.list(journals); names(li) = journals
  uiJournal = selectizeInput("journalsInput", "Journals:",li,selected=li, multiple=TRUE,
    options = list(
      maxItems=100
    )
  )


  jel1 = jel[jel$digits==1,] 
  jel1 = jel[order(jel$digits,jel$code),]
  li = as.list(jel1$code); names(li) = jel1$name
  uiJel1 = selectizeInput("jel1Input", "JEL:",li,selected="L", multiple=TRUE,
    options = list(
      maxItems=30
    )
  )

  uiDateRange = dateRangeInput("daterange", "Date range:",
               start = "2000-01-01",
               end   = NULL,
               format= "mm/yyyy")

  tags = read.csv(paste0(main.dir,"/tags.csv"))
  args = lapply(1:NROW(tags), function(i) {
    bsButton(paste0("with_",tags$code[i],"_Input"), label=tags$label[i], value=tags$code[i])
  })
  uiWithTags=do.call(bsButtonGroup, c(list(inputId="withTagsInput", label = "with tag", toggle = "checkbox", size="small"), args))
  
  args = lapply(1:NROW(tags), function(i) {
    bsButton(paste0("without_",tags$code[i],"_Input"), label=tags$label[i], value=tags$code[i])
  })
  uiWithoutTags=do.call(bsButtonGroup, c(list(inputId="withoutTagsInput", label = "without  tag", toggle = "checkbox", size="small"), args))

  
  return(verticalLayout(
    uiJournal,
    uiJel1,
    uiDateRange,
    uiWithTags,
    uiWithoutTags,
    fluidRow(
      HTML("<BR>"),
      actionButton("searchButton","Search"),
      actionButton("updateButton","Update"),
      textOutput("buttonState")
    )
    
  ))
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


shiny.articles.html = function(dt, file="articles.html") {
  restore.point("make.html")
  d = dt
  
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


