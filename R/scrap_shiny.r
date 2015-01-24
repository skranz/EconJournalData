examples.scrapApp = function() {
  library(shinyEvents)
  library(shinyBS)
  library(EconJournalData)
  
  set.restore.point.options(display.restore.point = TRUE)
  init.journal.scrapper()
  app = scrapApp()
  runEventsApp(app)
}

scrapApp = function() {
  library(shinyEvents)
  library(shinyBS)
  library(dplyrExtras)
    
  app = eventsApp()
  
  app$dt = as.data.frame(read.complete.data())
  
  app$opt = list(
  )
    
  app$ui = shinyUI(navbarPage("Scrap Data",
    tabPanel("Articles", sidebarLayout(
      sidebarPanel(
        uiScrapSelectors()
      ),
      mainPanel(
        uiOutput("scrapHtml") 
      )
    ))
  ))
  app
}


uiScrapSelectors = function(app=getApp()) {
  restore.point("uiScrapSelectors")
  cat("uiArticleSelectors")
  ui = NULL
  return(ui)
}

gaps.string = function(x, by=1, start=NULL, end=NULL) {
  if (length(x)==0) return("")
  x.seq = seq(min(c(x,start)), max(c(x,end)),by=by )
  x.mis = setdiff(x.seq,x)
  if (length(x.mis)==0) return("")
  return(paste0(x.mis,collapse=","))
}


gaps.list = function(x, by=1, start=NULL, end=NULL) {
  if (length(x)==0) return("")
  x.seq = seq(min(c(x,start)), max(c(x,end)),by=by )
  x.mis = setdiff(x.seq,x)
  list(x.mis)
}



scrap.info = function(dt=read.complete.data()) {
  dt = as.data.frame(dt)
  dt = arrange(dt, journ, vol, issue, articleNum)
  j.dt = summarise(group_by(dt,journal),
    num.art = NROW(id),
    num.vol = NROW(unique(vol)),
    first.vol = min(vol),
    last.vol = max(vol),
    first.year = min(year),
    last.year = max(year),
    missing.vol = gaps.string(vol),
    art.has.data = sum(is.true(data.size>0)),
    art.na.data = sum(is.na(data.size)),
    has.data.share = round(art.has.data / num.art,3)*100
  )
  
  jv.dt =  summarise(group_by(dt,journal,year,vol),
    num.art = NROW(id),
    num.issues = NROW(unique(issue)),
    missing.issue = gaps.string(issue,start=1),
    art.has.data = sum(is.true(data.size>0)),
    art.na.data = sum(is.na(data.size)),
    has.data.share = round(art.has.data / num.art,3)*100
  )
  
  # Find missing articles  
  d =  summarise(group_by(dt,journal,year,vol,issue),
    num.art = NROW(id),
    missing.art = gaps.list(articleNum,start=1)
  )
  missing.art = unnest(d,missing.art)

  # Find missing issues
  jis
  
}
