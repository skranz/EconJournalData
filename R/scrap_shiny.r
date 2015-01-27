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


