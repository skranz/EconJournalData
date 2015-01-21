library(shiny)
library(shinyBS)
library(EconJournalData)
setwd("D:/libraries/EconJournalData")
init.journal.scrapper()

jel = jel.codes
adt = read.complete.data()
adt$code.str = make.code.str(adt)
adt = adt[order(-adt$data.size, adt$has.data),]
rows = is.na(adt$data.size) | adt$data.size >0
adt = adt[rows,]
dt = adt



ajel.dt = read.articles.jel.csv()
setkey(ajel.dt, jel)



names(jel)
shinyServer(function(input, output, session) {

  restore.point("shiny Server")
  addResourcePath("zipdata", "D:/libraries/EconJournalData/zipdata")
  
  r.all_data <- reactive({
    cat("Hi all_data")   
    cat(input[["all_data"]])
    input$all_data
  })
  
  r.journals <- reactive(input$journalsInput)
  r.jel1 <- reactive({
    input$jel1Input
  })
  
  # Update articles
  r.dt <- reactive({
    cat("\nfilter.articles()")
    input$searchButton
    isolate(filter.articles(adt,ajel.dt,journals=r.journals(),jel1=r.jel1()))
  })
  
  output$buttonState <- renderText({
    cat("Hi!!!!")
    #checked = isolate(names(reactiveValuesToList(input)))
    print(input$all_data)
    paste0("Updated: ",input$updateButton )   
  })
  
  output$articlesHtml <- renderUI({
    cat("\nshiny.articles.html()")
    #HTML(r.commentUpdate())
    HTML(shiny.articles.html(r.dt()))

  })

})


#runApp("D:/libraries/ExpEconDB/ExpEconDB/inst/shiny")
#runExample("07_widgets")
#bsDemo()
