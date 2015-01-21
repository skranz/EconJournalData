library(shiny)
library(shinyBS)
library(EconJournalData)
setwd("D:/libraries/EconJournalData")
init.journal.scrapper()
jel = jel.codes
#jel = jel[jel$digit <= 2,]

shinyUI(navbarPage("Reproducible Econonomics",
  tabPanel("Articles", sidebarLayout(
    sidebarPanel(
      uiArticleSelectors(jel)
    ),
    mainPanel(
      uiOutput("articlesHtml") 
    )
  ))
))

#runApp("D:/libraries/EconJournalData/EconJournalData/shiny")