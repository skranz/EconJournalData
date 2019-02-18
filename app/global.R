
library(shinyEvents)
library(shinyBS)
library(EconJournalData)
library(RCurl)
library(XML)
# setwd("D:/libraries/EconJournalData/EconJournalData/app")
init.journal.scrapper()
app = articlesApp()

#runEventsApp(app)
