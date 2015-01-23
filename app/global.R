library(shinyEvents)
library(shinyBS)
library(EconJournalData)

# setwd("D:/libraries/EconJournalData/EconJournalData/app")

# set.restore.point.options(display.restore.point = TRUE)

init.journal.scrapper(base.dir = "./base", base.data.dir="./base")
app = articlesApp()

#runEventsApp(app)
