examples.update.all = function() {
  setwd("D:/libraries/EconJournalData/")
  init.journal.scrapper()

  # Scrap all journals
  scrap.aea.journal(journ="aer", ignore.existing.issues = TRUE)
  scrap.aea.journal(journ="jep", ignore.existing.issues = TRUE)
  scrap.aea.journal(journ="aejmic", ignore.existing.issues = TRUE)
  scrap.aea.journal(journ="aejmac", ignore.existing.issues = TRUE)
  scrap.aea.journal(journ="aejpol", ignore.existing.issues = TRUE)
  scrap.aea.journal(journ="aejapp", ignore.existing.issues = TRUE)

  issue.df = find.missing.issues("restud") %>%
    filter(year < 2018) # Latest years are not always available
  scrap.oxf.journal(journ="restud", issue.df = issue.df)

  update.dataverse.journal("restat")
  update.dataverse.journal("qje")
  update.dataverse.journal("jaere")

  # Download article ZIPs
  download.newest.articles.zips(max_articles=1, max_mb=1000, wait.between=0)
  
  # Summarize file infos
  ids = articles.with.unsummarized.files()
  update.files.summary(ids)

}