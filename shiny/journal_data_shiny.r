
uiArticleSelectors = function(dt) {

  restore.point("uiArticleSelectors")
  
  journals = names(jis)
  jel.codes = setdiff(LETTERS,c("S","T","U","V","W","X"))

  li = as.list(journals); names(li) = journals
  uiJournal = selectizeInput("journInput", "Journals:",li,selected=li, multiple=TRUE,
    options = list(
      maxItems=100
    )
  )
  

  return(verticalLayout(
      uiJournal
  ))
}

