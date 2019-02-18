examples.my.search = function() {
  setwd("D:/libraries/EconJournalData/")
  init.ejd() 
  db = get.articles.db()
  articles = dbGet(db,"article") %>% filter(has_data)
  abstracts = tolower(articles$abstract)
  
  query = '"social preferences" experiment reciprocity'
  query = 'pollution cars vehicles'
  query = 'electri'
  query = 'price pricing'

  res = abstracts.keyword.search(query, articles, abstracts)
  
  
  
  ?grepl
  library(stringr)
  res = str_count(abstracts, fixed(keyword))
  table(res)
}

abstracts.keyword.search = function(query, articles, contents=tolower(paste0(articles$title," ",articles$abstract))) {
  restore.point("abstracts.keyword.search")
  keywords = query.to.keywords(tolower(query))

  n = NROW(articles)
  counts.mat = matrix(0,n,length(keywords))
  for (w in seq_along(keywords)) {
    counts.mat[,w] = str_count(contents, fixed(keywords[w]))
  }
  counts.mat[is.na(counts.mat)] = 0
  
  num.match = rowSums(counts.mat>0)
  #table(num.match)
  
  sum.match = rowSums(counts.mat)
  table(sum.match)
  ord = order(-num.match, -sum.match,-articles$year)
  max.hit = sum(num.match>0)
  
  res = articles[ord[seq_len(max.hit)],]
  res
}

query.to.keywords = function(query) {
  
  #query = '"social preferences" experiment'
  keywords = strsplit(query,'"', fixed=TRUE)[[1]]
  if (length(keywords)>1) {
    fixed.ind = seq(2,length(keywords), by=2)
    fixed = keywords[fixed.ind]
    query = trimws(paste0(trimws(keywords[-fixed.ind]),collapse=" "))
  } else {
    fixed = NULL
  }
  keywords = strsplit(query," ", fixed=TRUE)[[1]] %>% trimws
  c(fixed,keywords)
  
}
