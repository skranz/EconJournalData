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
  
  query = 'electricity DD'

  method.count.li = readRDS("method_count.Rds")
  res = do.keyword.search(query, articles, abstracts, method.count.li=method.count.li)
  
  ?grepl
  library(stringr)
  res = str_count(abstracts, fixed(keyword))
  table(res)
}

# Allow keywords also in lower case and without quotes
adapt.Keywords.for.methods = function(Keywords, methods) {
  restore.point("adapt.Keywords.for.methods")
  if (length(Keywords)<2) {
    mkw = c(Keywords)
  } else {
    nkw = length(Keywords)
    mkw = c(Keywords, paste0(Keywords[1:(nkw-1)]," ",Keywords[2:nkw]))
  }
  #methods = tolower(methods)
  use.methods = tolower(methods) %in% tolower(mkw)
  found.methods = methods[use.methods]
  new.Keywords = Keywords[!tolower(Keywords) %in% tolower(found.methods)]
  list(Keywords = union(new.Keywords, found.methods), methods=found.methods) 
}

do.keyword.search = function(query, articles,Contents = paste0(articles$title," ",articles$title," ",articles$abstract),  contents=tolower(contents), method.count.li = getApp()$glob$method.count.li) {
  restore.point("do.keyword.search")
  Keywords = query.to.keywords(query)
  res = adapt.Keywords.for.methods(Keywords, names(method.count.li))
  Keywords = res$Keywords
  methods = res$methods
  keywords = tolower(Keywords)

  n = NROW(articles)
  counts.mat = matrix(0,n,length(keywords))
  w = 1
  for (w in seq_along(keywords)) {
    if (keywords[w]==Keywords[w]){
      counts.mat[,w] = str_count(contents, fixed(keywords[w]))
    } else {
      counts.mat[,w] = str_count(Contents, fixed(Keywords[w]))
      
      # Special handling of common method abbreviations
      # that have another match
      if (Keywords[w]=="IV") {
        counts.mat[,w] = counts.mat[,w]-str_count(Contents, fixed("HIV"))
      }
    }
  }
  counts.mat[is.na(counts.mat)] = 0
  
  # We have precomputed methodology keyword
  # counts from the full text and add it to the search
  
  if (length(methods)>0) {
    colnames(counts.mat) = Keywords
    rownames(counts.mat) = articles$id
    m = methods[[1]]
    for (m in methods) {  
      tab = method.count.li[[m]]
      # take square root of counts
      ids = intersect(tab$id,articles$id)
      
      if (length(ids)<NROW(tab)) {
        rows = match(ids, tab$id)
        counts.mat[ids,m] = counts.mat[ids,m] + sqrt(tab$count)[rows]
      } else {
        counts.mat[ids,m] = counts.mat[ids,m] + sqrt(tab$count)
      }
    }
  }
  
  num.match = rowSums(counts.mat>0)
  #table(num.match)
  
  sum.match = rowSums(counts.mat)
  #table(sum.match)
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
