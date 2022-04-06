fulltext.keyword.counts = function(articles, Keywords, counts.mat=NULL, method.count.li=NULL) {
  methods = intersect(names(method.count.li), Keywords)
  if (length(methods)==0) return(counts.mat)
  
  counts = rep(0, NROW(articles))
  names(counts) = articles$id
  
  m = methods[1]

  for (m in methods) {  
    tab = method.count.li[[m]]
    counts[tab$id] = counts[tab$id] + tab$count
  }
  counts
}

methods.help.table = function() {
  methods = jsonlite::fromJSON(readLines(warn = FALSE,system.file("base/methods.json", package="EconJournalData")))

  pattern = sapply(methods, function(x) paste0(x,collapse=", "))
  tab = tibble(keyword=names(methods), pattern=pattern) %>% arrange(keyword)
  html = paste0(
'<tr><td><span class="mtag">',tab$keyword,'</span></td><td>',tab$pattern,'</td></tr>', collapse="\n"
  )
  writeClipboard(html)
}


methods.study = function() {
  counts = readRDS("arts_with_counts.Rds")
 
  sum = counts %>%
    summarize_all(~sum(.>1))
  sv = as.vector(as.matrix(sum[1,-1]))
  names(sv) = colnames(sum)[-1]
  
  sort(sv)
  
  ycounts = counts
  ycounts$year = dat$year
  
  byyear = ycounts %>%
    group_by(year) %>%
    summarize_all(~ round(100*mean(sum(.>1)) / length(.),2) )

  yjcounts = ycounts
  yjcounts$journ = dat$journ
  
  byjournyear = yjcounts %>%
    group_by(journ,year) %>%
    summarize_all(~ round(100*mean(sum(.>1)) / length(.),2) )
  ggplot(byjournyear, aes(x=year, y=`p-value`)) + 
    geom_line() +
    geom_line(aes(y=`confidence interval`), col="blue") +
    facet_wrap(~journ)

}

make.method.counts.rds = function(min.count=2, dir = getwd()) {
  setwd(dir)
  
  counts = readRDS("arts_with_counts.Rds")
  cols = colnames(counts)[-1]
  lcounts = tidyr::pivot_longer(counts, -id, names_to="col", values_to = "count")
  
  methods = jsonlite::fromJSON(readLines(warn = FALSE,system.file("base/methods.json", package="EconJournalData")))
  mdf = tibble(key=names(methods), col=methods) %>% unnest(col)
  
  df = left_join(lcounts, mdf, by="col")
  
  key.df = df %>% 
    group_by(id,key) %>%
    summarize(
      count = sum(count)
    ) %>%
    filter(count >0) %>%
    na.omit()

  keys = unique(key.df$key)
  
  method.tabs = lapply(keys, function(key) {
    .key = key
    filter(key.df, key==.key) %>% arrange(-count) %>% select(-key)
  })
  names(method.tabs) = keys

  saveRDS(method.tabs, "method_count.Rds")
  
  temp = key.df %>%
    ungroup() %>%
    mutate(total_art = n_distinct(id)) %>%
    group_by(key) %>%
    summarize(
      num_art = n(),
      share_art = num_art / first(total_art),
      mean_phrases = sum(count) / sum(count >0),
      num_mentions = sum(count),
    )
  temp$phrases = methods[temp$key]
  saveRDS(temp,"method_key_count.Rds")
  
  key.str.df = key.df %>%
    group_by(id) %>%
    arrange(-count) %>%
    filter(count > 1) %>%
    summarize(method.count.str = paste0("<span class='mtag'>", key,"</span><span class='mcount'> (", count,")</span>", collapse=", "))
  
  saveRDS(key.str.df, "method_str.Rds")
}