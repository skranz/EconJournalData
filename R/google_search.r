
cx.keys = function() {
  list(
    aea = "005521900964631941209:pqh2ykhk3qq",
    mitpress = "005521900964631941209:cml1wn3dqfm",
    restud = "005521900964631941209:pz9gmjmlr_k",
    aea_restud = "005521900964631941209:ehutzpzhf9u"
  )
}

articles.from.search.df = function(sdf, articles, art.titles = tolower(articles$title)) {
  restore.point("articles.from.query.items")
  mart = bind_rows(lapply(seq_len(NROW(sdf)), function(srow) {
    url = sdf$link[srow]
    row = match(url, articles$article_url)
    if (!is.na(row)) return(articles[row,])
    title = str.remove.ends(tolower(sdf$title[srow]),right = 4)
    title = str.left.of(title, " |")
    row = which(str.starts.with(art.titles, title))
    if (length(row)>0)
      return(articles[row[1],])
    return(NULL)
  }))
  mart
}

custom.article.search = function(query, pages=1,api.key, cx.key, past.years=year(Sys.Date())-2010, start.page=1) {
  restore.point("custom.article.search")
  li = lapply(seq_len(pages)+(start.page-1), function(page) {
    custom.article.search.page(query, page=page,api.key, cx.key, past.years)    
  })
  bind_rows(li)
}

custom.restat.search.page = function(query, page=1,api.key, past.years=year(Sys.Date())-2010,cx.key=cx.keys()$mitpress) {
  restore.point("custom.restat.search.page")
  query = paste0('"Review of Economics and Statistics" ', query)
  get.url = paste0("https://www.googleapis.com/customsearch/v1?key=", api.key,"&cx=", cx.key,"&dateRestrict=y",past.years, "&start=",(page-1)*10+1, "&q=",query)
  get.url = URLencode(get.url)
  res <- GET(get.url)
  restore.point("custom.restat.search.page2")
  
  it = content(res)$items
  items = lapply(content(res)$items, function(item) list(title=item$title, link=item$link))
  
  sdf = bind_rows(items)
  sdf
}

custom.article.search.page = function(query, page=1,api.key, cx.key, past.years=year(Sys.Date())-2010) {
  restore.point("custom.article.search.page")
  
  get.url = paste0("https://www.googleapis.com/customsearch/v1?key=", api.key,"&cx=", cx.key,"&dateRestrict=y",past.years, "&start=",(page-1)*10+1, "&q=",query)
  get.url = URLencode(get.url)
  res <- GET(get.url)
  items = lapply(content(res)$items, function(item) list(title=item$title, link=item$link))
  
  sdf = bind_rows(items)
  sdf
}
