examples.correct.data.url = function() {
  setwd("D:/libraries/EconJournalData/")
  init.journal.scrapper() 
  db = get.articles.db()
  df = dbGet(db,"article")
  "https://www.aeaweb.orghttps://www.aeaweb.org/aej/mac/data/2011-0150_data.zip"
  rows = which(str.starts.with(df$data_url,"https://www.aeaweb.orghttps://www.aeaweb.org"))
  rhs = str.right.of(df$data_url[rows],"https://www.aeaweb.orghttps://www.aeaweb.org")
  rhs.rows = which(!str.starts.with(rhs,"/"))
  df$data_url[rows] = paste0("https://www.aeaweb.org",rhs)
  
  dbWithTransaction(db,{
    dbDelete(db,"article", params=list())
    dbInsert(db,"article", df)
  })

}

examples.scrap.aea.journal = function() {
  
  setwd("D:/libraries/EconJournalData/")
  init.journal.scrapper()
  journ="aer"
  get.existing.issues(journ)
  year = 2018
  scrap.aea.journal(journ="aejmac", vol=4, issue=3, artnum=NULL, ignore.existing.issues = FALSE)
  
  # Scrap all journals
  scrap.aea.journal(journ="jep", ignore.existing.issues = TRUE)
  
}


scrap.aea.journal = function(journ="aer", vol=NULL, issue=NULL, artnum=NULL, year=NULL, ji=if(length(journ)==1) get.ji(journ) else NULL, ignore.existing.issues=!(is.null(vol) & !is.null(issue)), first.vol = ji$first_vol) {
  
  if (is.null(journ)) {
    journ = c("aer","aejpol", "aejapp","aejmic","aejmac","jpe")
  }
  if (length(journ)>1) {
    for (j in journ) {
      cat("\n********************************\n    ", j, "\n********************************\n")
      scrap.aea.journal(journ=j, vol=vol, issue=issue, artnum=artnum, year=year, ignore.existing.issues = ignore.existing.issues)
    }
    return()
  }
  restore.point("scrap.aea.journal")
  
  if (!is.null(year)) {
    vol = vol.of.year(year = year,journ=journ)
  }

  ihtml = read_html(ji$start_url)
  issue_links = html_nodes(ihtml,"article a") %>%
    html_attr("href")
  issue_labels = tolower(html_nodes(ihtml,"article a") %>% html_text())
  vols = as.integer(str.between(issue_labels, "vol. ",","))
  issues = suppressWarnings(as.integer(str.between(issue_labels, "no."," )")))
  issues[is.na(issues)] = suppressWarnings(as.integer(str.between(issue_labels, "no.","-"))[is.na(issues)])
  
  
  
  
  rows = rep(TRUE, NROW(vols))
  if (!is.null(vol)) {
    rows = rows & vols %in% vol 
  }
  if (!is.null(first.vol)) {
    rows = rows & vols >= first.vol
  }
  if (!is.null(issue)) {
    rows = rows & issues %in% issue
  }
  if (ignore.existing.issues) {
    exist.df = get.existing.issues(journ=journ)
    if (NROW(exist.df)>0) {
      e_vol_issue = paste0(exist.df$vol, "-", exist.df$issue)
      vol_issue = paste0(vols,"-", issues)
      rows = rows & !(vol_issue %in% e_vol_issue)
    }
  }
  
  
  rows = which(rows)
  cat("\nScrap ", length(rows), " issues...\n")
  for (ind in rows) {
    cat("\n", issue_labels[ind]," ")
    scrap.aea.issue(paste0(ji$base_url,issue_links[ind]), journ=journ, ji=ji, artnum=artnum)
  }
}

scrap.aea.issue = function(issue_link, journ, db=get.articles.db(), ji=get.ji(journ), skip.front.matter = 1, artnum=NULL) {
  restore.point("scrap.aea.issue")
  
  html = read_html(issue_link)
  article_links = html_nodes(html,".journal-article a") %>% html_attr("href")
  inds = setdiff(seq_along(article_links), seq_len(skip.front.matter))
  article_links = article_links[inds]
  artnums = seq_along(article_links)
  if (!is.null(artnum))
    artnums = intersect(artnums, artnum)
  for (cur.artnum in artnums) {
    cat(".")
    article_link = article_links[cur.artnum]
    aurl = paste0(ji$base_url, article_link)
    ahtml = read_html(aurl)
    try({
      art = scrap.aea.article.page(ahtml,artnum=cur.artnum, url=aurl, journ=journ, base_url = ji$base_url)
      if (!is.null(art))
        insert.aea.article(art, db=db)
    })
  }
}

insert.aea.article = function(art, db=get.articles.db()) {
  restore.point("insert.aea.article")
  dbWithTransaction(db, {
    res = dbInsert(db, "article",art, mode="replace")
    
    if (length(art$authors)>0) {
      df = data.frame(id=art$id, author = art$author)
      dbDelete(db,"author",params=list(id=art$id))
      res = dbInsert(db, "author",df)
    }
    if (length(art$jel)>0) {
      jel = art$jel
      df = data.frame(id=art$id, jel = jel, jel2 = substring(jel,1,2), jel1 = substring(jel,1,1))
      dbDelete(db,"jel",params=list(id=art$id))
      res = dbInsert(db, "jel",df)
    }
      
  })
}

scrap.aea.article.page = function(html,artnum,url, journ="aer", base_url="", ignore.zero.authors=FALSE) {
  restore.point("scrap.aea.article.page")
  

  title = html_node(html, ".title") %>% html_text()
  authors = html_nodes(html, ".attribution .author") %>% html_text(trim=TRUE)
  
  if (length(authors)==0 & ignore.zero.authors) {
    return(NULL)
  }
  
  
  str = (html_nodes(html, "li.journal") %>% html_text(trim=TRUE))[2]
  vol = as.integer(str.between(str, "vol. ",","))
  issue = suppressWarnings(as.integer(str.between(str, "no. ",",")))
  if (is.na(issue)) issue = as.integer(str.between(str, "no. ","-"))
  
  abstract = html_node(html, ".abstract") %>% html_text(trim=TRUE)
  
  jel = html_nodes(html, ".jel-codes .code") %>% html_text(trim=TRUE)
  
  # Extract Dataset URL and Size
  materials = html_nodes(html, ".additional-materials a")
  links = materials %>% html_attr("href")
  labels = materials %>% html_text(trim=TRUE)
  
  is_data = str.ends.with(links, ".data") | has.substr(tolower(labels),"data")
  has_data = sum(is_data)>0
  if (has_data) {
    links  = links[is_data][1]
    labels = labels[is_data][1]
    
    # Relative or absolute link?
    if (!str.starts.with(links,"https://")) {
      rows = !str.starts.with(links,"/")
      links[rows] = paste0("/",links)
      
      data_url = paste0(base_url, links)
    } else {
      data_url = links
    }
    size_str = str.between(labels,"(",")")
    
    unit = str.right.of(size_str," ")
    size = as.numeric(str.left.of(size_str," "))
    if (isTRUE(unit =="KB")) {
      unit = "MB"
      size = size / 1000
    } else if (isTRUE(unit)=="GB") {
      unit = "MB"
      size = size * 1000
    }
  } else {
    size_str = ""
    data_url = ""
    size = 0
    unit = "MB"
  }
  
  id = paste0(journ, "_", vol, "_", issue, "_", artnum)
  
  art = list(id=id, journ=journ, vol=as.integer(vol), issue=as.integer(issue), title=title, artnum = artnum, article_url = url, has_data = has_data, data_url = data_url,size=size, unit=unit, files_analyzed=FALSE, jel=jel, authors = authors, abstract=abstract, num_authors=length(authors))
  art = add.article.date.info(art)
  art
}

