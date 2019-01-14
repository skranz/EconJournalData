examples.oxf.issues.urls = function() {
  setwd("D:/libraries/EconJournalData/")
  init.journal.scrapper()
  ji = get.ji("restud")
  res = find.missing.issues("restud")
  scrap.oxf.journal(vol=81, issue=1, artnum=2)

  scrap.oxf.journal(vol=81)
  
  vol = 81
  issue = 1
  urls = oxf.issue.urls(vol=vol,issue=issue)
  
  
  url = urls[2]
  html = read_html(url)
  scrap.restud.article.page(html,url=url,artnum = 1)
  
  
}

scrap.oxf.journal = function(journ="restud", vol=NULL, issue=NULL, artnum=NULL, year=NULL, ji=if(length(journ)==1) get.ji(journ) else NULL, ignore.existing.issues=!(is.null(vol) & !is.null(issue)), first.vol = ji$first_vol, only.missing=is.null(vol) & is.null(issue.df), issue.df=NULL) {
  
  
  if (is.null(journ)) {
    journ = c("restud")
  }
  if (length(journ)>1) {
    for (j in journ) {
      cat("\n********************************\n    ", j, "\n********************************\n")
      scrap.oxf.journal(journ=j, vol=vol, issue=issue, artnum=artnum, year=year, ignore.existing.issues = ignore.existing.issues)
    }
    return()
  }
  restore.point("scrap.oxf.journal")
  if (!is.null(year)) {
    vol = vol.of.year(year = year,journ=journ)
  }
  if (!is.null(issue.df)) {
    idf = issue.df
  } else if (only.missing) {
    idf = find.missing.issues(journ) 
  } else {
    idf = ji$issue.dt
  }

  if (!is.null(vol)) {
    idf = idf[idf$vol %in% vol,]
  }
  if (!is.null(first.vol)) {
    idf = idf[idf$vol >= first.vol,]
  }
  if (!is.null(issue)) {
    idf = idf[idf$issue %in% issue,]
  }

  rows = seq_len(NROW(idf))
  cat("\nScrap ", length(rows), journ, " issues...\n")
  for (row in rows) {
    cat(paste0("\n vol ",idf$vol[row], " issue ", idf$issue[row]," "))
    scrap.oxf.issue(journ=journ, vol=idf$vol[row], issue=idf$issue[row], artnum=artnum)
  }
}


scrap.oxf.issue = function(journ="restud", vol, issue, artnum=NULL) {
  restore.point("scrap.oxf.issue")
  urls = oxf.issue.urls(journ,vol,issue)
  rows = seq_along(urls)
  if (!is.null(artnum)) {
    rows = intersect(rows, artnum)
  }
  
  for (row in rows) {
    cat("\n....")
    art = scrap.oxf.article.page(artnum = row, url=urls[row], journ=journ, vol=vol, issue=issue)
    insert.oxf.article(art)
    cat(" ", art$title)
  }
}

oxf.issue.urls = function(journ="restud",vol, issue, update=FALSE, ji=get.journal.info(journ)) {
  restore.point("oxf.issue.urls")
  
  html.root = ji$html_root
  url = paste0(html.root,"/",vol,"/",issue)
  html = read_html(url)
  article_links = html_nodes(html,".al-article-items h5 a") %>%
    html_attr("href")
  paste0("https://academic.oup.com", article_links)
}

insert.oxf.article = function(art, db=get.articles.db()) {
  insert.aea.article(art, db)
}

scrap.oxf.article.page = function(html=read_html(url),artnum,url, journ="restud", base_url="", ignore.zero.authors=FALSE, vol=NA, issue=NA) {
  restore.point("scrap.oxf.article.page")
  
  title = html_node(html, "h1.article-title-main") %>% html_text(trim=TRUE)
  authors = html_nodes(html, ".al-authors-list a.linked-name") %>% html_text(trim=TRUE)
  
  if (length(authors)==0 & ignore.zero.authors) {
    return(NULL)
  }
  
  abstract = html_node(html, "section.abstract") %>% html_text(trim=TRUE)
  
  # Extract Dataset URL and Size
  data_url = html_nodes(html, ".dataSuppLink a") %>% html_attr("href")
  has_data = length(data_url)>0
  data_url = paste0(data_url, collapse="\n")
  
  if (!has_data) {
    data_url = ""
  }
  size_str = ""
  size = NA
  unit = "MB"
  
  jel = NULL
  id = paste0(journ, "_", vol, "_", issue, "_", artnum)
  
  art = list(id=id, journ=journ, vol=as.integer(vol), issue=as.integer(issue), title=title, artnum = artnum, article_url = url, has_data = has_data, data_url = data_url,size=size, unit=unit, files_analyzed=FALSE, jel=jel, authors = authors, abstract=abstract, num_authors=length(authors))
  art = add.article.date.info(art)
  art
}


