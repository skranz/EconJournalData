examples.scrap.jpe = function() {
  
  setwd("D:/libraries/EconJournalData/")
  setwd("~")
  init.journal.scrapper()

  scrap.jpe(vol=123, issue=4:6)
  
  scrap.jpe()
}



scrap.jpe = function(journ="jpe", vol=NULL, issue=NULL, artnum=NULL, year=NULL, ji=if(length(journ)==1) get.ji(journ) else NULL, ignore.existing.issues=!(is.null(vol) & !is.null(issue)), first.vol = ji$first_vol, issue.df=NULL, only.missing=is.null(vol) & is.null(issue.df)) {
  
  restore.point("scrap.jpe")
  
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
    scrap.jpe.issue(journ=journ, vol=idf$vol[row], issue=idf$issue[row], artnum=artnum)
  }
}

scrap.jpe.issue = function(journ="restud", vol, issue, artnum=NULL) {
  restore.point("scrap.jpe.issue")
  urls = jpe.issue.urls(journ,vol,issue)
  rows = seq_along(urls)
  if (!is.null(artnum)) {
    rows = intersect(rows, artnum)
  }
  
  for (row in rows) {
    cat("\n....")
    art = scrap.jpe.article.page(artnum = row, url=urls[row], journ=journ, vol=vol, issue=issue)
    insert.jpe.article(art)
    # Wait to abvoid blocking
    Sys.sleep(sample(10:120,1))
    cat(" ", art$title)
  }
}

jpe.issue.urls = function(journ="jpe",vol, issue, ji=get.journal.info(journ)) {
  restore.point("jpe.issue.urls")
  
  year = year.of.vol(vol=vol, ji=ji)
  url = paste0("https://www.journals.uchicago.edu/toc/jpe/",year,"/",vol,"/",issue)
  html = read_html(url)
  all_links = html_nodes(html,".tocContent a")
  link_label = all_links %>% html_text(trim=TRUE)
  abstract_links = all_links[link_label=="Abstract"] %>% html_attr("href")
  paste0("https://www.journals.uchicago.edu", abstract_links)
}

insert.jpe.article = function(art, db=get.articles.db()) {
  insert.aea.article(art, db)
}

scrap.jpe.article.page = function(html=read_html(url),artnum,url, journ="jpe", base_url="", ignore.zero.authors=FALSE, vol=NA, issue=NA) {
  restore.point("scrap.jpe.article.page")
  
  title = html_node(html, ".publicationContentTitle h1") %>% html_text(trim=TRUE)
  authors = html_nodes(html, ".publicationContentAuthors .entryAuthor") %>% html_text(trim=TRUE)
  
  if (length(authors)==0 & ignore.zero.authors) {
    return(NULL)
  }
  
  abstract = html_node(html, ".abstractSection") %>% html_text(trim=TRUE)
  
  # Suplementary material is on an extra page
  supp_url = html_nodes(html, "a.show-supplement") %>% html_attr("href")
  
  if (length(supp_url)>0) {
    supp_url = supp_url[1]
    supp_url = paste0("https://www.journals.uchicago.edu", supp_url)
    shtml = read_html(supp_url)
    data_urls = html_nodes(shtml, ".tab-content a") %>%  html_attr("href")
    data_urls = data_urls[has.substr(tolower(data_urls), ".zip")]
    has_data = length(data_urls)>0
  } else {
    has_data = FALSE
  }
  
  if (has_data) {
    data_url = paste0("https://www.journals.uchicago.edu",data_urls, collapse="\n")
  } else {
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
