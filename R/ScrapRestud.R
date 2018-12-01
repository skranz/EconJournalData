examples.restud.issues.urls = function() {
  init.journal.scrapper()
  vol = 81
  issue = 1
  restud.issue.urls(vol=vol,issue=issue)

}

restud.issue.urls = function(journ="restud",vol, issue, update=FALSE, ji=get.journal.info(journ)) {
  restore.point("restud.issue.urls")
  
  html.root = "http://restud.oxfordjournals.org/content"
  url = paste0(html.root,"/",vol,"/",issue)
  txt = try(readLines(url), silent=TRUE)
  if (is(txt,"try-error")) {
    cat("\nDid not find ",journ," vol",vol, "issue",issue, " in the web.")
    return(NULL)
  }

  if (length(txt)<10) {
    return(NULL)
  }
  

  rows = which(str.starts.with(str.trim(txt),'<h4 class="cit-title-group">'))
  str = str.trim(txt[rows])
  str = str.right.of(str,'<span class="cit-first-page">')
  str = as.numeric(str.left.of(str,'<'))
  
  urls = paste0(url,"/",str)
  urls
}

examples.parse.restud.article = function() {
  vol = 81
  issue = 1
  articleNum = 2
  
  #http://restud.oxfordjournals.org/content/81/1/137
  urls = restud.issue.urls(vol=vol, issue=issue)
 
  url = urls[articleNum]
  d = nlist(vol,issue, url, articleNum)
  d.ind = d

    
  d = parse.restud.article(d.ind)

  file = "D:/libraries/EconJournalData/scrap_html/Effects of Banning Advertising in Junk Food Markets _ The Review of Economic Studies _ Oxford Academic.html"
  html = readLines(file, warn=FALSE)
  parse.restud.article(html)
}

scrap.restud.issue = function(vol, issue, artnum=NULL) {
  restore.point("scrap.restud.issue")
  
  journ="restud"
  article_urls = restud.issue.urls(vol=vol, issue=issue)
  if (length(article_urls)==0) {
    cat(paste0("\nCould not find ReStud vol ", vol, " issue", issue))
    return(NULL)
  }
  
  artnums = seq_along(article_urls)
  if (!is.null(artnum))
    artnums = intersect(artnums, artnum)
  for (cur.artnum in artnums) {
    cat(".")
    aurl = article_urls[[cur.artnum]]
    ahtml = read_html(arl)
    try({
      art = scrap.restud.article.page(ahtml,artnum=cur.artnum, url=aurl, journ=journ, vol=vol, issue=issue)
      if (!is.null(art))
        insert.aea.article(art, db=db)
    })
  }
}


scrap.restud.article.page = function(html, url=NULL, vol=NULL, issue=NULL, artnum=NULL) {
  restore.point("parse.restud.article")
  journ = "restud"

  title = html_node(html, ".article-title-main") %>% html_text(trim = TRUE)
  authors = html_nodes(html, ".wi-authors a.linked-name") %>% html_text(trim=TRUE)

  # Volume and Issue
  str = (html_nodes(html, ".ww-citation-primary") %>% html_text(trim=TRUE))
  if (is.null(vol))
    vol = as.integer(str.between(str, "Volume ",","))
  if (is.null(issue))
    issue = as.integer(str.between(str, "Issue ",","))

  abstract = html_node(html, "section.abstract") %>% html_text(trim=TRUE)

  jel.str = html_nodes(html, ".article-metadata-taxonomies a") %>% html_text(trim=TRUE)
  jel = trimws(str.left.of(jel.str,"-"))
  
  # Extract Dataset URL
  data_url = html_node(html, ".dataSuppLink a") %>% html_attr("href")
  has_data = is.na(data_url)
  if (!has_data) {
    data_url = ""
  }
  size=NA
  unit=""

  id = paste0(journ, "_", vol, "_", issue, "_", artnum)
  
  art = list(id=id, journ=journ, vol=as.integer(vol), issue=as.integer(issue), title=title, artnum = artnum, article_url = url, has_data = has_data, data_url = data_url,size=size, unit=unit, files_analyzed=FALSE, jel=jel, authors = authors, abstract=abstract, num_authors=length(authors))
  art = add.article.date.info(art)
  art

}



parse.restud.article.old = function(d) {
  restore.point("parse.restud.article")
  
  d$htmlFile = paste0(html.dir,"/restud_vol_",d$vol,"_issue_",d$issue,"_article_",d$articleNum,".html")
  d = download.scrap.html(d)
  txt = readLines(d$htmlFile)
  
  if (length(txt)<20) {
    d$exists = FALSE
    return(d)
  }
  
  
  # Find title
  #<title>Contractual Incompleteness, Unemployment, and Labour Market Segmentation </title>
  txt = str.trim(txt)
  
  row = which(str.starts.with(txt,'<title>'))[1]
  str = txt[row]
  str = str.right.of(str,'<title>')
  str = str.trim(str.left.of(str,'</title>'))
  d$title = str
  
  d$article.url = paste0(d$url,".full.pdf")
  
  # Find link of data set
  dc = list(
    url = paste0(d$url,"/suppl/DC1"),
    htmlFile =  paste0(html.dir,"/restud_dc_vol_",d$vol,"_issue_",d$issue,"_article_",d$articleNum,".html")
  )
  dc = download.scrap.html(dc)
  dc.txt =  readLines(dc$htmlFile)
  
  #<li><a href="/content/suppl/2013/10/22/rdt034.DC1/Supplementarydata.zip">Supplementary Data</a>  
  #row = which(has.substr(dc.txt,"Supplementarydata.zip"))
  row = which(has.substr(dc.txt,'.zip"'))
  if (length(row)>0) {
    row = row[1]
    str = str.trim(dc.txt[row])
    str = str.right.of(str,'href="/content/')
    str = str.left.of(str,'">')    
    d$data.url = paste0("http://restud.oxfordjournals.org/content/",str)
    d$has.data = TRUE
    d$data.size  = NA
    d$data.unit = NA        
  } else {
    d$data.url = ""
    d$has.data = FALSE
    d$data.size = 0
    d$data.unit = "MB"        
  }
  
  # Find JEL Classifications
  #<li class="kwd"><span><a class="kwd-jel-link" href="/cgi/collection/c91">C91</a></span></li>
  rows = which(str.starts.with(txt,'<li class="kwd"><span><a class="kwd-jel-link"'))
  str = txt[rows]
  str = str.right.of(str,'<li class="kwd"><span><a class="kwd-jel-link"')
  str = str.right.of(str, '">')
  str = str.left.of(str, '</a>')  
  
  for (i in 1:7) {
    if (i > length(str)) {
      d[[paste0("JEL",i)]] = ""
    } else {
      d[[paste0("JEL",i)]] = str[i]      
    }
  }
  
  # Find Keywords  
  #<li class="kwd"><span><a class="kwd-search"
  #href="/search?fulltext=Contractual+incompleteness&amp;sortspec=date&amp;submit=Submit&amp;andorexactfulltext=phrase">Contractual incompleteness</a></span></li>
    
  rows = which(str.starts.with(txt,'<li class="kwd"><span><a class="kwd-search"'))
  if (length(rows)>0) {
    rows = rows +1
    
    str = txt[rows]
    str = str.right.of(str, '">')
    str = str.left.of(str, '</a>')  
    d$keywords = paste0(str, collapse=";")    
  } else {
    d$keywords = ""
  }
  d$exists = TRUE
  d  
}

