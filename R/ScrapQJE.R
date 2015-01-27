
examples.parse.qje.volume = function() {
  parse.qje.volume(128,4,1)
  
  vols = rev(124:128)
  for (vol in vols) {
    parse.qje.volume(vol)
  }
}

parse.qje.volume = function(vol=81,issues = 1:7, articles=1:100) {
  restore.point("parse.qje.volume")
  
  issue = 1
  articleNum = 1
  li = NULL
  
  counter = 1
  page.ind = 1
  for (issue in issues) {
    urls = qje.issue.urls(vol=vol,issue=issue)
    urls = unique(urls)
    for (articleNum in intersect(seq_along(urls),articles)) {
      
      d.ind = list(journ="qje",issue=issue, vol=vol, issue=issue, articleNum=articleNum, url=urls[articleNum])    
      d = parse.qje.article(d.ind)
      cat(paste0("\nvol ", vol, " issue ", issue, " exists: ",d$exist,": ", d$url))
      
      if (d$exists) {
        li[[counter]] = d
        counter = counter+1
        cat("\n",d$title," (",d$data.size, " ", d$data.unit,")\n")
      }
    }
  }
  dt = parsed.articles.li.to.table(li)
  if (write.csv)
    write.journal.vol.csv(dt, journ=journ, vol=vol)
  dt
}


vol = 103; issue = 1
qje.issue.urls = function(vol, issue, update=FALSE) {
  restore.point("qje.issue.urls")
  
  html.root = "http://qje.oxfordjournals.org/content"
  url = paste0(html.root,"/",vol,"/",issue)
  htmlFile = paste0(html.dir,"/qje_vol_",vol,"_issue_",issue,".html")
  if ( (!update) & file.exists(htmlFile)) {
    txt = readLines(htmlFile)
  } else {
    txt = "No issue found"
    tryCatch(txt <- readLines(url),
             error=function(e) {}
    )
    writeLines(txt,htmlFile)    
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

examples.parse.qje.article = function() {
  vol = 81
  issue = 1
  articleNum = 2
  urls = qje.issue.urls(vol=vol, issue=issue)
 
  url = urls[articleNum]
  d = nlist(vol,issue, url, articleNum)
  d.ind = d
  
  d = parse.qje.article(d.ind)

}

parse.qje.article = function(d) {
  restore.point("parse.qje.article")
  
  d$htmlFile = paste0(html.dir,"/qje_vol_",d$vol,"_issue_",d$issue,"_article_",d$articleNum,".html")
  d = download.scrap.html(d)
  txt = readLines(d$htmlFile)
  
  if (length(txt)<20) {
    d$exists = FALSE
    return(d)
  }
  
  
  # Find title
  #<title>Contractual Incompleteness, Unemployment, and Labour Market Segmentation </title>
  txt = str.trim(txt)
  
  row = which(has.substr(txt,'name="DC.Title"'))[1]
  str = txt[row]
  str = str.right.of(str,'content="')
  str = str.trim(str.left.of(str,'"'))
  d$title = str
  
  # Find link of data set
  dc = list(
    url = paste0(d$url,"/suppl/DC1"),
    htmlFile =  paste0(html.dir,"/qje_dc_vol_",d$vol,"_issue_",d$issue,"_article_",d$articleNum,".html")
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
    d$data.url = paste0("http://qje.oxfordjournals.org/content/",str)
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
  rows = which(has.substr(txt,"<em>JEL</em> Codes: "))[1]
  str = txt[rows]
  str = str.right.of(str,'<em>JEL</em> Codes: ')
  str = gsub(".","", str,fixed=TRUE)
  str = str.trim(sep.lines(str,","))  
  
  for (i in 1:7) {
    if (i > length(str)) {
      d[[paste0("JEL",i)]] = ""
    } else {
      d[[paste0("JEL",i)]] = str[i]      
    }
  }

  # We don't have keywords
  d$keywords = ""
  d$exists = TRUE
  d  
}

