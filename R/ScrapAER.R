

examples.parse.aer.volume = function() {
  parse.aer.volume(vol=100, issues=1, articles=2)
  vols = 100
  
  vols = rev(95)
  for (vol in vols) {
    parse.aer.volume(vol=vol)
  }
  
  parse.aer.volume(vol=96)
  
  create.all.detailed.csv()
}

parse.aer.volume = function(journ="aer",vol=103, issues = 1:12, articles=1:100) {
  restore.point("parse.aer.volume")
  
  if (!is.finite(vol))
    stop("Non finite vol")
  
  
  issue = 1
  articleNum = 1
  li = NULL
  
  journal = "aer"
  counter = 1
  page.ind = 1
  ignore.issues = 3
  if (vol <= 100)
    ignore.issues = 2
  if (vol >= 104)
    ignore.issues = 5
  

  
  for (issue in setdiff(issues, ignore.issues)) {
    urls = aer.issue.urls(vol=vol,issue=issue)[-1]
    urls = unique(urls)
    for (articleNum in intersect(seq_along(urls),articles)) {
              
      d.ind = list(journal="aer",issue=issue, vol=vol, issue=issue, articleNum=articleNum, url=urls[articleNum])    
      d = parse.aer.article(d.ind)
      cat(paste0("\nvol ", vol, " issue ", issue, " exists: ",d$exist,": ", d$url))
      
      if (d$exists) {
        li[[counter]] = d
        counter = counter+1
        cat(d$title," (",d$data.size, " ", d$data.unit,")\n")
      }
    }
  }
  
  dt = write.journal.vol.csv(li=li, journal="aer", vol=vol)
}


aer.issue.urls = function(vol, issue, update=FALSE) {
  restore.point("aer.issue.urls")
  html.root = "https://www.aeaweb.org/articles.php?doi=10.1257/aer"
  url = paste0(html.root,".",vol,".",issue)
  htmlFile = paste0(html.dir,"/aer_vol_",vol,"._issue_",issue,".html")
  if ( (!update) & file.exists(htmlFile)) {
    txt = readLines(htmlFile)
  } else {
    txt = suppressWarnings(try(readLines(url),silent = TRUE))
    if (is(txt,"try-error")) {
      cat("\nDid not find AER vol",vol, "issue",issue, " in the web.")
      return(NULL)
    }
    writeLines(txt,htmlFile)    
  }
  
  if (length(txt)<20) {
    return(NULL)
  }
  
#  <a style="font-weight:bold;" class="hyperlink" href="articles.php?doi=10.1257/aer.103.4.1138" />
  rows = which(str.starts.with(str.trim(txt),'<a style="font-weight:bold;" class="hyperlink" href="'))

  if (length(rows)==0) {
    rows = which(str.starts.with(str.trim(txt),'<a class="hyperlink" target="_blank" href="articles.php?doi='))
  }
  str = str.trim(txt[rows])
  str = str.right.of(str,'href="')
  str = str.left.of(str,'"')
  if (vol > 96 | (vol == 96 & issue >= 4)) {
    str = str.right.of(str,'/aer.')
    urls = paste0(html.root,".",str)
  } else {
    urls = paste0("https://www.aeaweb.org/",str)
  }
  unique(urls[-1])
}


examples.parse.aer.article = function() {
  d.ind = list(journal="aer",issue=1, vol=96, articleNum=1, url=NULL)    
  d = parse.aer.article(d.ind)

}

aer.extract.jel.from.html = function(d, txt=NULL) {
  restore.point("aer.extract.jel.from.html")

  s = NULL
  if (is.null(txt)) {
    d$htmlFile = paste0(html.dir,"/aer_vol_",d$vol,"_issue_",d$issue,"_article_",d$articleNum,".html")
    d = download.scrap.html(d)
    txt = readLines(d$htmlFile)
    if (length(txt)<20) {
      txt = NULL
    }
  }
  
  if (!is.null(txt)) {
    row = which(has.substr(txt,"JEL Classifications"))+1
    if (length(row)>0) {
      str = txt[row] 
      str = str.trim(txt[row])
      s = str.right.of(str,"style='width:700px;'>")
      s = str.left.of(s,'<br /></div>')
      s = sep.lines(s,"<br />")
    }
    if (is.null(s)) {
      row = which(has.substr(txt,"(JEL "))
      
      if (length(row)>0) {
        str = txt[row[1]]
        s = str.trim(str.left.of(str.right.of(str,"(JEL "),")"))
        s = sep.lines(s,", ")
      }
    }
  }
  s
}


aer.extract.jel.from.pdf = function(d, txt=NULL) {
  restore.point("aer.extract.jel.from.pdf")

  s = NULL
  if (is.null(txt)) {
    file = paste0(main.dir,"/articles_txt/",d$id,".txt")
    if (!file.exists(file))
      return(s)
    txt = readLines(file)
  }
  
  if (!is.null(txt)) {
    row = which(has.substr(txt,"(JEL "))
    if (length(row)>0) {
      str = txt[row[1]]
      s = str.trim(str.left.of(str.right.of(str,"(JEL "),")"))
      s = sep.lines(s,", ")
    }
  }
  s
}


aer.article.parse.jel = function(d, html.txt=NULL, pdf.txt = NULL) {
  restore.point("aer.article.parse.jel")
  s= aer.extract.jel.from.html(d=d,txt=html.txt)
  
  if (is.null(s)) {
    s = aer.extract.jel.from.pdf(d=d,txt=pdf.txt)
  }
  
  for (i in 1:7) {
    if (i > length(s)) {
      d[[paste0("JEL",i)]] = ""
    } else {
      d[[paste0("JEL",i)]] = s[i]      
    }
  }
  d$JEL = paste(d$JEL1,d$JEL2,d$JEL3,d$JEL4,d$JEL5,d$JEL6,d$JEL7, sep=", ")
  
  d
}

parse.aer.article = function(d) {
  restore.point("parse.aer.article")
  
  if (!is.finite(d$vol))
    stop("Non finite d$vol")

  
  d$id =  paste0("aer_",d$vol,"_",d$issue,"_",d$articleNum)
  
  d$htmlFile = paste0(html.dir,"/aer_vol_",d$vol,"_issue_",d$issue,"_article_",d$articleNum,".html")
  d = download.scrap.html(d)
  txt = readLines(d$htmlFile)

  if (length(txt)<20) {
    d$exists = FALSE
    return(d)
  }
  

  # Find title
  #<meta name="citation_title" content="Sales Taxes and Internet Commerce">
  row = which(str.starts.with(txt,'<meta name="citation_title"'))
  str = str.trim(txt[row])
  s = str.right.of(str,'content="')
  s = str.left.of(s,'"')
  d$title = s
  
  # Find publication date
  row = which(str.starts.with(txt,'<meta name="citation_publication_date"'))
  str = str.trim(txt[row])
  s = str.right.of(str,'content="')
  s = str.left.of(s,'"')
  d$publication.date = s
  
  # Find pdf link
  row = which(has.substr(txt,"Full-text Article"))
  if (length(row)>0) {
    row = row[1]
    str = paste0(txt[(row-1):row],collapse="\n")
    str = str.right.of(str,"href=\"")
    str = str.left.of(str,'"')
    d$article.url = paste0("http://www.aeaweb.org/",str)
  }

  
  # Find size of data set
  
  #<a style='color:#9A272D; text-decoration:underline;' target='_blank' href='http://www.aeaweb.org/aer/data/aug2013/20100951_data.zip'>Download Data Set</a> (2.69 MB) | 
    
  row = which(has.substr(txt,"Download Data Set"))
  if (length(row)==0)
    row = which(has.substr(txt,"Data Set"))
  
  if (length(row)>0) {
    row = row[1]
    str = txt[row] 
    str = str.right.of(str, "href='")
    str = str.left.of(str, "'>")
    d$data.url = str.trim(str)
    
    
    str = txt[row] 
    str = str.right.of(str, "(")
    str = str.left.of(str, ")")
    ret = str.split(str," ")[[1]]
    
    size = as.numeric(gsub(",","",ret[1],fixed=FALSE)); unit = ret[2]
    if (isTRUE(unit =="KB")) {
      unit = "MB"
      size = size / 1000
    }
    
    d$data.size = size
    d$data.unit = unit        
    d$has.data = TRUE
  } else {
    d$data.url = NA
    d$data.size = 0
    d$data.unit = "MB"        
    d$has.data = FALSE
  }
  
  # Find JEL Classifications
  d = aer.article.parse.jel(d, html.txt=txt)  

  d$exists = TRUE
  d  
}
