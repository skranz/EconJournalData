
example.scrap.econometrica = function() {
  vol = 80
  issue = 1
  d = nlist(vol,issue)
  
}

examples.ecma.issue.urls = function(d) {
  init.journal.scrapper()
  vol = 81
  issue = 5
  ecma.issue.urls(vol=vol,issue=issue)
  #http://www.econometricsociety.org/issue.asp?ref=0012-9682&vid=82&iid=1&oc=&s=
}

ecma.issue.urls = function(journ="ecma",vol, issue, update=FALSE, ji=get.journal.info(journ)) {
  restore.point("ecma.issue.urls")
  url = paste0("http://www.econometricsociety.org/issue.asp?ref=0012-9682&vid=",vol,"&iid=",issue,"&oc=&s=")
  htmlFile = paste0(html.dir,"/",journ,"_vol_",vol,"._issue_",issue,".html")
  if ( (!update) & file.exists(htmlFile)) {
    txt = readLines(htmlFile)
  } else {
    txt = readLines(url)
    writeLines(txt,htmlFile)    
  }
  
  if (length(txt)<20) {
    return(NULL)
  }
  
  
  code = paste0("abstract.asp?ref=0012-9682&vid=",vol,"&iid=",issue,"&aid=")
  #code = paste0("abstract.asp")
  rows = which(has.substr(txt,code))
  str = str.trim(txt[rows])
  str = str.right.of(str,'href="')
  str = str.left.of(str,'"')
  urls = paste0(ji$html_root,"/",str)
  urls
}


ecma.extract.jel.from.pdf = function(d, txt=NULL) {
  restore.point("aej.extract.jel.from.pdf")

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


aej.article.parse.jel = function(d, html.txt=NULL, pdf.txt = NULL) {
  restore.point("aej.article.parse.jel")
  s= aej.extract.jel.from.html(d=d,txt=html.txt)
  
  if (is.null(s)) {
    s = aej.extract.jel.from.pdf(d=d,txt=pdf.txt)
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


examples.parse.aej.article = function() {
  init.journal.scrapper()
  
  d.ind = list(journal="aejmic",issue=2, vol=6, articleNum=8, url=NULL)    
  html.root = jis[[d.ind$journal]]$html_root
  d.ind$url = "http://www.aeaweb.org/articles.php?doi=10.1257/mic.6.2.247"
  
  d = parse.aej.article(d.ind)

}


parse.aej.article = function(d) {
  restore.point("parse.aej.article")
  
  if (!is.finite(d$vol))
    stop("Non finite d$vol")

  
  d$id =  paste0(d$journal,"_",d$vol,"_",d$issue,"_",d$articleNum)
  
  d$htmlFile = paste0(html.dir,"/",d$journal,"_vol_",d$vol,"_issue_",d$issue,"_article_",d$articleNum,".html")
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
  
  #<a style='color:#9A272D; text-decoration:underline;' target='_blank' href='http://www.aeaweb.org/aej/data/aug2013/20100951_data.zip'>Download Data Set</a> (2.69 MB) | 
    
  row = which(has.substr(txt,"Download Data Set"))
  
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
  d = aej.article.parse.jel(d, html.txt=txt)  

  d$exists = TRUE
  d  
}
