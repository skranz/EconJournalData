
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

