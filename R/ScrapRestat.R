# Works quite different than other scraping methods
# Data is in a dataverse collection

examples.parse.restat.volume = function() {
  init.journal.scrapper()
  df = restat.issue(journ="restat", vol=96, issue=1)

}


restat.issue = function(journ,vol, issue, update=FALSE, ji=get.ji(journ)) {
  restore.point("restat.issue.urls")
  html.root = ji$html_root
  url = paste0(html.root,"/",vol,"/",issue)
  htmlFile = paste0(issues_html.dir,"/",journ,"_vol_",vol,"._issue_",issue,".html")
  if ( (!update) & file.exists(htmlFile)) {
    txt = readLines(htmlFile)
  } else {
    txt = try(readLines(url), silent=TRUE)
    if (is(txt,"try-error")) {
      cat("\nDid not find ",journ," vol",vol, "issue",issue, " in the web.")
      return(NULL)
    }

    writeLines(txt,htmlFile)    
  }
  
  if (length(txt)<20) {
    return(NULL)
  }
  
#<div class="arttitle">Moral Hazard in Health Insurance: Do Dynamic Incentives Matter? <img src="/templates/jsp/_midtier/_mit/images/access_no.gif" alt="No Access" title="No Access" class="accessIcon noAccess" /></div><span class="hlFld-ContribAuthor"><a href="/author/Aron-Dine%2C+Aviva">Aviva Aron-Dine</a></span>, <span class="hlFld-ContribAuthor"><a href="/author/Einav%2C+Liran">Liran Einav</a></span>, <span class="hlFld-ContribAuthor"><a href="/author/Finkelstein%2C+Amy">Amy Finkelstein</a></span>, <span class="hlFld-ContribAuthor"><a href="/author/Cullen%2C+Mark">Mark Cullen</a></span><div class="art_meta">Review of Economics and Statistics October 2015, Vol. 97, No. 4: 725-741.</div><a class="ref nowrap" href="/doi/abs/10.1162/REST_a_00518">Abstract</a> | <a class="ref nowrap pdf" target="_blank" title="Opens new window" href="/doi/pdf/10.1162/REST_a_00518">PDF (545 KB)</a> 
    
  txt = merge.lines(txt)
  str = strsplit(txt,split =  '<div class="arttitle">', fixed=TRUE)[[1]][-1]
  titles = str.left.of(str,' <img src')
  short.urls = str.between(str,'href="/doi/abs/','"')
  urls = paste0(paste0("http://www.mitpressjournals.org/doi/abs/", short.urls))
 
  data.frame(title=titles, url=urls)
}


examples.parse.restat.article = function() {
  init.journal.scrapper()
  
  d.ind = list(journal="restatmic",issue=2, vol=6, articleNum=8, url=NULL)    
  html.root = jis[[d.ind$journ]]$html_root
  d.ind$url = "http://www.aeaweb.org/articles.php?doi=10.1257/mic.6.2.247"
  
  d = parse.restat.article(d.ind)

}
# 
# find.dataverse.url = function(title) {
#   dv.url = NA
#   try({
#     query = paste0('dataverse "', title,'"')
#     url = google.search.url(query,num=1)
#     res = googleSearchXScraper(url)   
#     dv.url = str.right.of(res$url,"/url?q=") 
#     dv.url
#   })
#   dv.url
# }
