examples.google.search = function() {
  url = "http://www.google.com/search?as_q=bank+crisis&as_epq=&as_oq=&as_eq=&as_nlo=&as_nhi=&lr=&cr=&as_qdr=all&as_sitesearch=https%3A%2F%2Fwww.aeaweb.org%2Farticles.php&as_occt=any&safe=images&tbs=&as_filetype=&as_rights=&gws_rd=ssl"  

  url="http://www.google.com/search?q=bank+crisis+site%3Ahttps%3A%2F%2Fwww.aeaweb.org%2Farticles.php&lr=lang_en&as_qdr=all&source=lnt&tbs=lr%3Alang_1en%2Ccdr%3A1%2Ccd_min%3A12%2F29%2F2013%2Ccd_max%3A01%2F01%2F2015&tbm="
  URLdecode(url)
  

  
  url = google.search.url("crisis+bank",num=100, start.date="2005-12-29",site="https://www.aeaweb.org/articles.php")  
  url
  URLdecode("http://www.google.com/search?q=bank+crisis+site%3Ahttps%3A%2F%2Fwww.aeaweb.org%2Farticles.php&lr=lang_en&as_qdr=all&source=lnt&tbs=lr%3Alang_1en%2Ccdr%3A1%2Ccd_min%3A12%2F29%2F2013%2Ccd_max%3A01%2F01%2F2015&tbm=")
  res = googleSearchXScraper(url)  

  
  ma = match(rurl, aurl)  
  rows = unique(ma[!is.na(ma)])
  sdt = dt[rows,]
  
  #http://www.aeaweb.org/articles.php?doi=10.1257/app.6.3.1
  
  URLdecode(res$url)
  res = googleSearchXScraper("http://www.google.com/search?q=r+google+search&ie=utf-8&oe=utf-8#q=r+google+search&start=10")  
}

google.journals = function(query,dt) {
  restore.point("google.journals")
  rows = NULL
    
  ret = google.aej(query,dt)
  rows = unique(c(rows, ret$rows))
  if (ret$hits >= 100) {
    ret = google.aej(query,dt,start=100)
    setdiff(ret$rows,rows)
    rows = unique(c(rows, ret$rows))
  }
  
  ret = google.restud(query,dt)
  rows = unique(c(rows, ret$rows))
  
  sdt = as.data.frame(dt)[rows,,drop=FALSE]
  sdt
}

examples.google.aej = function() {
  sdt = google.aej("cartel", dt = app$glob$dt)
  sdt = google.aej("collusion", dt = app$glob$dt)
  sdt = google.aej("development", dt = app$glob$dt)

  sdt = google.restud("development", dt = app$glob$dt)

}

google.restud = function(query,dt,start=NULL,start.date = "2005-01-01",num=100, ...) {

  url = google.search.url(query,num=num, start.date=start.date,start=start, ...,
                          as_sitesearch="http://restud.oxfordjournals.org/content/") 
  url
  res = googleSearchXScraper(url)   

  rurl = str.right.of(res$url,"/content/")
  rurl = str.right.of(rurl,"early/")
  rurl = str.left.of(rurl,".")
  rurl = str.left.of(rurl, "%")
  rurl = str.left.of(rurl,"/restud")
  rurl

  aurl = dt$url
  aurl = str.right.of(aurl,"/content/")
  aurl
  
  ma = match(rurl, aurl)  
  rows = unique(ma[!is.na(ma)])
  
  return(list(rows=rows, hits = NROW(res)))
  

  #http://restud.oxfordjournals.org/content/79/1/88
}

google.aej = function(query, dt,start = NULL, start.date = "2005-01-01",num=100) {
  restore.point("google.aej")
  
  url = google.search.url(query,num=num, start.date=start.date, start=start,
                          as_sitesearch="http://www.aeaweb.org/articles.php") 
  url
  res = googleSearchXScraper(url)  
    
  rurl = str.right.of(res$url,"/articles.php")
  rurl = str.right.of(rurl,"/")
  rurl = str.left.of(rurl,"&")
  rurl = str.left.of(rurl, "%")
  rurl
  
  aurl = dt$url
  aurl = str.right.of(aurl,"?doi")
  aurl = str.right.of(aurl,"/")

  ma = match(rurl, aurl)  
  rows = unique(ma[!is.na(ma)])
  
  return(list(rows=rows, hits = NROW(res)))
}

google.search.url = function(query, base.url = "http://www.google.com/search?", lr="lang_en",num=100,site=NULL,as_sitesearch=NULL, start=NULL, start.date = NULL, end.date=NULL,as_qdr="all",source=NULL, tbm=NULL) {
  query = paste0(query,collapse="+")
  url = base.url
  add = function(lab, value=NULL) {
    if (is.null(value)) return()
    url <<- paste0(url, lab,value)
  }
  

  
  add("q=",query)
  add("+site:",site)
  add("&lr=",lr)
  add("&num=",num)
  add("&as_sitesearch=",as_sitesearch)
  add("&as_qdr=",as_qdr)
  add("&source=",source)
  
  
  if (!is.null(start.date)) {
    if (is.null(end.date))
      end.date = Sys.time()
    start.date = as.Date(start.date)    
    sstr = format(start.date,"%m/%d/%Y")

    end.date = as.Date(end.date)    
    estr = format(end.date,"%m/%d/%Y")
    #"tbs=cdr:1,cd_min:3/2/1984,cd_max:6/5/1987"
    tbs = paste0("lr:lang_1en,cdr:1,cd_min:",start.date,",cd_max:",end.date)
    add("&tbs=",tbs)
  }
  
  add("&tbm=",tbm)
  add("&start=",start)
  URLencode(url)
#   
#   
#   https://www.google.com/search?
# as_q=test (query string)
# &hl=en (language)
# &num=10 (number of results [10,20,30,50,100])
# &btnG=Google+Search
# &as_epq= (complete phrase)
# &as_oq= (at least one)
# &as_eq= (excluding)
# &lr= (language results. [lang_countrycode])
# &as_ft=i (filetype include or exclude. [i,e])
# &as_filetype= (filetype extension)
# &as_qdr=all (date [all,M3,m6,y])
# &as_nlo= (number range, low)
# &as_nhi= (number range, high)
# &as_occt=any (terms occur [any,title,body,url,links])
# &as_dt=i (restrict by domain [i,e])
# &as_sitesearch= (restrict by [site])
# &as_rights= (usage rights [cc_publicdomain,cc_attribute,cc_sharealike,cc_noncommercial,cc_nonderived]
# &safe=images (safesearch [safe=on,images=off])
# &as_rq= (similar pages)
# &as_lq= (pages that link)
# &gl=us (2-digit country code in lowercase) 

  
}

# Author: Tony Breyal
# Date: 2011-11-11
# Modified: 2011-11-13
# Description: This function extracts as much information as it can for each result returned by a Google search page.
# Contributations: Philipp Riemer - improvements to the xpathLVApply function code, see http://tonybreyal.wordpress.com/2011/11/11/web-scraping-yahoo-search-page-via-xpath/#comment-45
# Blog Reference: http://tonybreyal.wordpress.com/2011/11/10/facebook-graph-api-explorer-with-r/

# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.


googleSearchXScraper <- function(input) {
  restore.point("googleSearchXScraper")
  
  ###--- PACKAGES ---###
  # load packages
  require(RCurl)
  require(XML)
  
  
  ###--- LOCAL FUNCTIONS ---###
  # I added a wrapper around xpathSApply to deal with cases are NULL and are lost during the list to vector conversion process.
  xpathLVApply <- function(doc, xpath.base, xpath.ext, FUN, FUN2 = NULL) {
    # get xpaths to each child node of interest
    nodes.len <- length(xpathSApply(doc, xpath.base))
    paths <- sapply(1:nodes.len, function(i) paste(xpath.base, "[", i, "]", xpath.ext, sep = ""))
    
    # extract child nodes
    xx <- lapply(paths, function(xpath) xpathSApply(doc, xpath, FUN))
    
    # perform extra processing if required
    if(!is.null(FUN2)) xx <- FUN2(xx)
    
    # convert NULL to NA in list
    xx[sapply(xx, length)<1] <- NA
    
    # return node values as a vector
    return(as.vector(unlist(xx)))
  }
  
  # Determine how to grab html for each element of input
  evaluate_input <- function(input) {
    # determine which elements of input are files (assumed to contain valid html) and which are not(assumed to be valid URLs)
    is.file <- file.exists(input)
    
    # stop if input does not seem to be URLS and/or files
    if(sum(is.file) < 1 && length(input) > 1) stop("'input' to googleSearchXScraper() could not be processed.")
    
    # read html from each file
    html.files <- lapply(input[is.file], readLines, warn = FALSE)
    
    # read html from each URL
    html.webpages <- lapply(input[!is.file], getURL, followlocation = TRUE)
    
    # return all html data as list
    return(c(html.files, html.webpages))
  }
  
  # construct data frame from the html of a single google search page
  get_google_search_df <- function(html) {
    # parse html into tree structure
    doc <- htmlParse(html)

    # construct google search data frame
    xpath.base <- "//li[@class='g']"
    df <- data.frame(
      title = xpathLVApply(doc, xpath.base, "//h3//a[@href]", xmlValue),
      url = xpathLVApply(doc, xpath.base, "//h3//a/@href", I),
      description = xpathLVApply(doc, xpath.base, "//div[@class='s']", xmlValue),
      cached = xpathLVApply(doc, xpath.base, "//div[@class='s']//span[@class='flc']//a/@href", 
                            FUN = function(x) ifelse(length(x[grepl("webcache", x)])<1, "", x[grepl("webcache", x)]),
                            FUN2 = function(xx) sapply(xx, function(x) ifelse(is.null(x[x!=""]), NA, x[x!=""]))),
      similar = xpathLVApply(doc, xpath.base, "//div[@class='s']//span[@class='flc']//a/@href", 
                             FUN = function(x) ifelse(length(x[!grepl("webcache", x)])<1, "", x[!grepl("webcache", x)]), 
                             FUN2 = function(xx) sapply(xx, function(x) ifelse(is.null(x[x!=""]), NA, x[x!=""]))),
      stringsAsFactors = FALSE)
  
    # free doc from memory
    free(doc)
    
    # return yahoo search dataframe
    return(df)
  }
  
  
  ###--- MAIN ---##
  # STEP 1: Determine input type(s) and grab html accordingly
  html.list <- evaluate_input(input)
  
  # STEP 2: get google data frame.
  df <- do.call("rbind", lapply(html.list, get_google_search_df))
  return(df)
}


# ###--- EXAMPLES ---###
# input <- "http://www.google.co.uk/search?aq=f&gcx=w&sourceid=chrome&ie=UTF-8&q=r+project#q=r+project&hl=en&tbo=1&prmdo=1&output=search&source=lnt&tbs=qdr:m&sa=X&ei=qvO_Ttj1KITb8AOPzqT_Aw&ved=0CAoQpwUoBA&fp=1&biw=1920&bih=1086&cad=b&bav=on.2,or.r_gc.r_pw.r_cp.,cf.osb"
# df <- googleSearchXScraper(input)
# t(df[1, ])
# 
# # title       "The R Project for Statistical Computing"                                                                                                                                                                                                                                                                        
# # url         "http://www.r-project.org/"                                                                                                                                                                                                                                                                                      
# # description "R, also called GNU S, is a strongly functional language and environment to   statistically explore data sets, make many graphical displays of data from custom ...www.r-project.org/ - Cached - SimilarThe Comprehensive R Archive NetworkDownload R for WindowsManualsCRAN - MirrorsSearchContributed Packages"
# # cached      "//webcache.googleusercontent.com/search?gcx=w&hl=en&q=cache:zxfJms4oTtkJ:http://www.r-project.org/+r+project&ct=clnk"                                                                                                                                                                                           
# # similar     "/search?gcx=w&hl=en&tbo=1&q=related:http://www.r-project.org/+r+project&sa=X" 
