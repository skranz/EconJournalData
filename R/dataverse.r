update.dataverse = function(name="restat", dir="D:/libraries/EconJournalData/") {
  name="restat"
  dir="D:/libraries/EconJournalData/"
  setwd(dir)
  
  dv = paste0("https://dataverse.harvard.edu/dataverse/",name)
  file = paste0("dv_",name,".csv")
  detail.file = paste0("dv_",name,"_details.csv")
  
  prev.df = read.csv(file,stringsAsFactors = FALSE)
  tu = dataverse.title.and.url(dv = dv,prev.df = prev.df)
  write.csv(tu,file, row.names=FALSE)
  
  prev.dat = read.csv(detail.file,stringsAsFactors = FALSE)
  dat = dataverse.detailed.csv(tu, prev.dat=prev.dat)
  write.csv(dat, detail.file, row.names = FALSE)
}

examples.dataverse.title.and.url = function() {
  init.journal.scrapper()
  ji = jis[["restat"]]
  jis[["restat"]]$dataverse_root

  df = restat.issue(journ="restat", vol=96, issue=1)
  
  if (FALSE) {
    dv = dataverse.title.and.url(ji$dataverse_root,max_page=100)
    setwd("D:/libraries/EconJournalData/")
    write.csv(dv,"dv_restat.csv",row.names = FALSE)
  }
}

#devtools::install_github("ropensci/dvn")
dataverse.title.and.url = function(dv = "https://dataverse.harvard.edu/dataverse/restat", page = NULL, max_page=100, prev.df=NULL) {
  
  if (is.null(page)) {
    page = 1
    li = list()
    while(page <= max_page) {
      cat("\npage ",page,"...")
      df = dataverse.title.and.url(dv, page=page)
      if (!is.null(prev.df)) {
        if (length(intersect(df$title, prev.df$title))>0)
          break
      }
      if (is.null(df)) break
      li[[page]] = df
      page = page+1
    }
    df = bind_rows(li)
    if (!is.null(prev.df)) {
      df = rbind(df, prev.df)
      df = df[!duplicated(df$title),]
    }
    return(df)
  }
  
  restore.point("dataverse.title.and.url")

  url = paste0(dv,"?q=&types=dataverses%3Adatasets&sort=dateSort&order=desc&page=",page)

  txt=readLines(url,warn = FALSE)
  
  txt = merge.lines(txt)
  str = strsplit(txt,split =  '<div class="card-title-icon-block">', fixed=TRUE)[[1]][-1]
  if (length(str)==0) return(NULL)
  
  str[1]
  short.urls = str.between(str,'href="','"')
  str = str.right.of(str,'href="')
  str = str.right.of(str, "<span")
  title = str.between(str, '">','</span>')

  title = str.right.of(title, 'Replication Data for: ')
  title = str.right.of(title, 'Replication Data and Codes for: ')
  title = str.right.of(title, 'Replication Data for ')
  title = str.right.of(title, 'Replication Data and Codes for ')
  title = str.right.of(title, 'Replication data for: ')
  title = str.right.of(title, 'Replication data and codes for: ')
  title = str.right.of(title, 'Replication data for ')
  title = str.right.of(title, 'Replication data and codes for ')
  title = str.trim(title)

  urls = str.right.of(short.urls,"persistentId=doi:")
  urls = paste0("http://dx.doi.org/", urls)
 
  data_frame(title = title, data.url = urls)
}

examples.dataverse.detailed.csv = function() {
  setwd("D:/libraries/EconJournalData/")
  tu = read.csv("dv_restat.csv",stringsAsFactors = FALSE)
  prev.dat = read.csv("dv_restat_details.csv",stringsAsFactors = FALSE)
  
  dat = dataverse.detailed.csv(tu, prev.dat=prev.dat)
  write.csv(dat, "dv_restat_details.csv", row.names = FALSE)
}

dataverse.detailed.csv = function(tu, max_row = NROW(tu),  prev.dat=NULL) {
  restore.point("dataverse.detailed.csv")
  
  if (length(prev.dat)>0) {
    exists = tu$title %in% prev.dat$title
    rows = which(!exists)
  } else {
    rows = 1:NROW(tu)
  }

  ind = 1
  li = lapply(seq_along(rows), function(ind) {
    row = rows[ind]
    ret = NULL
    try({
      cat("\n row ",ind, " of ", length(rows)," ...")
      title = tu$title[row]
      url = tu$data.url[row] 
      
      df = dataverse.files.df(url)
      
      ret <- dataverse.detailed.entry(title = title,data.url=url,file.df = df)
            
    })
    ret 
  })
  li = li[!sapply(li,is.null)]
  
  as_data_frame(rbindlist(li))
    
  
}

dataverse.files.df = function(url) {
  restore.point("dataverse.files.df")
  
  txt = readLines(url, warn = FALSE)
  
  txt = merge.lines(txt)
  str = strsplit(txt,'"datasetForm:tabView:filesTable:',fixed = TRUE)[[1]][-1]
  num = suppressWarnings(as.numeric(str.left.of(str,":")))
  rows = !is.na(num) & is.true(num>=0)
  str = str[rows]
  num = num[rows]
  type = str.between(str,":",'"')
  rows = type %in% c("fileSize","fileName")
  str = str[rows]; num = num[rows]; type = type[rows]
  val = str.between(str, '">','</span>')
  
  long = data_frame(pos=num, type=type, value=val)
  library(tidyr)
  df = spread(long,key=type,value = value)
  
  str = df$fileSize
  str = str.right.of(str, "- ")
  size = as.numeric(str.left.of(str, " "))
  unit = str.right.of(str, " ")
  
  # convert size to MB
  uf = c("TB"=1e6, "GB"=1000, "MB"=1,KB=1/1000)
  factor = rep(0,length(size))
  
  inds = unit %in% names(uf)
  factor[inds] = uf[unit[inds]]
  
  size = size * factor
  
  ext = tools::file_ext(df$fileName)
  
  ret = data_frame(file=df$fileName, ext=ext, mb=size)
  
  ret
  
}

dataverse.detailed.entry = function(title,data.url, file.df) {
  restore.point("create.detailed.csv")

  fd = file.df
  dr = list(title=title, data.url=data.url)
  
  ext = c("do","ado","r","mod","nb","py","m", "sas","prg", "ztt","c","java","cpp","js","f95","dta","csv","xls","xlsx","txt","zip","mat","dat","sas7bdat","rdata","xml","7z","gz","tar","tsp","g","lng","gms")

  data.ext = c("dta","csv","xls","xlsx","mat","dat","sas7bdat","rdata","xml","zip","tab")
  
  dr$total.size = sum(fd$mb)
  data.rows = tolower(fd$ext) %in% data.ext
  dr$data.size =  sum(fd$mb[data.rows])

  # Add total size of different file types
  ext = c(ext,"tab")
  d1 = fd[fd$ext %in% ext,]
  dat = summarise(group_by(d1,ext), mb=sum(mb))
  
  all.sizes = rep(0, length(ext))
  names(all.sizes) = ext
  
  all.sizes[dat$ext] = dat$mb
  names(all.sizes) = paste0("size.", ext)

  dr = c(dr, as.list(all.sizes))  
  dr
}


