# Steps:
# 
# 1. Scrap meta data from journal websites
#       parse.aer.volume
# 2. Download data appendices
# 3. Analyse data appendices and generate files.csv
# 4. Generate html sites

examples.create.all.detailed.csv = function() {
  init.journal.scrapper()
  create.all.detailed.csv()  
}

create.all.detailed.csv = function(overwrite=FALSE) {
  files = list.files(csv.dir)
  dfiles = list.files(paste0(main.dir,"/detailed_csv"))
  if (!overwrite)
    files = setdiff(files, dfiles)
  
  for (file in files) {
    try(create.detailed.csv(csv.file=file))
  }
  
}
examples.create.detailed.csv = function() {
  create.detailed.csv(vol=81,journ="restud")
  create.detailed.csv(csv.file = "restud_vol_81.csv")
}

create.detailed.csv = function(vol,journ, csv.file = paste0(journ,"_vol_",vol,".csv")) {
  restore.point("create.detailed.csv")
  dt = read.csv(paste0(csv.dir,"/",csv.file))

  if (missing(journ)) {
    str = csv.file
    journ = str.left.of(str,"_")
  }
 if (missing(vol)) {
    str = csv.file
    vol = as.numeric(str.right.of(str.left.of(str,".csv"),"_vol_"))
  }
    
  
  
  ext = c("do","ado","r","mod","nb","py","m", "sas","prg", "ztt","c","java","cpp","js","f95","dta","csv","xls","xlsx","txt","zip","mat","dat","sas7bdat","rdata","xml","7z","gz","tar","tsp","g","lng","gms")

  data.ext = c("dta","csv","xls","xlsx","mat","dat","sas7bdat","rdata","xml","zip")
  
  li = lapply(1:NROW(dt),function(i) {
    cat(".")
    summarize.article.files.csv(d=dt[i,],ext = ext)
  })
  dat = robust.rbindlist(li)
  if (length(dat)==0) {
    message(paste0("No attachment files found in ", csv.file))
    return()
  }
    
  dat$file.type = tolower(dat$file.type)

  library(reshape2)
  library(dplyr)

  # Add total size of files in zip
  d = summarise(group_by(dat,id), zip.size = sum(total.size))
  dt = merge(dt,d, by="id", all.x=TRUE)
   
  # Add total size of data files
  d1 = dat[dat$file.type %in% data.ext,]
  d = summarise(group_by(d1,id), data.files.size = sum(total.size))
  dt = merge(dt,d, by="id", all.x=TRUE)

  # Add total size of different file types
  d1 = dat[dat$file.type %in% ext,]
  d = dcast.data.table(d1,id~file.type, value.var="total.size", fun.aggregate=sum)
  d[is.na(d)] = 0
  setnames(d,old=colnames(d)[-1], new=paste0("size.",colnames(d)[-1]))  
  dt = merge(dt,d, by="id", all.x=TRUE)
  
  rows = is.na(dt$data.size) & !is.na(dt$zip.size)
  dt$data.size[rows] = dt$zip.size[rows]
  rows = is.na(dt$data.unit)
  dt$data.unit[rows] = "MB"
  
 


  ji = get.journal.info(journ)
  year  = compute.article.year(dt$vol, dt$issue, ji)
  month = compute.article.month(dt$vol, dt$issue, ji)

  dt$year = year
  dt$month = month
  dt$date = paste0(year,"-", month, "-01")
 
  rows = which(is.na(dt$publication.date))
  if (length(rows)>0) {
    dt$publication.date[rows] = dt$date = paste0(year[rows],"-", month[rows], "-01")
  }
 
  destfile = paste0(main.dir,"/detailed_csv/",csv.file) 
  write.csv(dt,destfile)  
}



examples.create.article.files.csv = function() {
  journ = "aer"
  vol = 103
  file = paste0(journ,"_vol_",vol,".csv")
  dt = read.csv(paste0(csv.dir,"/",file))
  d = dt[1,]
  create.article.files.csv(d)
  summarize.article.files.csv(d$id)
}

# Look into the .zip file and generate a .csv with all filenames
create.article.files.csv = function(d, zipfile = paste0(data.dir,"/",d$journal,"_vol_",d$vol,"_issue_",d$issue,"_article_",d$articleNum,".zip")) {
  restore.point("create.article.files.csv")
  
  #"D:/data/EconJournalData/zipdata/aer_vol_105_issue_1_article_14.zip"
  #"D:/data/EconJournalData/zipdata/aer_vol_105_issue_1_article_14.zip"
  if (!file.exists(zipfile)) {
    cat(paste0("\nData of ", d$journal, " ", d$vol,".",d$issue, " '", d$title, "'' was not yet downloaded."))
    return(NULL)
  }
  
  fd = NULL
  tryCatch({fd <- unzip(zipfile, files = NULL, list = TRUE, exdir = ".", unzip = "internal")},
           error= function(e) {message(e)})
  if (is.null(fd))
    return(NULL)
    
  fd$file.type = file_ext(fd$Name)  
  
  
  if (FALSE) {  
  if (any("zip" %in% fd$file.type)) {
    temp.dir = paste0(main.dir,"/temp_zip")
    files = list.files(temp.dir, full.names=TRUE)
    if (length(files)>0) {
      remove.files(files)
    }
        
    unzip(zipfile, files = NULL, list = TRUE, exdir = ".", unzip = "internal")    
  }
  }
  
  fd = cbind(data.frame(id=d$id),fd)
  file = paste0(main.dir,"/files_csv/",d$id,".csv")
  write.csv(fd,file, row.names=FALSE)
}

summarize.article.files.csv = function(id=d$id, d=NULL, ext=NULL, dat=NULL) {
  restore.point("summarize.one.data.zip")
  
  if (is.null(dat)) {
    file = paste0(main.dir,"/files_csv/",id,".csv")
    if (!file.exists(file)) {
      cat("-")
      return(NULL)
    }
    dat = read.csv(file)
  }
  dat$file.type = tolower(dat$file.type)
  res = quick.by(dat, by="file.type","num.files=.N, total.size=sum(Length)/1e6")
  add.ext = setdiff(ext, res$file.type)
  if (length(add.ext)>0) {
    res = rbind(res,
      data.frame(file.type=add.ext, num.files=0, total.size=0))
  }
  cbind(data.frame(id=id),res)
}

examples.download.article.data.zip = function() {
    ret = apply.to.vol(vol=get.all.vol("aer"),journ="aer", fun= download.article.data.zip)

}
  

download.article.data.zip = function(d, max.size = 5000, verbose=FALSE) {
  restore.point("download.article.data.zip")
  d$data.url = as.character(d$data.url)
  if (is.na(d$data.url) | nchar(str.trim(d$data.url))==0)
    return(NULL)
  
  file = paste0(data.dir,"/",d$journal,"_vol_",d$vol,"_issue_",d$issue,"_article_",d$articleNum,".zip")
  if (!is.true(d$data.size< max.size) & is.finite(d$data.size)) {
    cat(paste0("\nData attachment of '", d$title, "'' is too large. Don't download."))
    return()
  }
  
  if (file.exists(file)) {
    if (verbose) {
      cat(paste0("\nData attachment of '", d$title, "'' was already downloaded."))
    } else {
      cat("e")
    }
    return()
  }
  names(d)
  cat(paste0("\nStart download of data attachment of '", d$title, "'...\n"))
  download.file(url = as.character(d$data.url), 
              destfile=file, quiet = FALSE, mode = "wb",
              cacheOK = TRUE, extra = getOption("download.file.extra"))

} 

download.article.pdf = function(d) {
  restore.point("download.article.pdf")
  url = as.character(d$article.url)
  if (nchar(url)==0)
    return(NULL)
  
  file = paste0(main.dir,"/articles/",d$id,".pdf")
  if (file.exists(file))
    return(NULL)
  
  tryCatch(
    suppressWarnings(download.file(url = url, destfile=file, quiet = FALSE, mode = "wb",cacheOK = TRUE)),
    error = function(e) message(e)
  )

  
}

convert.article.to.txt = function(d) {
  restore.point("convert.article.to.txt")
  
  source.file = paste0(main.dir,"/articles/",d$id,".pdf")
  if (!file.exists(source.file))
    return(NULL)
  dest.file = paste0(main.dir,"/articles_txt/",d$id,".txt")
  if (file.exists(dest.file))
    return(NULL)

  # set path to pdftotxt.exe and convert pdf to text
  exe <- "D:/programs/xpdfbin/bin64/pdftotext.exe"
  com = paste('"', exe, '" "', source.file, '" "', dest.file,'"', sep = "")
  cat(com)
  system(com, wait=TRUE, invisible=FALSE)
  
}
