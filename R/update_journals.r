

examples.update.journals = function() {
  init.journal.scrapper()
  
  max.size = 200
  update.journals(journals=c("jep","aer","aejpol","aejmic", "aejmac","aejapp","restud"), max.size=max.size, download.zip = !TRUE)
  update.journals(journals=c("aer"), max.size = max.size,download.zip = TRUE)
  update.journals(journals=c("restud"))
  update.journals(journals=c("aer"))

  
  mdt = find.missing.csv(journs=c("jep","aer","aejpol","aejmic", "aejmac","aejapp","restud"))
  update.missing.vols(mdt, max.num=Inf, download.zip = FALSE)
  

  md = find.missing.data()
  download.missing.data()

  create.all.detailed.csv()
  write.complete.data()
  write.articles.jel.csv()

  
  journ= "restud"
  journ = "aer"
}


update.missing.vols = function(mdt,download.zip=TRUE, max.size=500, max.num=Inf) {
  restore.point("update.missing.vols") 
  if (length(mdt)==0) return()
  i =1
  mdt = select(mdt, journ, vol) %>% unique
  if (max.num<NROW(mdt))
    mdt = slice(mdt,1:max.num)
  
  for (i in 1:NROW(mdt)) {
    vol = mdt$vol[i]; journ= mdt$journ[i]
    cat("\nupdate ", journ, " volume ", vol,"...")
    
    vol.dt = parse.journal.volume(journ=journ, vol=vol)
    if (download.zip) {
      message("download.article.data.zip...")
      try(ret <- apply.to.vol(vol=vol,journ=journ, fun= download.article.data.zip, max.size=max.size))
    }
    message("create.article.files.csv...")
    try(ret <- apply.to.vol(vol=vol,journ=journ, fun= create.article.files.csv))

  }
    
  message("create.all.detailed.csv...")
  create.all.detailed.csv()
  message("write.complete.data...")
  write.complete.data()
  message("write.articles.jel.csv...")
  write.articles.jel.csv()
}

download.missing.data = function(dt = NULL,dir=data.dir, journs = names(jis), max.size=500) {
  restore.point("download.missing.data")
  if (is.null(dt))
    dt = find.missing.data(dir=dir, journs=journs)
  
  if (NROW(dt)==0) return()
  
  dt = as.data.table(dt)
  i = 60
  for (i in 1:NROW(dt)) {
    d = dt[i,]
    try({
      download.article.data.zip(d=d, max.size=max.size)
      create.article.files.csv(d)         
    })
  }
}

find.missing.data = function(dt=read.complete.data(), dir=data.dir, journs=names(jis)) {
  restore.point("find.missing.data")
  dt = filter(dt, journ %in% journs)
  dt = filter(dt, has.data==TRUE)
  #aer_vol_104_issue_12_article_14.zip
  rnames = paste0(dt$journ, "_vol_",dt$vol,"_issue_",dt$issue,"_article_",dt$articleNum,".zip") 
  enames = list.files(path = dir)
  missing = setdiff(rnames, enames)
  rows = match(missing, rnames)
  res = dt[rows,]
  res
  
}


find.missing.csv = function(dt=all.issues.data(journs), dir=csv.dir, missing.type="csv", journs=names(jis)) {
  restore.point("find.missing.csv")
  
  rnames = paste0(dt$journ, "_vol_",dt$vol,".csv") 
  enames = list.files(path = dir)
  missing = setdiff(rnames, enames)
  rows = match(missing, rnames)
  
  if (length(rows)>0) {
    cbind(dt[rows,],missing.type="csv")
  } else {
    cbind(dt[rows,],missing.type="csv"[rows])
  }
  
}

find.missing.detailed.csv = function(dt=all.issues.data(journs), dir=dcsv.dir,missing.type="detailed",journs=names(jis)) {
  find.missing.csv(dt,dir, missing.type)
}

find.missing.files.csv = function(dt=all.issues.data(journs), dir=csv.dir,journs=names(jis)) {
  dt = all.issues.data()
  rnames = paste0(dt$journ, "_",dt$vol,"_", dt$issue,".csv") 
  enames = list.files(path = dir)
  setdiff(rnames, enames)
}



update.journals = function(journals=names(jis),overwrite=FALSE,download.zip=FALSE, max.size=500, years = year(Sys.time())) {
  restore.point("update.journals") 
  journ = journals[1]
  for (journ in journals) {
    message("scrap.journal.web.data...")
    try(scrap.journal.web.data(journ, overwrite=overwrite))
    if (download.zip) {
      message("download.article.data.zip...")
      try(ret <- apply.to.vol(vol=get.all.vol(journ),journ=journ, fun= download.article.data.zip, max.size=max.size))
    }
    message("create.article.files.csv...")
    try(ret <- apply.to.vol(vol=vol,journ=journ, fun= create.article.files.csv))
  
  }
  message("create.all.detailed.csv...")
  create.all.detailed.csv()
  message("write.complete.data...")
  write.complete.data()
  message("write.articles.jel.csv...")
  write.articles.jel.csv()
}

detailed.csv.for.update = function(overwrite=FALSE) {
  restore.point("detailed.csv.for.update")
  files = list.files(csv.dir)
  dfiles = list.files(paste0(main.dir,"/detailed_csv"))
  if (!overwrite) {
    files = setdiff(files, dfiles)  
  }
  files
  
}

create.all.detailed.csv = function(files = detailed.csv.for.update(),overwrite=FALSE) {
  restore.point("create.all.detailed.csv")
  for (file in files) {
    try(create.detailed.csv(csv.file=file))
  }
  
}

