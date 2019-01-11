examples.scrap.aea.journal = function() {
  setwd("D:/libraries/EconJournalData/")
  init.journal.scrapper()
  db = get.articles.db()
  arts = dbGet(db,"article")
  art = as.list(arts[1,])
  file = download.or.get.data.zip(art)
  dat = extract.zip.files.info(file, art=art)
  store.article.files.info(dat, art)
  
  download.newest.articles.zips(max_articles=1, max_mb=1, wait.between=0)
}

download.newest.articles.zips = function(max_articles=1, max_mb=5000, where = NULL,db=get.articles.db(), wait.between = 60,...) {
  restore.point("download.newest.articles.zips")
  rs = prepare.nondowloaded.zip.article.query(max_mb, where, db)
  art.df = dbFetch(rs,max_articles)
  
  rows = art.df$unit == "GB"
  
  art.df$size[rows] = art.df$size[rows] * 1000
    
  art.df = filter(art.df, size <= max_mb)
  
  counter = 0
  while(counter < NROW(art.df)) {
    counter = counter+1
    art = as.list(art.df[counter,])
    
    try({
      art$downloaded_file=download.or.get.data.zip(art=art,...)
      if (!is.null(file)) {
        dat = extract.zip.files.info(art = art,...)
        store.article.files.info(dat,art)
      }
      if (counter < max_articles)
        Sys.sleep(wait.between)
    })
  }
}

prepare.nondowloaded.zip.article.query = function(max_mb = 5000, where=list(), db=get.articles.db()) {
  restore.point("prepare.nondowloaded.zip.article.query")
  sql = "select * from article WHERE
  has_data == 1 AND downloaded_file IS NULL
AND (size < :max_mb OR size IS NULL)"
  if (length(where)>0) {
    sql = paste0(sql, paste0(" AND ", names(where), " = :",names(where), collapse=" ")) 
  }
  sql = paste0(sql," ORDER BY date DESC" )

  params = c(list(max_mb=max_mb), where)
  rs = dbSendQuery(db, sql, params)
  rs
} 

# Downloads a ZIP file with data
download.or.get.data.zip = function(art, update.db=TRUE,download.dir = opts$download.dir, opts=get.ejd.opts(), max.mb=5000) {
  if (!isTRUE(art$has_data==1))
    return(NULL)
  
  restore.point("download.or.get.data.zip")
  
  if (!is.empty(art$downloaded_file)) {
    file = file.path(download.dir, art$downloaded_file)
    if (file.exists(file))
      return(file)
  }
  
  file = paste0(art$journ,"_vol_",art$vol,"_issue_",art$issue,"_article_",art$artnum,".zip")  
  
  cat(paste0("Download '", art$title, "': ", ifelse(is.null(art$size),0,art$size)," ", art$unit,"..."))
  res = try(
    download.file(url = as.character(art$data_url), 
              destfile=file.path(download.dir,file), quiet = FALSE, mode = "wb",
              cacheOK = TRUE, extra = getOption("download.file.extra"))
  )
  if (is(res,"try-error")) {
    file = NULL
    cat("\n\t",as.character(res))
  } else {
    cat(" done.")
  }
  db = get.articles.db()
  dbUpdate(db, "article",list(downloaded_file=file),where=list(id=art$id))
  return(file)
}

store.article.files.info = function(dat,art,fdb=get.files.db(), db=get.articles.db()) {
  dbWithTransaction(fdb,{
    dbDelete(fdb, "files", list(id=art$id))
    dbInsert(fdb, "files", dat)
  })
  
  dbUpdate(db,"article", list(file_info_stored=TRUE), list(id=art$id))
}

extract.zip.files.info = function(file=file.path(download.dir,art$downloaded_file),art,download.dir = opts$download.dir, temp.dir = opts$temp.dir, opts=get.ejd.opts(), recursive=TRUE, nested=FALSE, clear.temp.dir=FALSE, ignore.files.start = "__MACOSX/") {
  restore.point("extract.zip.files.info")

  if (!file.exists(file)) {
    cat("\nZIP file does not exist")
    return(NULL)
  }
  
  fd = NULL
  tryCatch({fd <- unzip(file, files = NULL, list = TRUE, exdir = ".", unzip = "internal")},
           error= function(e) {message(e)})
  if (is.null(fd))
    return(NULL)
  
  if (length(ignore.files.start)==1)
    fd = filter(fd, !str.starts.with(fd$Name, ignore.files.start))
  
  .nested = nested
  dat = transmute(fd,
    id = art$id,
    journ=art$journ,
    vol=art$vol,
    file=Name,
    file_type=tolower(file_ext(Name)),
    kb = Length / 1000,
    nested = .nested
  )
    
  if (recursive) {  
    if (any("zip" %in% dat$file_type)) {
      restore.point("get.zip.files.info.recursive")
      if (is.null(temp.dir))
        stop("No temp.dir specified.")
      
      if (!dir.exists(temp.dir)) {
        try(dir.create(temp.dir))
        cat("Tried to create temporary directory ", temp.dir)
      }
      # Clear TEMP dir
      files = list.files(temp.dir, full.names=TRUE,include.dirs = TRUE)
      if (length(files)>0 & clear.temp.dir) {
        unlink(files)
      }
      
      
      zip.files = dat$file[dat$file_type=="zip"]
      unzip(file, files = zip.files, exdir = temp.dir)
      li = lapply(zip.files, function(zip.file) {
        extract.zip.files.info(file = file.path(temp.dir,zip.file), art = art, temp.dir=temp.dir, opts=opts, nested=TRUE, clear.temp.dir=FALSE)        
      })
      inner.df = bind_rows(li)
      dat = bind_rows(dat, inner.df)
   
    }
  }
  dat
}
