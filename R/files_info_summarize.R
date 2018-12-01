example.files.info.summarize = function() {
  setwd("D:/libraries/EconJournalData/")
  init.journal.scrapper()
  db = get.articles.db()
  fdb = get.files.db()
  
  fi = dbGet(fdb,"files")
  res = summarize.files.info(fi)
  res
  store.files.summary(res)
  update.files.summary()
}

# Looks which files summaries have not yet been created
# and create them
update.files.summary = function(ids = articles.with.unsummarized.files(), chunk.size = 50) {
  restore.point("update.files.summary")
  db = get.articles.db()
  fdb = get.files.db()
  
  id.li = split.into.chunks(ids, chunk.size)
  
  chunk = id.li[[1]]
  for (chunk in id.li) {
    fi = dbGet(fdb, "files", list(id=chunk))
    #fi = dbGet(fdb,)
  }
  
}

articles.with.unsummarized.files = function() {
  db = get.articles.db()
  fdb = get.files.db()
  files_ids = dbGet(fdb, sql="select distinct id from files", schema=NULL)$id
  sum_ids = dbGet(db, sql="select distinct id from files_summary", schema=NULL)$id
  setdiff(files_ids, sum_ids) 
}

store.files.summary = function(fs, db = get.articles.db()) {
  dbInsert(db,"files_summary", fs,mode = "replace")
}

summarize.files.info = function(fi, opts=get.ejd.opts()) {
  restore.point("summarize.files.info")
  
  fi = filter(fi, file_type %in% opts$file_type$all_ext)
  file_types = unique(fi$file_type)
  
  res = group_by(fi,id, file_type) %>%
    summarize(num_files=n(), mb=sum(kb)/1000) %>%
    ungroup()
  res$is_code = res$file_type %in% opts$file_types$code_ext
  res$is_data = res$file_type %in% opts$file_types$data_ext
  
  res
}