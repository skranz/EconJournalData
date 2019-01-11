example.files.info.summarize = function() {
  setwd("D:/libraries/EconJournalData/")
  init.journal.scrapper()
  db = get.articles.db()
  fdb = get.files.db()
  
  ids = articles.with.unsummarized.files()
  update.files.summary(ids)
  
  
  
  ids = c("aer_108_11_1","aer_108_11_2")
  arts = dbGet(db,"article", list(id=ids), where.in=TRUE)
  arts = dbGet(db,"article", list(id=ids[1]), where.in=!TRUE)

}

#' Looks which files summaries have not yet been created
#' and create them
update.files.summary = function(ids = articles.with.unsummarized.files(max.art), chunk.size = 50, max.art = Inf) {
  restore.point("update.files.summary")
  db = get.articles.db()
  fdb = get.files.db()
  
  id.li = split.into.chunks(ids, chunk.size)
  
  chunk = id.li[[1]]
  for (chunk in id.li) {
    fi = dbGet(fdb, "files", list(id=chunk), where.in=TRUE)
    fs = summarize.files.info(fi)
    store.files.summary(fs)
  }
  
}

articles.with.unsummarized.files = function(max.art=Inf) {
  db = get.articles.db()
  fdb = get.files.db()
  files_ids = dbGet(fdb, sql="select distinct id from files", schema=NULL)$id
  sum_ids = dbGet(db, sql="select distinct id from files_summary", schema=NULL)$id
  res = setdiff(files_ids, sum_ids)
  if (length(res)>max.art)
    res = res[seq_len(max.art)]
  res
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