write.journal.vol.csv = function(dt, vol, journ) {
  restore.point("write.journal.vol.csv")     
  file = paste0(csv.dir,"/",journ,"_vol_",vol,".csv")
  write.csv(dt,file, row.names=FALSE)
}


download.scrap.html = function(d, update=FALSE) {
  restore.point("download.scrap.html")
  if ( (!update) & file.exists(d$htmlFile))
    return(d)    
  
  txt = "Not found"
  tryCatch(
    txt <- readLines(d$url),
    error = function(e) {}
  )
  
  writeLines(txt,d$htmlFile)
  
  return(d)
}
