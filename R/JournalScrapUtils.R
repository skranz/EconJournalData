write.journal.vol.csv = function(li, vol, journal) {
  restore.point("write.journal.vol.csv")
  
  dt = as.data.frame(do.call("rbind",li))
  for (col in colnames(dt)) {
    dt[,col] = unlist(dt[,col])
  }
  dt = as.data.table(dt)
  if (length(li)==0)
    return(NULL)
  
  rows = which(nchar(dt$title)==0)
  if (length(rows)>0)
    dt$title[rows] = "Unknown Title"
  dt$JEL = paste(dt$JEL1,dt$JEL2,dt$JEL3,dt$JEL4,dt$JEL5,dt$JEL6,dt$JEL7, sep=", ")
  
  dt$data.size = as.numeric(dt$data.size)
  rows = which(dt$data.unit=="GB")
  if (length(rows)>0) {
    dt$data.size[rows] = dt$data.size[rows]*1000
    dt$data.unit[rows] = "MB"
  }
  rows = which(dt$data.unit=="bytes")
  if (length(rows)>0) {
    dt$data.size[rows] = dt$data.size[rows] / (1000*1000)
    dt$data.unit[rows] = "MB"
  }
  if (is.null(dt$keywords))
    dt$keywords = ""

  if (is.null(dt$publication.date))
    dt$publication.date = NA

  if (is.null(dt$has.data))
    dt$has.data = dt$data.size > 0
  
  dt$id = paste0(dt$journal,"_",dt$vol,"_",dt$issue,"_",dt$articleNum)

  library(dplyr)
  dt = select(dt,id,journal,vol,issue,articleNum,title,has.data,url, article.url, data.url, htmlFile,publication.date,data.size,data.unit,keywords,JEL,JEL1,JEL2,JEL3,JEL4,JEL5,JEL6,JEL7)
    
  file = paste0(csv.dir,"/",journal,"_vol_",vol,".csv")
  write.csv(dt,file, row.names=FALSE)
  invisible(dt)  
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
