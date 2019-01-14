examples.dataverse.html = function() {
  library(dataverse)
  
  journ="restat"
  contents=dataverse_contents(journ)
  update.dataverse.journal(journ, contents=contents,max.articles = 100)

  
  journ="qje"
  contents=dataverse_contents(journ)
  update.dataverse.journal(journ, contents=contents,max.articles = 20)


  journ="jaere"
  contents=dataverse_contents(journ)
  update.dataverse.journal(journ, contents=contents,max.articles = 20)

    
  li = lapply(contents, function(c) {
    class(c) = "list"
    as_data_frame(as.list(c))
  })
  content.df = bind_rows(li)
  content = content.df[1,]
  res = extract.qje.dataverse(content)
  
    
  res = extract.restat.dataverse(content,dv)
  insert.dataverse.article(res)
}


update.dataverse.journal = function(journ="restat", db=get.articles.db(), fdb=get.files.db(), contents=dataverse_contents(journ), max.articles = Inf) {
  restore.point("update.dataverse.journal")
  
  
  
  li = lapply(contents, function(c) {
    class(c) = "list"
    as_data_frame(as.list(c))
  })
  content.df = bind_rows(li)
  
  articles = dbGet(db,"article", list(journ=journ))
  articles = filter(articles, title != "Review of Economics and Statistics: Forthcoming", !is.na(year), !str.starts.with(tolower(title),"replication data"), !str.starts.with(title, '"'), !str.starts.with(title, "'"))
  
  has.dv.ids = as.integer(str.right.of(articles$id,"_"))
  
  new.rows = which(!(content.df$id %in% has.dv.ids))
  
  cat(paste0("\n", length(new.rows), " new articles with data for ", journ, " found."))
  
  if (max.articles < length(new.rows)) {
    new.rows = new.rows[1:max.articles]
    cat(paste0("\nOnly update max.articles = ", max.articles, " articles."))
  }
  
  cat("\n")
  for (row in new.rows) {
    cat("\n...")
    content=content.df[row,]
    dv = get_dataset(content$id)
    res = extract.dataverse.article(journ=journ, content=content, dv=dv)
    if (is.null(res)) {
      cat(" no files found.")
      next
    }
    if (is.na(res$article$title) | isTRUE(nchar(res$article$title <= 4))) {
      stop(paste0("Could not correctly parse row ", row, " dataverse id=", content$id))
    }
    insert.dataverse.article(res, db=db, fdb=fdb)
    cat(paste0(" added ", res$article$id," ", res$article$title, " (", res$article$year,")"))
  }
  cat("\n")
}

insert.dataverse.article = function(li,db = get.articles.db(), fdb = get.files.db(), add.author=!is.null(li$author), add.files = !is.null(li$files)) {
  restore.point("insert.dataverse.article")
  
  article = li$article
  article$date = as.Date(article$date)
  if (is.null(article)) return()
  
  dbWithTransaction(db,{
    dbDelete(db,"article",list(id=article$id))
    dbInsert(db,"article", article)
    if (add.author) {
      dbDelete(db,"author",list(id=article$id))
      dbInsert(db,"author", li$author)
    } 
  })
  if (add.files) {
    dbWithTransaction(fdb,{
      dbDelete(fdb,"files",list(id=article$id))
      dbInsert(fdb,"files",li$files)
    })
  }
}

extract.dataverse.article = function(journ,content,dv,article_url=NA) {
  extract.restat.dataverse(content, dv, article_url, journ=journ)
}

# Extract article, authors and files
extract.restat.dataverse = function(content,dv=NULL, article_url=NA, journ="restat") {
  restore.point("extract.restat.dataverse")
  
  if (is.null(dv))
    dv = dataverse::get_dataset(content$id)

  if (length(dv$files)==0) return(NULL)
  
  
  meta = dv$metadataBlocks$citation$fields
  title = meta$value[[1]]
  
  title = remove.str.starts(title, c(
    "Replication data for: ","Replication Data for: ",
    "Replication Data For: ","Replication data for ",
    "Replication Data for ","Replication Data and Programs for: ",
    "Replication Data and Codes for:", "Replication Data and Code for: ",
    "Replication Code for: "
  ))
  title = remove.quotes(title)
  year = as.integer(format(as.Date(content$publicationDate), "%Y"))

  
  desc = NA
  vals = as.character(unlist(meta$value[[4]]))
  ind = which(has.substr(vals, "Review of Economics and Statistics"))[1]
  if (!is.na(ind)) {
    desc = vals[ind]
  }

  if (!is.na(desc)) {
    #title = str.between(desc,'"','"')
    vol = as.integer(str.between(desc,"Review of Economics and Statistics ",":"))
    issue = as.integer(str.between(desc,paste0("Review of Economics and Statistics ",vol,":"),","))
  } else {
    vol = NA
    issue = NA
  }
  artnum = NA
  article_url = NA
  data_url = content$persistentUrl
  has_data = 1
  
  id = paste0(journ,"_",content$id)
  
  author.row = 2
  if (isTRUE(str.starts.with(colnames(meta$value[[3]])[1],"author")))
    author.row = 3
  authors = meta$value[[author.row]]$authorName$value
  
  files = as_data_frame(list(
    id=id,
    journ=journ,
    vol=vol,
    file=dv$files$filename,
    file_type = tools::file_ext(dv$files$filename),
    kb = dv$files$filesize / 1000,
    nested = 0
  ))
  size = sum(files$kb)/1000

  article = list(id=id,journ=journ,year=year,date=content$publicationDate, title=title, vol=vol, issue=issue, artnum=artnum,article_url=article_url,has_data=TRUE,data_url=data_url, size=size,unit="MB",file_info_stored=TRUE, num_authors=length(authors))
  
  author = as_data_frame(list(id=id, author=authors))
  
  return(list(article=article, author=author, files=files))
    
}



remove.str.starts = function(str, starts) {
  for (start in starts) {
    if (str.starts.with(str, start)) {
      str = str.right.of(str, start)
      return(str)
    }
  }
  str
}

remove.quotes = function(str) {
  if (str.starts.with(str, '"') & str.ends.with(str, '"')) {
    str = str.remove.ends(str,1,1)
  }
  if (str.starts.with(str, "'") & str.ends.with(str, "'")) {
    str = str.remove.ends(str,1,1)
  }

  str
}