schemas.ejd = function(dbname) {
  schemas = c(
    load.and.init.schemas(system.file("schema", paste0(dbname,".yaml"),package = "EconJournalData"))
  )
  schemas
}

setOption = function(name, value) {
  li = list(value)
  names(li) = name
  do.call(options, li)
}

get.ejd.db = function(dbname="articles", db.dir=getwd(), schemas=schemas.ejd(dbname)) {
  restore.point("get.ejd.db")

  db = getOption(paste0(dbname,".db.connection"))
  if (!is.null(db)) {
    if (!dbIsValid(db)) db = NULL
  }
  if (is.null(db)) {
    db = dbConnect(RSQLite::SQLite(),file.path(db.dir, paste0(dbname,".sqlite")))
    db = set.db.schemas(db, schemas)
    setOption(paste0(dbname,".db.connection"), db)
  }
  db
  
}

get.articles.db = function(db.dir=getwd()) {
  get.ejd.db("articles", db.dir)
}

get.files.db = function(db.dir=getwd()) {
  get.ejd.db("files", db.dir)
}

get.custom.db = function(db.dir=getwd()) {
  get.ejd.db("custom", db.dir)
}

