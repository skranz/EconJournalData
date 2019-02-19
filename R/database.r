schemas.ejd = function() {
  schemas = c(
    load.and.init.schemas(system.file("schema/articles.yaml",package = "EconJournalData"))
  )
  schemas
}

get.articles.db = function(db.dir=getwd(), db.name="articles.sqlite",schemas=schemas.ejd()) {
  restore.point("get.articles.db")

  db = getOption("articles.db.connection")

  if (!is.null(db)) {
    if (!dbIsValid(db)) db = NULL
  }

  if (is.null(db)) {
    db = dbConnect(RSQLite::SQLite(),file.path(db.dir, db.name))
    db = set.db.schemas(db, schemas)
    options(articles.db.connection = db)
  }
  db
}

get.files.db = function(db.dir=getwd(), db.name="files.sqlite",schemas=schemas.ejd()) {
  restore.point("get.files.db")

  db = getOption("files.db.connection")

  if (!is.null(db)) {
    if (!dbIsValid(db)) db = NULL
  }

  if (is.null(db)) {
    db = dbConnect(RSQLite::SQLite(),file.path(db.dir, db.name))
    db = set.db.schemas(db, schemas)
    options(files.db.connection = db)
  }
  db
}
