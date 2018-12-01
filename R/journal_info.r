
init.journal.scrapper = function(
    base.dir = getwd(),
    pkg.base.dir = system.file("base", package = "EconJournalData"),
    download.dir = file.path(base.dir,"downloads"),
    temp.dir = file.path(base.dir, "temp")
) {
  restore.point("init.journal.scrapper")
  
  jis = load.jis(file = paste0(pkg.base.dir,"/journal_info.yaml"))
  jel <- read.csv(paste0(pkg.base.dir,"/jel_codes.csv"), stringsAsFactors=FALSE)
  jel$digits <- nchar(jel$code)
  jel$name = paste0(jel$code,": ", jel$label)
  rows = jel$digits==1
  jel$name[rows] = gsub(": General","",jel$name[rows],fixed=TRUE)
  jel.codes = jel
  
  tags.csv = read.csv(paste0(pkg.base.dir,"/tags.csv"), stringsAsFactors=FALSE)
  
  li = nlist(jel.codes, tags.csv, base.dir, download.dir, temp.dir, jis)
  
  file_types = readLines(paste0(pkg.base.dir,"/file_types.yaml"), warn=FALSE) %>% paste0(., collapse="\n") %>% yaml.load()
  file_types[["all_ext"]] = unique(unlist(do.call(c,file_types)))
  li$file_types = file_types
  
  setOption("ejd_options",li)
  copy.into.env(source=li, dest=globalenv())
  
}

get.ejd.opts = function() {
  opts = getOption("ejd_options")
  if (is.null(opts))
    stop("Please call init.journal.scrapper() to initialize the package options.")
  opts
}
 
get.jis = function(journ) {
  get.ejd.opts()$jis    
}


get.ji = function(journ) {
  get.ejd.opts()$jis[[journ]]    
}



examples.load.jis = function() {
  init.journal.scrapper()
  file = paste0(base.dir,"/journal_info.yaml")
}

load.jis = function(file = paste0(base.dir,"/journal_info.yaml")) {
  yaml = paste0(readLines(file, warn=FALSE), collapse="\n")
  jis = yaml.load(yaml)
  jis = lapply(jis, init.ji)
  jis
  
}

init.ji = function(ji) {
  restore.point("init.ji")
  ji$max_year = year(Sys.time())
  ji$max_vol = vol.of.year(ji$max_year, ji=ji)
  
  vols = ji$first_vol:ji$max_vol
  years = ji$first_year:ji$max_year
  vol.info = data_frame(journ=ji$short,year=years,vol=vols,  num_issues = ji$max_issues)
  
  if (length(ji$issue_month)>1) {
    prev.vol = ji$first_vol
    ivols = as.numeric(names(ji$issue_month))
    for (i in seq_along(ivols)) {
      prev.vol = ivols[i]
      vol = c(ivols[-1],ji$max_vol)[i]
      #cat("i=",i, "prev.vol=", prev.vol, "vol=",vol)
      rows = (prev.vol:vol)-ji$first_vol+1
      vol.info[rows,"num_issues"] <- length(ji$issue_month[[i]])
    }
  }
  
  ji$vol.info = vol.info
  vol.info$issue = lapply(vol.info$num_issues, function(num) 1:num)
  idt = unnest(vol.info, issue)
  idt$month = compute.article.month(vol = idt$vol,issue = idt$issue,ji=ji)
  idt$date = as.Date(paste0(idt$year,"-",idt$month,"-01"))
  idt = filter(idt, date <= as.Date(Sys.time()))
  ji$issue.dt = idt
  ji
}

examples.vol.of.year = function() {
  
  vol.of.year(2015,"aer")
}

vol.of.year = function(year=year(Sys.time()), journ, ji=get.ji(journ)) {
  year - ji$first_year + ji$first_vol  
}


compute.article.year = function(vol,issue=NULL, ji) {
  year = vol - ji$first_vol + ji$first_year
  year
}
compute.article.month = function(vol,issue, ji) {
  restore.point("compute.article.month")
  if (length(ji$issue_month)==0) {
    return(rep(NA,length(vol)))
  }
  month_vol = as.numeric(names(ji$issue_month))
  ind = findInterval(vol, month_vol)
  
  month=rep(NA, length(vol))
  
  for (mv in seq_along(ji$issue_month)) {
    im = unlist(ji$issue_month[[mv]])
    month[ind==mv] = im[issue[ind==mv]]
  }
  month
}

all.issues.data = function(journs = names(jis), jis=get.jis()) {
  li = lapply(journs, function(journ) {
    jis[[journ]]$issue.dt
  })
  arrange(bind_rows(li), desc(year), desc(journ), desc(issue))
  
}

find.missing.issues = function(journs = names(jis),dt = read.complete.data(), jis=get.jis()) {
  li = lapply(journs, function(journ) {
    issue.dt = jis[[journ]]$issue.dt
    missing = anti_join(issue.dt, dt, by=c("journ","year","vol","issue"))
    missing
  })
  arrange(bind_rows(li), desc(year), desc(journ), desc(issue))
}



scrap.info = function(dt=read.complete.data()) {
  dt = as.data.frame(dt)
  dt = arrange(dt, journ, vol, issue, articleNum)
  j.dt = summarise(group_by(dt,journ),
    num.art = NROW(id),
    num.vol = NROW(unique(vol)),
    first.vol = min(vol),
    last.vol = max(vol),
    first.year = min(year),
    last.year = max(year),
    missing.vol = gaps.string(vol),
    art.has.data = sum(is.true(data.size>0)),
    art.na.data = sum(is.na(data.size)),
    has.data.share = round(art.has.data / num.art,3)*100
  )
  
  jv.dt =  summarise(group_by(dt,journ,year,vol),
    num.art = NROW(id),
    num.issues = NROW(unique(issue)),
    missing.issue = gaps.string(issue,start=1),
    art.has.data = sum(is.true(data.size>0)),
    art.na.data = sum(is.na(data.size)),
    has.data.share = round(art.has.data / num.art,3)*100
  )
  
  # Find missing articles  
  d =  summarise(group_by(dt,journ,year,vol,issue),
    num.art = NROW(id),
    missing.art = gaps.list(articleNum,start=1)
  )
  missing.art = unnest(d,missing.art)

  # Find missing issues
  jis
  
}


get.journal.info = function(journ) {
  ji = get.ji(journ)
  cur.year = year(Sys.time())
  cur.vol = cur.year - ji$first_year + ji$first_vol
  c(ji, nlist(cur.year, cur.vol))
}

get.all.vol = function(journ) {
  ji = get.journal.info(journ)
  ji$first_vol:ji$cur.vol
}

get.current.vol = function(journ) {
  get.journal.info(journ)$cur.vol
}

get.all.journals = function(jis = get.ji) {
  names(jis)
}


add.article.date.info = function(art, vol=art$vol, issue=art$issue, ji=jis[[art$journ]]) {
  restore.point("add.article.date.info")
  
  art$year = compute.article.year(vol = vol, issue=issue, ji=ji)
  month = compute.article.month(vol=vol, issue=issue, ji=ji)
  art$date = as.Date(paste0(art$year,"-",month,"-01"))
  art
    
}
