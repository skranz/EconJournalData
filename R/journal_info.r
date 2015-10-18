examples.load.jis = function() {
  init.journal.scrapper()
  file = paste0(base.dir,"/journal_info.yaml")
}



load.jis = function(file = paste0(base.dir,"/journal_info.yaml")) {
  jis = yaml.load_file(file)
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
      cat("i=",i, "prev.vol=", prev.vol, "vol=",vol)
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

vol.of.year = function(year=year(Sys.time()), journ, ji=jis[[journ]]) {
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

all.issues.data = function(journs = names(jis)) {
  li = lapply(journs, function(journ) {
    jis[[journ]]$issue.dt
  })
  arrange(bind_rows(li), desc(year), desc(journ), desc(issue))
  
}

find.missing.issues = function(journs = names(jis),dt = read.complete.data()) {
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

