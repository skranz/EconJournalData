# Steps:
# 
# 1. Scrap meta data from journal websites
#       parse.aer.volume
# 2. Download data appendices
# 3. Analyse data appendices and generate meta data 
# 4. Generate html sites

examples.make.artciles.html = function() {
  setwd("D:/libraries/EconJournalData")
  init.journal.scrapper()

  df = read.complete.data()
  
  rows = is.true(df$data.size>0.01) | is.na(df$data.size)
  d = df[rows,]
  ord = order(-d$data.size)
  d = d[ord,]
  d$code.str = make.code.str(d)
  
  d = filter(d, year*100+month >= 201405, journ == "restud")
  
  file="new_restud_articles.html"
  make.long.html(d, file=file)
  browseURL(paste0("file://", getwd(), "/",file))

  
  file="new_non_aer_jel_articles.html"
  make.JEL.html(d, file=file)
  browseURL(paste0("file://", getwd(), "/",file))


  file="fav_articles.html"
  make.favorite.html(d, file=file)
  browseURL(paste0("file://", getwd(), "/",file))
  
  
}

display = cat

make.articles.html = function() {
  setwd("D:/libraries/EconJournalData")
  init.journal.scrapper()

  df = read.complete.data()
  
  rows = is.true(df$data.size>0.01) | is.na(df$data.size)
  d = df[rows,]
  ord = order(-d$data.size)
  d = d[ord,]
  
  d$code.str = make.code.str(d)
  
  file="jel_articles.html"
  make.JEL.html(d, file=file)
  #browseURL(paste0("file://", getwd(), "/",file))

  file="all_articles.html"
  make.long.html(d, file=file)
  #browseURL(paste0("file://", getwd(), "/",file))

  file="fav_articles.html"
  make.favorite.html(d, file=file)
  #browseURL(paste0("file://", getwd(), "/",file))

  dfav = parse.favorite.articles(d)
  file="jel_fav_articles.html"
  make.JEL.html(dfav, file=file)

}

robust.rbindlist = function(li) {
  restore.point("robust.rbindlist")
  
  not.null = !sapply(li, is.null)
  li = li[not.null]
  cols = Reduce(intersect,lapply(li, function(li) names(li)) )
  ili = lapply(li, function(li) li[cols])
  rbindlist(ili)
}

file_ext = function (x) 
{
    pos <- regexpr("\\.([[:alnum:]]+)$", x)
    ifelse(pos > -1L, substring(x, pos + 1L), "")
}

make.code.str = function(dt, ext=c("do","ado", "m","py","r","nb", "mod", "lng", "gms", "c","cpp","java", "f95","sas","prg","g","tsp", "ztt", "zip")) {
  restore.point("make.code.str")
  
  cols = paste0("size.",ext)
  li = lapply(cols, function(col) {
    txt = ifelse(dt[[col]]>0,
          paste0(" ",str.right.of(col,"size."), ": ", round(dt[[col]]*1000,0)),
          "")
    #txt[is.na(txt)] = ""
  })
  str = li[[1]]
  for (i in seq_along(cols)[-1]) {
    str = paste0(str, li[[i]])
  }
  str = ifelse(nchar(str)>0,paste0("Code in KB:", str),"")
  str[is.na(li[[1]])] = "Data and code not yet analysed."
  rows = which(!is.na(dt$data.files.size))
  if (length(rows)>0) {
    str[rows] = paste0(str[rows],";  Data (decompressed): ", round(dt$data.files.size[rows],1)," MB")
  }
  str
}

articles.html = function(d) {
  restore.point("articles.html")
  local.url = paste0(data.dir,"/",d$journ,"_vol_",d$vol,"_issue_",d$issue,"_article_",d$articleNum,".zip")
  
  data.url = ifelse(nchar(d$data.url)>0,
    paste0('<a href="file://',local.url,'">  (downloaded zip) </a>'),
    "")
  
  str = paste0('<p><a href="', d$url,'">',d$title,'</a>',
    ' (', signif(d$data.size,4),' MB, ' , d$journ,', ', d$publication.dat,') <BR>', d$JEL,
    '<BR> ', d$code.str, data.url ,  
    '</p>') 
} 

make.JEL.html = function(dt, file="jel_articles.html") {
  restore.point("make.JEL.html")

  jel = get.jel.codes()
  
  txt = NULL
  dt$code.str = make.code.str(dt)
  df = as.data.frame(dt)
  
  
  hli = lapply(1:NROW(jel), function(jelNum) {
    code = jel$code[jelNum]
    
    restore.point("gdzfgd")
    rows = NULL
    for (i in 1:7) {
      rows = c(rows,which(str.starts.with(df[,paste0("JEL",i)],code)))
    }
    rows = unique(rows)
    
    if (length(rows)>0) {
      d = dt[rows,]
      d = d[order(-d$data.size),]
       str = articles.html(d)
    } else {
      str = "<p> No article </p>"
    }
    str
    
    label = jel$label[jelNum]
    head = paste0('<p><H2><a id="jel',code,'">', code,': ',label,'</a></H2></p>')
    txt = paste0(head, paste0(str, collapse="\n"))
    txt
  })
  names(hli) = jel$code
  
  links = paste0('<a href="#jel', jel$code,'">', jel$code,': ',jel$label,'</a><br>')
  
  txt = paste0(paste0(links, collapse=""), paste0(hli, collapse="\n<br>"))
  
  content.txt =  merge.lines(txt, collapse="\n")
  body.txt = paste0('<body>', wrap.in.bootstrap.div(content.txt),"</body>")
  header.txt = paste0("<header>", html.css.imports(),'\n', html.script.imports(), "</header>", sep="\n")
  txt = paste0(header.txt, body.txt, sep="\n")
  
  if (!is.null(file))
    writeLines(txt,file)
  invisible(txt)
  
}


make.long.html = function(dt, file="articles.html") {
  restore.point("make.html")

  
  txt = articles.html(dt)
  txt
  content.txt =  merge.lines(txt, collapse="\n")
  body.txt = paste0('<body>', wrap.in.bootstrap.div(content.txt),"</body>")
  header.txt = paste0("<header>", html.css.imports(),'\n', html.script.imports(), "</header>", sep="\n")
  txt = paste0(header.txt, body.txt, sep="\n")
  
  if (!is.null(file))
    writeLines(txt,file)
  invisible(txt)
  
}

examples.write.favorite.articles = function() {
  init.journal.scrapper()
  write.favorite.articles()
  dfav = read.favorite.articles()
  dfav
} 

write.favorite.articles = function(dt = read.complete.data(),
                                       fav.file = paste0(base.dir,"/favorite articles.txt"),
                                       csv.file = paste0(base.dir,"/favorite articles.csv")) {
  dfav = parse.favorite.articles(dt, fav.file)
  write.csv(dfav,csv.file,row.names=FALSE)  
}

read.favorite.articles = function(csv.file = paste0(base.dir,"/favorite articles.csv")) {
  read.csv(csv.file)  
}


parse.favorite.articles = function(dt, fav.file = paste0(base.dir,"/favorite articles.txt")) {
  restore.point("get.favorite.articles")

  fav = str.trim(readLines(fav.file))
  fav = fav[nchar(fav)>0]
  header.rows = str.starts.with(fav,"#")
  art.rows = !header.rows  
  
  art = fav[art.rows]
  titles = str.left.of(art, " (")
  titles[is.na(titles)] = art[is.na(titles)]
  
  rows = match(titles, dt$title)
  if (any(is.na(rows))) {
    display("The following titles in favorite articles could not be matched and are ignored: ")
    print(art[which(is.na(rows))])  
  }
  d = dt[rows,]  
  d
}

make.favorite.html = function(dt, file="favorite_articles.html",fav.file = "favorite articles.txt") {
  restore.point("make.html")

  fav = str.trim(readLines(fav.file))
  fav = fav[nchar(fav)>0]
  header.rows = str.starts.with(fav,"#")
  art.rows = !header.rows  
  
  art = fav[art.rows]
  titles = str.left.of(art, " (")
  titles[is.na(titles)] = art[is.na(titles)]
  
  rows = match(titles, dt$title)
  if (any(is.na(rows))) {
    display("The following titles in favorite articles could not be matched and are ignored: ")
    print(art[which(is.na(rows))])  
  }
  d = dt[rows,]  
  txt = articles.html(d)
  
  fav[art.rows] = txt
  fav[which(art.rows)[is.na(rows)]] = ""
  
  h = fav[header.rows]
  h = str.trim(gsub("#","", h, fixed = TRUE))
  h = paste0("<h2>", h, "</h2>")
  fav[header.rows] = h
  
  fav = c("<h1> Selected articles with data and code available </h1>",fav)
  content.txt =  merge.lines(fav, collapse="\n")
  
  
  body.txt = paste0('<body>', wrap.in.bootstrap.div(content.txt),"</body>")
  header.txt = paste0("<header>", html.css.imports(),'\n', html.script.imports(), "</header>", sep="\n")
  txt = paste0(header.txt, body.txt, sep="\n")
  
  if (!is.null(file))
    writeLines(txt,file)
  invisible(txt)
  
}


wrap.in.bootstrap.div = function(content="", left="", left.size=1) {
  txt = paste0('
               <div class="container-fluid"><div class="row-fluid">
               <div class="span', left.size,'">
               ', left, '
               </div>
               <div class="span',12-left.size,'">
               ',content,'
               </div>
               </div></div>
               ')
  txt
}



html.css.imports = function(bootstrap=TRUE, accordion=FALSE, DataTables=FALSE, jquery=bootstrap | accordion | DataTables) {
  txt = ''
  if (bootstrap)
    txt = paste0(txt,'\n<link href="js/bootstrap/css/bootstrap.min.css" rel="stylesheet" media="screen">')
  if (jquery)
    txt = paste0(txt,'
                 ')             
  
  if (accordion)
    txt = paste0(txt,'
                 <link rel="stylesheet" href="http://code.jquery.com/ui/1.10.3/themes/smoothness/jquery-ui.css" />
                 <link rel="stylesheet" href="/resources/demos/style.css" />
                 ')
  if (DataTables)
    txt = paste0(txt,'
                 <style type="text/css" title="currentStyle">
                 @import "js/datatables/media/css/demo_page.css";
                 @import "js/datatables/media/css/demo_table.css";
                 </style>
                 ')
  txt
}

html.script.imports = function(bootstrap=TRUE, accordion=FALSE, DataTables=FALSE, jquery=bootstrap | accordion | DataTables) {
  txt = ''
  if (bootstrap)
    txt = paste0(txt,'
                 <script src="js/bootstrap/js/bootstrap.min.js"></script>
                 ')             
  if (jquery)
    txt = paste0(txt,'
                 <script src="js/jquery/js/jquery.min.js"></script>
                 <script src="http://code.jquery.com/ui/1.10.3/jquery-ui.js"></script>                 
                 ')
  if (accordion)
    txt = paste0(txt,'
                 ')
  if (DataTables)
    txt = paste0(txt,'
                 <script type="text/javascript" language="javascript" src="js/datatables/media/js/jquery.dataTables.js"></script>
                 ')
  txt
}  

get.jel.codes = function() {
    
  txt =
  'A\tGeneral Economics and Teaching
  B  History of Economic Thought, Methodology, and Heterodox Approaches
  C	Mathematical and Quantitative Methods
  D	Microeconomics
  E	Macroeconomics and Monetary Economics
  F	International Economics
  G	Financial Economics
  H	Public Economics
  I	Health, Education, and Welfare
  J	Labor and Demographic Economics
  K	Law and Economics
  L	Industrial Organization
  M	Business Administration and Business Economics . Marketing . Accounting
  N	Economic History
  O	Economic Development, Technological Change, and Growth
  P	Economic Systems
  Q	Agricultural and Natural Resource Economics . Environmental and Ecological Economics
  R	Urban, Rural, Regional, Real Estate, and Transportation Economics
  Y	Miscellaneous Categories
  Z	Other Special Topics'
  
  jel = read.csv(textConnection(sep.lines(txt,"\n")),sep="\t", header=FALSE, stringsAsFactors=FALSE)
  colnames(jel)=c("code","label")
  jel$code = str.trim(jel$code)
  jel$label = str.trim(jel$label)
  jel
}
