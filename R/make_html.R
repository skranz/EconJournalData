# Steps:
# 
# 1. Scrap meta data from journal websites
#       parse.aer.volume
# 2. Download data appendices
# 3. Analyse data appendices and generate meta data 
# 4. Generate html sites

examples.make.articles.html = function() {
  setwd("D:/libraries/EconJournalData/")
  init.journal.scrapper() 
  db = get.articles.db()
  art = dbGet(db,"article")
  d = art %>%
    filter(has_data) %>%
    filter(date>="2014-12-01") %>%
    arrange(desc(size))
  
  simple_articles_html(d,"new_articles.html", need.data=TRUE)
 
  
  df = filter(art, has.substr(title,"In a Small Moment")) 
}

simple_articles_html = function(art,file,  fs = dbGet(db,"files_summary"), db=get.articles.db(), need.data=FALSE, add.jel=FALSE, readme.base.url = "") {
  restore.point("simple_articles_html")
  d = add_code_and_data_str(d, fs)

  if (need.data) {
    d = filter(d,data_mb>0 | archive_mb >0)
  }
  readme = ifelse(!is.na(d$readme_file),
    paste0(' (<a href="', readme.base.url,"/", d$readme_file,'" target="_blank">README</a>)'),"")
  
  str = paste0('<p><a href="', d$article_url,'">','<span class="title">',d$title,'</span>','</a>',
    ' (', signif(d$size,2),' ', d$unit, ' ', d$journ,', ',format(d$date,"%Y %b"),') <BR>', d$data_code_str, readme,
    '</p>') 
  
  writeLines(str, file)
  
}

add_code_and_data_str = function(art, fs, opts=get.ejd.opts()) {
  restore.point("add_code_and_data_str")
  
  fs = filter(fs, id %in% art$id)

  
  # Code string
  s = fs %>% group_by(id) %>%
    summarize(
      archive_mb = sum(mb[fs$file_type %in% opts$file_types$archive_ext]),
      num_data = sum(is_data),
      data_mb = sum(mb[is_data]),
      num_code = sum(is_code),
      code_str = paste0(file_type[is_code]," ",signif(mb[is_code]*1000,3), " KB", collapse=", ")) %>%
    mutate(archive_mb = ifelse(is.na(archive_mb),0,archive_mb)) %>%
    ungroup()
  
  
  
  s$code_str[s$num_code==0] = "no code files"
  
  s$data_str = ifelse(s$data_mb > 1000,
      paste0(signif(s$data_mb/1000,3), " GB"),
      paste0(signif(s$data_mb,3), " MB")
    )
  s$archive_str = ifelse(s$archive_mb > 0,paste0("Compressed ", signif(s$archive_mb,3)," MB "),"")
  s$data_code_str = paste0(s$archive_str, "Data: ", s$data_str, ", Code: ", s$code_str)
  
  art = left_join(art, s[,c("id","data_code_str","data_mb","archive_mb")], by="id")
  art
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
