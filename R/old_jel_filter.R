
get.selected.jel.or = function(jel1=app$opt$jel1, jel2=app$opt$jel2, jel3=app$opt$jel3, jel=app$glob$jel, app=getApp()) {
  restore.point("get.selected.jel")
  
  if (!is.null(jel1)) {
    jel = filter(jel,digit1 %in% jel1)
  }
  if (!is.null(jel2)) {
    d1 = substring(opt$jel2,1,1)
    free.jel1 = setdiff(opt$jel1,d1)
    jel = filter(jel,digit12 %in% opt$jel2 | digit1 %in% free.jel1)
  }
  if (!is.null(jel3)) {
    jel = filter(jel,code %in% jel3)
  }
  jel
}

filter.articles = function(adf,ajel.dt,journals=NULL,jel1=NULL,...) {
  restore.point("filter.articles")
  dt = as.data.table(adf)
  changed=FALSE
  if (!is.null(jel1)) {
    library(dplyr)
    id = unique(ajel.dt[jel1]$id)
    setkey(dt,id)
    dt = dt[id]
    changed=TRUE
  }
  if (changed) {
    ord = order(-dt$data.size)
    dt = dt[ord,]
  }
  dt
}

uiJEL = function(jel) {
  restore.point("uiStagesAccordion")
  
  jel1 = jel[jel$digits==1,] 
  panels.html = lapply(1:NROW(jel1), function(i){
    restore.point("hfjfj")
    je = jel1[i,,drop=FALSE]
    name = je$code
    button <- paste0(
      '<input type="checkbox" id="',paste0("checkJEL_",name),'" checked="checked">',
       actionButton(paste0("onlyJEL_",name),bsGlyph("icon-arrow-right"))
    )
    #button = checkboxInput(inputId=paste0("checkJEL_",name), label="", value = TRUE)
    title = paste0("<h4> ",name,': ', je$label,"</h4>")
    txt = paste0('<div>\n', "Hi",'\n</div>')
    skCollapsePanel(HTML(title),HTML(txt),titleUI=HTML(button), id=name)
  })
  panels.html[[1]]
  args = c(list(multiple = FALSE, open = NULL, id = "jelAccordion"), panels.html)
  do.call(bsCollapse,args)
}
