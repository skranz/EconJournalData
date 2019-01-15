
read_html = xml2::read_html



is.empty = function(x) {
  if (is.null(x)) return(TRUE)
  if (length(x)==0) return(TRUE)
  if (length(x)==1) {
    if (is.na(x)) return(TRUE)
    if (is.character(x)) {
      if (nchar(x)==0) return(TRUE)
    }
  }
  return(FALSE)
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

split.into.chunks = function(x, chunk.size) {
  split(x, ceiling(seq_len(NROW(x))/chunk.size))
}