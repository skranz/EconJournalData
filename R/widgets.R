
radioGroupButtonsVector = function(id,values, choices, labels=choices, row.ids=seq_along(values), size="sm", inline=TRUE) {
  restore.point("radioGroupButtonVector")
  n = length(values)
  inner = rep("", n)
  long.row.ids = paste0(id, "_", row.ids)
  checked.col = match(values, choices)
  checked.col[is.na(checked.col)] = 0
  for (i in seq_along(choices)) {
    str = paste0('<button class="btn btn-default', ifelse(checked.col==i,' active',''),'"><input type="radio" autocomplete="off" name="',long.row.ids,'" value="', choices[i],'"',ifelse(checked.col==i,' checked="checked"',''), '/>',labels[i],'</button>')
    inner = paste0(inner, str)
  }
  res = paste0('<', if (inline) 'span' else 'div', ' class="',id,' radioGroupButtons btn-group-container-sw btn-group btn-group-toggle btn-group-', size,'" role="group" data-toggle="buttons">', inner,if (inline) '</span>' else '</div>')
  res
}
