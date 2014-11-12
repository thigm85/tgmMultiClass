installSuggestedPackage <- function(package_name){
  if (!require(package_name, character.only = TRUE, quietly = TRUE)){
    stop("Please, install ", package_name, " to use this function.")
  }
}

# Functions necessary to use dplyr with strings
# Source: https://gist.github.com/skranz/9681509
s_group_by = function(.data, ...) {
  eval.string.dplyr(.data,"group_by", ...)
}
eval.string.dplyr = function(.data, .fun.name, ...) {
  args = list(...)
  args = unlist(args)
  code = paste0(.fun.name,"(.data,", paste0(args, collapse=","), ")")
  df = eval(parse(text=code,srcfile=NULL))
  df  
}
