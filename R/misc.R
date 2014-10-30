installSuggestedPackage <- function(package_name){
  if (!require(package_name, character.only = TRUE, quietly = TRUE)){
    stop("Please, install ", package_name, " to use this function.")
  }
}