#' Extract info from \code{multiClass} objects
#' 
#' @export
mcGet.multiClass <- function(x, attr, i = NULL){
  
  if (attr == "prob"){
    if (is.null(i)){
      return(x[["prob"]])  
    } else {
      if (i >= 1 & i <= ncol(x[["prob"]])){
        result <- matrix(x[["prob"]][, i], ncol = length(x[["class_labels"]]), byrow = FALSE)
        colnames(result) <- x[["class_labels"]]
        return(result)
      } else {
        stop("wrong index i.")
      }
    }
  } else if (attr == "class_labels"){
    return(x[["class_labels"]])
  } else {
    stop(attr, " not found.")
  }
  
}