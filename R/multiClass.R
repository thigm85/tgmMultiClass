#' Extract info from \code{multiClass} objects
#' 
#' @export
mcGet.multiClass <- function(x, attr, i = NULL){
  
  if (attr == "prob"){
    if (is.null(i)){
      return(x[["prob"]])  
    } else {
      if (i >= 1 & i <= length(x[["prob"]])){
        result <- matrix(x[["prob"]][[i]], ncol = length(x[["class_labels"]]), byrow = FALSE)
        colnames(result) <- x[["class_labels"]]
        return(result)
      } else {
        stop("index i out of range.")
      }
    }
  } else if (attr == "class_labels"){
    return(x[["class_labels"]])
  } else if (attr == "tag"){
    return(x[["datasetResample_tag"]])
  } else {
    stop(attr, " not found.")
  }
  
}