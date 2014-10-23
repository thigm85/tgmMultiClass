mcGet.multiClass <- function(x, attr){
  
  if (attr == "prob"){
    return(x[["prob"]])
  } else if (attr == "class_labels"){
    return(x[["class_labels"]])
  } else {
    stop(attr, " not found.")
  }
  
}