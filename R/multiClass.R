#' Extract info from \code{multiClass} objects
#' 
#' @export
mcGet.multiClass <- function(x, attr, i = NULL){
  
  if (attr == "prob"){ # probability predictions
    
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
    
  } else if (attr == "number_replicates"){ # number of prediction sets
    
    return(length(x[["prob"]]))
    
  } else if (attr == "class_labels"){ # class labels
    
    return(x[["class_labels"]])
    
  } else if (attr == "number_classes"){ # number of classes
    
    return(length(x[["class_labels"]]))
    
  } else if (attr == "tag"){ # resample indexes tag
    
    return(x[["datasetResample_tag"]])
    
  } else if (attr == "summary_validation"){ # Validation Summary
    
    summary_validation <- x[["summary_validation"]]
    if (is.null(summary_validation)){
      return(NULL)
    } else if (is.null(i)){
      return(summary_validation)
    } else if (i == "parameter_names"){
      parameter_names <- colnames(summary_validation)
      parameter_names <- parameter_names[-c(1, length(parameter_names))]
      return(parameter_names)
    } else {
      stop("Invalid i option for summary_validation")
    }
    
  } else if (attr == "metric_name"){ # Metric name used to evaluate models
    
    return(x[["metric_name"]])
    
  } else {
    stop(attr, " not found.")
  }
  
}