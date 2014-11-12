# Getter for S3 objects

#' Generic function to extract object elements
#' 
#' Standard function to be used when extracting information from objects.
#' 
#' @export
mcGet <- function(x, attr, ...) UseMethod("mcGet")

#' Generic function to modify object elements
mcSet <- function(object, attr, new.attr, ...) UseMethod("mcSet")

#' Generic function to summarize validation results
summarizeValidation <- function(fitted_model, ...) UseMethod("summarizeValidation")