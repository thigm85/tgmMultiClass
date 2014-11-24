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
#' 
#' @export
summarizeValidation <- function(fitted_model, ...) UseMethod("summarizeValidation")

#' Evaluate predicted class probabilities for a given score function.
#' 
#' @export
evaluateProbClass <- function(pred_obj, resample_indexes, type, ignore_tag = FALSE, ...) UseMethod("evaluateProbClass")

#' Summarize Validation metric
#' 
#' @export
summarizeValidationMetric <- function(validation_scores, 
                                      previous_summary = NULL, ...) UseMethod("summarizeValidationMetric")