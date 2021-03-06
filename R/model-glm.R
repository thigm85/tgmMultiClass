#' probability prediction with 'glm'
#' 
#' \code{predict_r_glm_prob} apply \code{glm} function to different training and test sets to predict class probability.
#' 
#' \code{predict_r_glm_prob} will then train with the training and validation
#' parts of the data using \code{glm} and output the predicted probabilities
#' on the test sets.
#' 
#' @param resample_indexes An object that contains information about resampled 
#'  instances of \code{data}, usually generated by \code{\link{generateTestIndexes}}.
#' @param verbose TRUE (default) for printing what is the current replication running. 
#' @param number_of_cores Number of cores to use in the foreach iteratins.
#' @param activate_unknown_category This is an experimental parameter. If set to
#'  TRUE, it will predict data points even if one of its categorical features have never been
#'  seen in the training of data. For this to make sense you need to manually set the
#'  contrast of each categorical variables to "contr.sum". E.g.:
#'  contrasts = list(categorical_1 = "contr.sum", categorical_1 = "contr.sum")). 
#'  Default = FALSE.
#' @param ... Arguments to be used in the \code{glm} function.
#' 
#' @details When predicting data points with 
#' 
#' @return It returns an S3 object of class c("multiClass_glm", "multiClass").
#' This object contains the predicted probabilities for each test set. See 
#' the examples to get more info about how to extract the desired info from 
#' this object.     
#' 
#' @seealso \code{\link{generateTestIndexes}}
#' 
# @examples
# data(soccer_game)
# indexes <- generateTestIndexes(dataset = soccer_game, 
#                                target_names = c("home.win", "home.draw", "home.lose"), 
#                                type = "3way", 
#                                options = list(prop_v = 0.2, 
#                                               prop_test = 0.2,
#                                               number_replicates = 4))
# pred_obj <- predict_r_vgam(resample_indexes = indexes,
#                            formula = cbind(home.win, home.draw, home.lose) ~ 1 + fair.odd.home + fair.odd.draw + fair.odd.away, 
#                            family = "multinomial")   
# 
# all_predictions <- mcGet(pred_obj, "prob")
# second_replication_only <- mcGet(pred_obj, "prob", 2)
#' 
#' @export
predict_r_glm_prob <- function(resample_indexes, verbose = TRUE, number_of_cores = NULL, 
                               activate_unknown_category = FALSE, ...){
  
  # data <- mcGet(resample_indexes, "dataset")
  
  tag <- mcGet(resample_indexes, "tag")
  
  number_replications <- mcGet(resample_indexes, "number_replicates")
  keep_probs <- list()
  
  registerDoMC(number_of_cores) # register parallel backend
  
  keep_probs <- foreach(index = 1:number_replications, .inorder=TRUE) %dopar% {
    trainAndPredict_glm_prob(index = index, resample_indexes = resample_indexes, 
                             verbose = verbose, number_replications = number_replications, 
                             activate_unknown_category = activate_unknown_category, ...)
  }
  
  result <- multiClass(resample_indexes = resample_indexes,
                       prob = keep_probs)
  
  return(result)
  
}

trainAndPredict_glm_prob <- function(index, resample_indexes, verbose, 
                                     number_replications, activate_unknown_category = FALSE, ...){
 
  if (verbose){
    print(paste("Iteration: ", index, "/", number_replications, sep = ""))
  }
  
  data <- mcGet(resample_indexes, "dataset")
  training_data <- data[c(mcGet(resample_indexes, "training", index), mcGet(resample_indexes, "validation", index)), ]
  
  fit <- do.call(what = "glm", args = list(data = training_data, ...))
  
  test_data <- data[mcGet(resample_indexes, "test", index), ]
  
  if (activate_unknown_category){
    categorical_variables <- names(fit$xlevels)
    for (i in categorical_variables){
      fit$xlevels[[i]] <- union(fit$xlevels[[i]], levels(as.factor(test_data[[i]])))  
    }
  }
  
  probs <- predict(fit, newdata = test_data, type = "response")  
  result <- c(as.numeric(probs), as.numeric(1 - probs))
  return(result)
  
}