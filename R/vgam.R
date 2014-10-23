#' multi-class prediction probability with 'vgam'
#' 
#' \code{predict_r_vgam} apply \code{vgam} function to different training and test sets.
#' 
#' Different datasets are formed from \code{data} according to the indexes
#' contained in \code{training}, \code{validation} and \code{test}. 
#' \code{predict_r_vgam} will then train with the training and validation
#' part of the data using \code{vgam} and output the predicted probabilities
#' on the test sets.
#' 
#' @param data A data.frame containing the response variable and the features
#'  used for fitting and predictions.
#' @param training A matrix where each column contain integers that indicate which lines
#'  of 'data' will be used for training.
#' @param validation Similar to \code{training} but contain the lines used for validation,
#'  in case there are tuning parameters.
#' @param test Similar to \code{training} but contain the lines used for test.
#' @param ... Arguments to be used in the \code{vgam} function.
#' 
#' @return It returns an S3 object of class c("multiClass_vgam", "multiClass").
#' This object contains the predicted probabilities for each test set. See 
#' the examples to get more info about how to extract the desired info from 
#' this object.     
#' 
#' @examples
#' 
predict_r_vgam <- function(data, training, validation, test, ...){
  
  number_replications <- ncol(training)
  keep_probs <- NULL
  
  for (i in 1:number_replications){

    training_data <- data[c(training[, i], validation[, i]), ]

    fit <- do.call(what = "vgam", args = list(data = training_data, ...))
    
    probs <- predict(fit, newdata = data[test[,i], ], type = "response")  
    keep_probs <- cbind(keep_probs, c(probs))    
    
  }
  
  class_labels <- colnames(probs)
  result <- list(class_labels = class_labels, prob = keep_probs)
  class(result) <- c("multiClass_vgam", "multiClass")
  
  return(result)
  
}

if (FALSE){

  load_all("~/projects/sw/R/tgmMultiClass")
  raw_dataset <- loadRawDataset(folder_path = "~/projects/sw/sport/app/brasileiro_serie_A/data/brazil_A_1011")
  soccer_game <- raw_dataset[, c("match.id", "home", "away", "scores.home", "scores.away", 
                                 "fair.odd.home", "fair.odd.draw", "fair.odd.away", "results.home", 
                                 "home.win", "home.draw", "home.lose")]
  
  soccer_game <- tail(soccer_game, 380)
  indexes <- generateTestIndexes(dataset = soccer_game, 
                                 target_names = c("home.win", "home.draw", "home.lose"), 
                                 type = "3way", 
                                 options = list(prop_v = 0.2, 
                                                prop_test = 0.2,
                                                number_replicates = 3))
  
  
  # criar um bundle junto com training, validation and test
  
  indexes <- loadTestIndexes(folder_path = "~/projects/sw/sport/app/brasileiro_serie_A/data/brazil_A_1011")
  
  r <- predict_r_vgam(data = raw_dataset, 
                      training = indexes$training, 
                      validation = indexes$validation, 
                      test = indexes$test,
                      formula = cbind(home.win, home.draw, home.lose) ~ 1 + fair.odd.home + fair.odd.draw + fair.odd.away, 
                      family = "multinomial")
  
}
