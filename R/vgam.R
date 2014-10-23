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

  raw_dataset <- loadRawDataset(folder_path = "~/projects/sw/sport/app/brasileiro_serie_A/data/brazil_A_1011")
  indexes <- loadTestIndexes(folder_path = "~/projects/sw/sport/app/brasileiro_serie_A/data/brazil_A_1011")
  
  r <- predict_r_vgam(data = raw_dataset, 
                      training = indexes$training, 
                      validation = indexes$validation, 
                      test = indexes$test,
                      formula = cbind(home.win, home.draw, home.lose) ~ 1 + fair.odd.home + fair.odd.draw + fair.odd.away, 
                      family = "multinomial")
  
}
