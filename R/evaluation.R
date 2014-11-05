checkTags <- function(resample_tag, prediction_tag, ignore_tag){
  if (resample_tag != prediction_tag){
    if (ignore_tag){
      warning("Careful: There is a mismatch between prediction and resample tags.")
    } else {
      stop("There is a mismatch between prediction and resample tags.")
    }
  } 
}

computeLogScore <- function(prediction, target){
  as.numeric(rowSums(prediction * target))
}

computeLogScoreMultiClass <- function(resample_indexes, pred_obj){
  
  data <- mcGet(resample_indexes, "dataset")
  
  number_replicates <- mcGet(resample_indexes, "number_replicates")
  scores_all <- NULL
  
  for (i in 1:number_replicates){
    
    predictions_i <- mcGet(pred_obj, "prob", i)
    target_i <- mcGet(resample_indexes, "test_target", i)
    scores_all <- cbind(scores_all, computeLogScore(prediction = predictions_i, target = target_i))
    
  }
  
  return(scores_all)
  
} 

evaluateProbClass <- function(resample_indexes, pred_obj, type, ignore_tag = FALSE){
  
  resample_tag <- mcGet(resample_indexes, "tag")
  prediction_tag <- mcGet(pred_obj, "tag")
  
  checkTags(resample_tag = resample_tag, 
            prediction_tag = prediction_tag, 
            ignore_tag = ignore_tag)
    
  if (type == "log_score"){
    result <- computeLogScoreMultiClass(resample_indexes = resample_indexes, pred_obj = pred_obj)
  } else {
    stop("Invalid type.\n")
  }
      
  return(result)
  
}