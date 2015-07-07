#' Training with Vowpal Wabbit
#'
#' @param data_train_path The path for the training data.
#' @param loss_function Loss function to use for training. Options are: \code{logistic}.
#' @param regressor_file Path of the file to write regressor estimates to.
#' @param readable_model Set to TRUE to output \code{regressor_file} that is human-readable with feature numbers. 
#'  Default to FALSE.
#' @param invert_hash Set to TRUE to output \code{regressor_file} that is human-readable with feature names. 
#'  Default to FALSE.
#' @param holdout If set to FALSE then there is no holdout data in multiple passes. Default to FALSE.
#' @param cache_file Cache file path. It is necessary in case of multiple passes.
#' @param number_of_passes How many passes through the data. Default to 1.
#' @param learning_rate Learning rate used in the OGD. 
#' @param bits Number of bits in the feature table.
RvwTrain <- function(data_train_path, loss_function = c("logistic"), ignore_features = NULL,
                     regressor_file = NULL, readable_model = FALSE, invert_hash = FALSE,
                     holdout = FALSE, cache_file = NULL, number_of_passes = 1, learning_rate = NULL,
                     bits = 18, l1 = NULL){

  # training data
  args_vector <- paste("-d ", data_train_path, sep = "")
  
  # ignore features
  if (!is.null(ignore_features)){
    args_vector <- c(args_vector, paste("--ignore", ignore_features))
  }
  
  # number of bits
  args_vector <- c(args_vector, paste("-b", as.numeric(bits)))
  
  # loss function
  loss_function <- match.arg(loss_function)
  args_vector <- c(args_vector, paste("--loss_function ", loss_function, sep = ""))
  
  # holdout
  if (!holdout){
    args_vector <- c(args_vector, "--holdout_off")
  }
  
  # cache file
  cache_activated <- FALSE
  if (!is.null(cache_file)){
    args_vector <- c(args_vector, paste("--cache_file ", cache_file, sep = ""))
    cache_activated <- TRUE
  }
  
  # number of passes
  if (number_of_passes > 1 & is.null(cache_file)){
    args_vector <- c(args_vector, paste("-c --passes ", number_of_passes, sep = ""))
    cache_activated <- TRUE
  } else {
    args_vector <- c(args_vector, paste("--passes ", number_of_passes, sep = ""))  
  }
  
  # learning rate
  if (!is.null(learning_rate)){
    args_vector <- c(args_vector, paste("-l ", as.numeric(learning_rate), sep = ""))  
  }

  # L1 regularization parameter
  if (!is.null(l1)){
    args_vector <- c(args_vector, paste("--l1 ", as.numeric(l1), sep = ""))  
  }
  
  # file with regressor file
  regressor_option <- "-f "
  if (is.null(regressor_file)){
    regressor_file <- "vw_model_file_default"
  }
  if (readable_model){
    regressor_option <- "--readable_model "
  } 
  if (invert_hash){
    if (cache_activated){
      warning("Cannot use 'invert_hash' option with a cache file activated. Disabling invert_hash option.")
    } else {
      regressor_option <- "--invert_hash "      
    }
  } 
  args_vector <- c(args_vector, paste(regressor_option, regressor_file, sep = ""))
  
  system2(command = "vw", args = args_vector)
  
  result <- list(regressor_file = regressor_file,
                 data_train_path = data_train_path)
  return(result)

}

RvwTest <- function(vw_fit, data_test_path, link_function = c("logistic")){
  
  # test data
  args_vector <- paste("-d ", data_test_path, sep = "")

  # use previously trained model
  trained_model_path <- vw_fit$regressor_file
  args_vector <- c(args_vector, paste("-t -i ", trained_model_path, sep = ""))
  
  # link function
  link_function <- match.arg(link_function)
  args_vector <- c(args_vector, paste("--link ", link_function, sep = ""))

  # raw prediction file
  raw_prediction_path <- tempfile(tmpdir = ".")
  args_vector <- c(args_vector, paste("-r ", raw_prediction_path, sep = ""))
  
  system2(command = "vw", args = args_vector)
  
  result <- list(link_function = link_function)
  
  # normalized prediction file
  raw_predictions <- read.table(raw_prediction_path, header = FALSE)
  if (link_function == "logistic"){
    normalized_predictions <- exp(raw_predictions[,1])/(1 + exp(raw_predictions[,1]))  
  }
  
  result$normalized_predictions <- normalized_predictions

  file.remove(raw_prediction_path)
  
  return(result)
  
}
# 
# vw_fit <- RvwTrain(data_train_path = "~/drive/projects/yahoo/2015-2/click_vw_experiments/data/20150501_20150629-click_data_vw",
#                    loss_function = "logistic", 
#                    regressor_file = "~/drive/projects/yahoo/2015-2/click_vw_experiments/data/trained_model_file",
#                    holdout = FALSE,
#                    number_of_passes = 5,
#                    learning_rate = 0.5)#, 
#                    #ignore_features = c("a", "t", "c"),
#                    #invert_hash = TRUE)
# 
# vw_test <- RvwTest(vw_fit = vw_fit,
#                    data_test_path = "~/drive/projects/yahoo/2015-2/click_vw_experiments/data/20150501_20150629-click_data_vw", 
#                    link_function = "logistic")
# 
# data <- readLines("~/drive/projects/yahoo/2015-2/click_vw_experiments/data/20150501_20150629-click_data_vw")

#' multi-class prediction probability with Vowpal-Wabbit
#' 
#' \code{predict_vw} will then train with the training and validation parts 
#' of the data using \code{vw} and output the predicted probabilities on the 
#' test sets. A score function should be provided to define what 'best' means
#' in terms of prediction.
#' 
#' @export
predict_vw <- function(resample_indexes, tune_grid, metric_name = "log_score", verbose, number_of_cores = NULL, 
                       summarize_function = function(x) mean(x, na.rm = TRUE),                           
                       pre_process_validation_set = NULL,
                       args_pre_validation = NULL,
                       platt_calibration = FALSE,
                       platt_train_calibration = 1/6,
                       ### amount of training data set aside for calibration set in the platt method
                       ...){
  
  library(doMC)
  library(foreach)
  
  number_tune_models <- nrow(tune_grid)
  
  number_replicates <- mcGet(resample_indexes, "number_replicates")
  resample_indexes$dataset_vw <- readLines(con = resample_indexes$vw_data_path)
    
  summary_validation_metric <- NULL
  prediction_test <- list()  
  
  for (i in 1:number_replicates){
    
    # compute validation predictions
    prediction_validation <- ComputeValidationPrediction(replicate_index = i, 
                                                         resample_indexes = resample_indexes,
                                                         tune_grid = tune_grid, 
                                                         verbose = verbose,
                                                         number_of_cores = number_of_cores,
                                                         platt_calibration = platt_calibration,
                                                         platt_train_calibration = platt_train_calibration,
                                                         ...)
    
    # compute validation scores
    validation_scores <- evaluateProbClass(pred_obj = prediction_validation, 
                                           resample_indexes = resample_indexes, 
                                           type = metric_name,
                                           replicate_index = i)
    
    # memory of the overall validation process
    summary_validation_metric <- summarizeValidationMetric(validation_scores = validation_scores,
                                                           previous_summary = summary_validation_metric,
                                                           summarize_function = summarize_function)
    
    # pick the best model
    best_model <- pickBestValidationModel(validation_scores = validation_scores,
                                          summarize_function = summarize_function)
    
    # predict test set with best model
    data_train <- resample_indexes$dataset_vw[c(mcGet(resample_indexes, "training", i), mcGet(resample_indexes, "validation", i))]
    data_train_path <- tempfile(tmpdir = ".")
    writeLines(data_train, data_train_path)
  
    data_test <- resample_indexes$dataset_vw[mcGet(resample_indexes, "test", i)]
    data_test_path <- tempfile(tmpdir = ".")
    writeLines(data_test, data_test_path)    
    
    if (!platt_calibration){
      prediction_test[[i]] <- trainAndPredict(tuneGrid = tune_grid[best_model, ], 
                                              data_train_path = data_train_path, 
                                              data_test_path = data_test_path, ...)
    } else if (regtree_loss == "logistic") {
      
      tune_grid_platt <- mcGet(validation_scores, "tune_grid")
      prediction_test[[i]] <- trainAndPredict_Platt(tuneGrid = tune_grid_platt[best_model, ], 
                                                    data_train = data_train, data_test = data_test, 
                                                    regtree_loss = regtree_loss,
                                                    ...)
      
      
    } else {
      
      stop("platt method only allowed for logistic regression.")
      
    }
    
    file.remove(data_train_path)
    file.remove(data_test_path)
    
  }
  
  # create multiClass object
  pred_obj <- multiClass(resample_indexes = resample_indexes, 
                         prob = lapply(X = prediction_test, FUN = function(x) c(x, 1 - x)), 
                         summary_validation = summary_validation_metric, 
                         metric_name = metric_name)
  
  return(pred_obj)  
  
}

#' compute validation predictions
#' 
#' @export
ComputeValidationPrediction <- function(replicate_index, resample_indexes, 
                                        tune_grid, verbose, number_of_cores = NULL,
                                        platt_calibration = platt_calibration,
                                        platt_train_calibration = platt_train_calibration,
                                        ...){
  
  registerDoMC(number_of_cores) # register parallel backend
  
  number_tune_models <- nrow(tune_grid)
  
  number_replicates <- mcGet(resample_indexes, "number_replicates")
  
  data_train <- resample_indexes$dataset_vw[mcGet(resample_indexes, "training", i=replicate_index)] 
  data_train_path <- tempfile(tmpdir = ".")
  
  data_validation <- resample_indexes$dataset_vw[mcGet(resample_indexes, "validation", i=replicate_index)]
  data_validation_path <- tempfile(tmpdir = ".")
  
  if (!platt_calibration){

    writeLines(data_train, data_train_path)
    writeLines(data_validation, data_validation_path)
    
    # generates a validation matrix
    metrics_tune_models <- foreach(index = 1:number_tune_models, .inorder=TRUE) %dopar% {
      tuneModel(index = index, tuneGrid = tune_grid, verbose = verbose, replicate_index = replicate_index,
                data_train_path = data_train_path, data_test_path = data_validation_path, ...) 
    }
  
  } else {
    
    # divide data in 3 parts
    index_calibration <- sample(x = 1:length(data_train), 
                                size = length(data_train)*platt_train_calibration,
                                replace = FALSE)
    platt_data_train <- data_train[-index_calibration] 
    writeLines(platt_data_train, data_train_path)
    
    platt_data_calibration <- data_train[index_calibration]
    data_calibration_path <- tempfile(tmpdir = ".")
    writeLines(platt_data_calibration, data_calibration_path)
    
    platt_data_validation <- data_validation  
    writeLines(platt_data_validation, data_validation_path)
    
    # ok, platt method will be used
    platt_validation_obj <- foreach(index = 1:number_tune_models, .inorder=TRUE) %dopar% {
      tuneModel_Platt(index = index, tuneGrid = tune_grid, verbose = verbose, 
                      data_train = data_train_path, data_calibration = data_calibration_path, 
                      data_validation = data_validation_path, 
                      target_name = mcGet(resample_indexes, "target_name"), ...) 
    }    
    # here should contain the validation prediction list as before
    metrics_tune_models <- lapply(platt_validation_obj, FUN = function(x) x[["validation_predictions"]])
    # a matrix with columns 'platt_intercept' and 'platt_slope', one row for each model
    platt_coefs <- t(sapply(platt_validation_obj, FUN = function(x) x[["platt_coefs"]]))
    colnames(platt_coefs) <- c("platt_intercept", "platt_slope")
    tune_grid <- cbind(tune_grid, platt_coefs)  
    
    file.remove(data_calibration_path)
    
  } 
  
  file.remove(data_train_path)
  file.remove(data_validation_path)
  
  result <- multiClassValidation(resample_indexes = resample_indexes, 
                                 prob = lapply(X = metrics_tune_models, FUN = function(x) c(x, 1 - x)), 
                                 replicate_index = replicate_index,
                                 tune_grid = tune_grid)
  return(result)
  
}

tuneModelVerbose <- function(index, tuneGrid, replicate_index){
  
  parameter_names <- colnames(tuneGrid)
  
  result <- paste("Replicate index: ", replicate_index, sep = "")
  result <- paste(result, ", Number of passes: ", tuneGrid[index, "number_of_passes"], sep = "")
  result <- paste(result, ", Learning rate: ", tuneGrid[index, "learning_rate"], sep = "")
  
  if ("l1" %in% parameter_names){
    result <- paste(result, ", L1 regularization: ", tuneGrid[index, "l1"], sep = "")
  }
  
  print(result)
  
}

#' Evaluate predictions according to some metric
tuneModel <- function(index, tuneGrid, verbose, replicate_index, ...) 
{
  if (verbose){ 
    tuneModelVerbose(index = index, tuneGrid = tuneGrid, replicate_index = replicate_index)
  }
  
  validation_predictions <- trainAndPredict(tuneGrid = tuneGrid[index, ], ...) 
  
  return(validation_predictions)
  
}


tuneModel_Platt <- function(index, tuneGrid, verbose, data_train, data_calibration, data_validation, target_name, ...) 
{
  if (verbose){ 
    tuneModelVerbose(index = index, tuneGrid = tuneGrid)
  }
  
  # train GBDT with training data and predict the calibration data
  calibration_predictions <- trainAndPredict(tuneGrid = tuneGrid[index, ], 
                                             data_train = data_train, data_test = data_calibration,
                                             ...)
  # convert to tree calibration predictions
  tree_calibration_predictions <- reverseProbMap(prob = calibration_predictions)
  
  # obtain target and tree predictions for the calibration points
  sucess_target <- as.numeric(data_calibration[ , target_name[1]])
  
  # optimize and obtain platt_intercept and platt_slope
  platt_coefs <- try(optiminPlattCoefs(sucess_target = sucess_target, 
                                       tree_predictions = tree_calibration_predictions))
  if (inherits(platt_coefs, "try-error")){
    platt_coefs <- c(0, -2)
  }
  
  # train with both training and calibration data using optmized platt values and predict the validation data using trainAndPredict_Platt
  tuneGrid_index <- cbind(tuneGrid[index, ], 
                          platt_intercept = platt_coefs[1], platt_slope = platt_coefs[2])
  validation_predictions <- trainAndPredict_Platt(tuneGrid = tuneGrid_index, 
                                                  data_train = rbind(data_train, data_calibration), 
                                                  data_test = data_validation, ...)
  
  # result should be a list containing the prediction for the validation set and also the platt optimized values
  result <- list(validation_predictions = validation_predictions, platt_coefs = platt_coefs)
  return(result)
  
}

#' Train and predict using vw model
#' 
#' Function to be used internally when cross-validating vw
#' 
#' @param tuneGrid A data.frame with one row containing the values
#'  of \code{number_of_passes} and \code{learning_rate} hyperparameters
#'  that will be used for training and predicting.
#' @param ... Further arguments to be passed to \code{vw}.
#' 
#' @return A vector of prediction values, one for each
#' point in the test_data. In case a point cannot be predicted it should
#' get NA instead. If some error happens in the fit or prediction function
#' return a vector full of NA with class 'try-error'.
trainAndPredict <- function(tuneGrid, data_train_path, data_test_path, link_function, 
                            loss_function, ...){
  
  parameter_names <- colnames(tuneGrid)
  
  regressor_file <- tempfile(tmpdir = ".")
  cache_file <- tempfile(tmpdir = ".")

  call_args <- list(...)
  call_args$data_train_path = data_train_path
  call_args$loss_function = loss_function
  call_args$regressor_file = regressor_file
  call_args$cache_file = cache_file
  call_args$number_of_passes = as.numeric(tuneGrid[, "number_of_passes"])
  call_args$learning_rate = as.numeric(tuneGrid[, "learning_rate"])
  if ("l1" %in% parameter_names){
    call_args$l1 = as.numeric(tuneGrid[, "l1"])
  }

  vw_fit <- do.call("RvwTrain", args = call_args)
#   vw_fit <- RvwTrain(data_train_path, loss_function = loss_function, 
#                      regressor_file = regressor_file,
#                      cache_file = cache_file,
#                      number_of_passes = as.numeric(tuneGrid[, "number_of_passes"]),
#                      learning_rate = as.numeric(tuneGrid[, "learning_rate"]),
#                      ...)
  
  vw_test <- RvwTest(vw_fit = vw_fit,
                     data_test_path = data_test_path, 
                     link_function = link_function)

  test_predictions <- vw_test$normalized_predictions
  
  file.remove(regressor_file)
  file.remove(cache_file)
  
  return(test_predictions)
  
}

#' @export
trainAndPredict_Platt <- function(tuneGrid, ...){
  
  if (!all(dim(tuneGrid) == c(1,6))){
    stop("Invalid dimension for a single tuneGrid row.")
  }
  
  prediction_obj <- try(Rgbdt(number_trees = as.numeric(tuneGrid[, "number_trees"]),
                              number_leaf_nodes = as.numeric(tuneGrid[, "number_leaf_nodes"]),
                              shrinkage = as.numeric(tuneGrid[, "shrinkage"]),
                              sampling_rate = as.numeric(tuneGrid[, "sampling_rate"]),
                              ...))
  
  extra_pars <- list(...)
  data_test <- extra_pars[["data_test"]]
  
  if (inherits(prediction_obj, "try-error")){
    test_predictions <- rep(NA, nrow(test_data))
    class(test_predictions) <- "try-error"
  } else {
    test_predictions <- rgGet(prediction_obj, "pred")
    tree_predictions <- reverseProbMap(test_predictions)
    test_predictions <- PlattMap(x = tree_predictions, 
                                 intercept = as.numeric(tuneGrid[, "platt_intercept"]), 
                                 slope = as.numeric(tuneGrid[, "platt_slope"]))
  }
  
  return(test_predictions)
  
}

#' @export
reverseProbMap <- function(prob){
  tree <- 0.5 * (log(prob) - log(1 - prob))
}

#' @export
PlattMap <- function(x, intercept, slope){
  1 / (1 + exp(intercept + slope*x))
}

# x[1]: platt_intercept
# x[2]: platt_slope

#' @export
plattObjectiveFunction <- function(x, sucess_target, tree_predictions){
  p <- 1 / (1 + exp(x[1] + x[2] * tree_predictions))
  result <- - sum( sucess_target * log(p) + (1 - sucess_target) * log(1 - p) )
  return(result)
}

#' @export
optiminPlattCoefs <- function(sucess_target, tree_predictions){
  
  opt_obj <- nlminb(start = c(0, -2), objective = plattObjectiveFunction, 
                    sucess_target = sucess_target, tree_predictions = tree_predictions)
  
  return(as.numeric(opt_obj$par))
  
}