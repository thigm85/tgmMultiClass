#' Creates a new index matrix based on a mapping data frame
#' 
#' @details
#' As an example, this function is used to convert resampling indexes
#' that were constructed wrt observational units back to the rows
#' mapping
#' 
#' @examples
#' \dontrun{
#' mapping <- data.frame(from = rep(1:5, times = 4), to = 1:20)
#' unit_index_list <- list(c(1,3,5), c(1,3,5))
#' 
#' new_index_list <- mappingIndexes(unit_index_list = unit_index_list, 
#'                                    mapping_from = mapping$from, 
#'                                    mapping_to = mapping$to)
#' }                                    
mappingIndexes <- function(unit_index_list, 
                           mapping_from, 
                           mapping_to){
  
  number_columns <- length(unit_index_list)
  
  mapping <- data.frame(from = mapping_from,
                        to = mapping_to)
  
  new_index_list <- NULL
  for (i in 1:number_columns){
    condition <- mapping[, "from"] %in% unit_index_list[[i]]
    new_index_list[[i]] <- mapping[condition, "to"]
  }
  
  return(new_index_list)
}

generateIndexThreeWay <- function # Generate train, cv and test using simple three-way splits.
(prop_v, 
 ### proportion of the data that goes to validation. Between 0 and 1.
 prop_test, 
 ### proportion of the data that goes to testing. Between 0 and 1.
 number_lines, 
 ### number of lines in the dataset.
 number_replicates
 ### number of replicates to generate.
) 
{
  
  size_train = ceiling((1 - prop_v - prop_test) * number_lines)
  size_v = ceiling((prop_v/(prop_v + prop_test)) * (number_lines - size_train))
  size_test = number_lines - size_train - size_v
  
  train_list <- list()
  validation_list <- list()
  test_list <- list()
  
  for (i in 1:number_replicates){
    all = 1:number_lines
    train = sample(x = all, 
                   size = size_train, 
                   replace = FALSE)
    all = all[!(all %in% train)]
    v = sample(x = all, 
               size = size_v, 
               replace = FALSE)
    test = all[!(all %in% v)]
    
    train_list[[i]] <- train
    validation_list[[i]] <- v
    test_list[[i]] <- test
  }
  
  result <- list(training = train_list,
                 validation = validation_list,
                 test = test_list)
  
}  

parseThreeWayOptions <- function(options){
  if (!all(c("prop_v", "prop_test", "number_replicates") %in% names(options))){
    stop("Please, include 'prop_v', 'prop_test', 'number_lines' and 
         'number_replicates' when generating indexes with generateIndexThreeWay function.")
  }
  result <- list(prop_v = options$prop_v, 
                 prop_test = options$prop_test, 
                 number_replicates = options$number_replicates)
  return(result)
}

#' Three Way wrapper function to be used within \code{generateTestIndexes}.
#' 
#' @param options It is a list with: \code{prop_v}, which is a number between 0 and 1 that
#' describes the proportion of the data that goes into the validation set, 
#' \code{prop_test}, which is a number between 0 and 1 that describes the proportion 
#' of the data that goes into the test set and \code{number_replicates} to indicate
#' how many data replications we want.
#' @inheritParams generateTestIndexes
#' 
#' @return A list with \code{indexes} and \code{number_replicates} elements.
#' \code{indexes} contains the \code{training}, \code{validation} and \code{test}
#' matrices and \code{number_replicates} contains the number of replications
#' used.   
generateTestIndexes_threeWay <- function(dataset, options, observational_unit = NULL){ 
  
  parsed_options <- parseThreeWayOptions(options)
  number_replicates <- parsed_options$number_replicates

  if (is.null(observational_unit)){
    number_lines <- nrow(dataset)  
    indexes <- generateIndexThreeWay(prop_v = parsed_options$prop_v, 
                                     prop_test = parsed_options$prop_test, 
                                     number_lines = number_lines, 
                                     number_replicates = parsed_options$number_replicates) 
  } else {
    rows_and_units <- data.frame(row = 1:nrow(dataset), 
                                 unit = as.numeric(as.factor(dataset[, observational_unit])))
    number_lines <- max(rows_and_units$unit)      
    indexes <- generateIndexThreeWay(prop_v = parsed_options$prop_v, 
                                     prop_test = parsed_options$prop_test, 
                                     number_lines = number_lines, 
                                     number_replicates = parsed_options$number_replicates)
    indexes[["training"]] <- mappingIndexes(unit_index_list = indexes[["training"]], 
                                            mapping_from = rows_and_units[["unit"]], 
                                            mapping_to = rows_and_units[["row"]])
    indexes[["validation"]] <- mappingIndexes(unit_index_list = indexes[["validation"]], 
                                              mapping_from = rows_and_units[["unit"]], 
                                              mapping_to = rows_and_units[["row"]])
    indexes[["test"]] <- mappingIndexes(unit_index_list = indexes[["test"]], 
                                        mapping_from = rows_and_units[["unit"]], 
                                        mapping_to = rows_and_units[["row"]])
  }
  result <- list(indexes = indexes, number_replicates = number_replicates)
  return(result)
}

#--------------------------


#' Generate k cross-validation indexes
#'
#' @param number_lines Number of lines in the dataset.
#' @param number_replicates Number of replicates to generate, see details.
#' @param k How many parts the dataset should be divided.
#' 
#' @details Check the implementation sketch 
#' (https://www.evernote.com/shard/s95/sh/e033ac5b-5e18-438e-993d-d47d051b6228/9518a782b9b95988db4acc5149debfd0).
#' The idea is to randomly divide that dataset into k mutually exclusive parts. 
#' Each replicate will take one of those parts as a test set and use the other k - 1 parts 
#' as training (k - 2 parts) and validation (1 part) sets. Because of that the maximum number 
#' of replication allowed is equal to k. So, for each replicate we will have k - 1 different 
#' training sets and k - 1 validation sets. Once we have the predictions of each model for 
#' the k - 1 validation sets we will combine the scores over the k - 1 validation sets and 
#' decide which model is the best. The best model will then be used to predict over the test set. 
#' This same process will happen for each replication. 
#' 
generateIndexCVk <- function(number_lines, 
                             number_replicates,
                             k){
  
  if (number_replicates > k){
    warning("'number_replicates' needs to be at most equal to 'k'. Setting number_replicates = k.")
    number_replicates <- k
  }
  
  train_list <- list()
  validation_list <- list()
  test_list <- list()
  replication_vector <- c()
  
  block_size <- trunc(number_lines / k)
  blocks_index <- 1:k
  
  elements <- 1:number_lines
  blocks <- list()
  for (i in 1:(k - 1)){
    sampling <- sample(x = 1:length(elements), size = block_size, replace = FALSE)
    blocks[[i]] <- elements[sampling]
    elements <- elements[-sampling]
  }
  blocks[[k]] <- elements
  
  count <- 0
  for (replication_index in 1:number_replicates){
    
    replication_vector <- c(replication_vector, rep(replication_index, times = k - 1))
    
    test_list[[replication_index]] <- blocks[[replication_index]]
    
    validation_blocks <- blocks_index[-replication_index]
    
    for (validation_index in 1:length(validation_blocks)){
      
      count <- count + 1
      
      training_blocks <- validation_blocks[-validation_index]
      train_list[[count]] <- blocks[[training_blocks[1]]]
      for (i in 2:length(training_blocks)){
        train_list[[count]] <- c(train_list[[count]], blocks[[training_blocks[i]]])
      }
      
      validation_list[[count]] <- blocks[[validation_blocks[validation_index]]] 
      
    }
    
  }

  result <- list(training = train_list,
                 validation = validation_list,
                 test = test_list,
                 replication = replication_vector)
  return(result)
  
}
  
#--------------------------

#' Generate indexes necessary to evaluate models
#' 
#' Create dataset replications with training, validation and test sets.
#' 
#' @param dataset Data.frame with the complete dataset available.
#' @param target_names Character vector with the name of the columns
#'  in \code{dataset} that contain the target variables. For a 
#'  classification problem with K classes, there should be K column names.
#' @param type Type of sampling used. Currently, only '3way' is implemented, see 
#'  details.
#' @param observational_unit Column name within \code{dataset} that will be used 
#' as reference to assign rows to training, validation and test sets. The same 
#' observational unit cannot be assigned to more than one of training, validation 
#' or test sets. Default is set to \code{NULL}, meaning that each row is an observational 
#' unit.   
#' @param options List with parameters to be passed to the resampling function. 
#'  It depends on the parameter \code{type}, see details.
#' @param include_dataset Boolean variable that indicates if the dataset should 
#'  also be returned. 
#' @param ... further arguments. Currently not used.
#' 
#' @return S3 object of class 'datasetResample'.
#' 
#' @examples
#' data(soccer_game)
#'
#' indexes <- generateTestIndexes(dataset = soccer_game, 
#'                                target_names = c("home.win", "home.draw", "home.lose"), 
#'                                type = "3way", 
#'                                options = list(prop_v = 0.2, 
#'                                               prop_test = 0.2,
#'                                               number_replicates = 4))
#' 
#' # Observation unit: Means that rows that belong to the same observation unit will be placed 
#' # together in one of training, validation or test set. The next example uses the 'round' variable 
#' # as observational unit, meaning that all games from an specific round will be clustered into the
#' # same set.
#' 
#' indexes <- generateTestIndexes(dataset = soccer_game, 
#'                                target_names = c("home.win", "home.draw", "home.lose"), 
#'                                type = "3way", 
#'                                observational_unit = "round",
#'                                options = list(prop_v = 0.2, 
#'                                               prop_test = 0.2,
#'                                               number_replicates = 4))

#' @export
generateTestIndexes <- function(dataset, target_names, type = "3way", 
                                observational_unit = NULL, options, 
                                include_dataset = TRUE, ...){
  
  target <- dataset[, target_names]
  if (inherits(target, "numeric")){
    target <- matrix(target, ncol = length(target_names))  
  }
  
  type <- match.arg(type)
  
  if (type == "3way"){
    
    index_obj <- generateTestIndexes_threeWay(dataset = dataset, 
                                              options = options, 
                                              observational_unit = observational_unit)
    indexes <- index_obj$indexes
    number_replicates <- index_obj$number_replicates
    
  }
  
  tag <- tempfile(pattern = "datasetResample_", tmpdir = "")
  tag <- substr(tag, start = 2 , stop = nchar(tag))
  
  if (!include_dataset){
    dataset <- NULL
  }
  
  result <- list(dataset = dataset,
                 target = target,
                 training = indexes$training,
                 validation = indexes$validation,
                 test = indexes$test,
                 type = type,
                 tag = tag,
                 number_replicates = number_replicates)
  class(result) <- "datasetResample"
  
  return(result)  
}

#' Extract info from \code{datasetResample} objects
#' 
#' @export
mcGet.datasetResample <- function(x, attr, i = NULL){
  
  if (attr == "dataset"){ # dataset
    
    return(x$dataset)
    
  } else if (attr == "target"){ # target
    
    return(x$target)
    
  } else if (attr == "class_labels"){ # name of the classes
    
    return(colnames(x[["target"]]))
    
  } else if (attr == "target_name"){ # Name of the target variable, same of class_labels.
    
    return(colnames(x[["target"]]))
    
  } else if (attr == "training"){ # training indexes
    
    if (is.null(i)){
      return(x$training)  
    } else {
      return(x$training[[i]])
    }
    
  } else if (attr == "validation"){ # validation indexes
    
    if (is.null(i)){
      return(x$validation)  
    } else {
      return(x$validation[[i]])
    }
    
  } else if (attr == "test"){ # test indexes
    
    if (is.null(i)){
      return(x$test)  
    } else {
      return(x$test[[i]])
    }
    
  } else if (attr == "type"){ # resampling type
    
    return(x$type)
    
  } else if (attr == "tag"){ # tag identifying a given resample object
    
    return(x$tag)
    
  } else if (attr == "number_replicates"){ # number of replicates used
    
    return(x$number_replicates)
    
  } else if (attr == "validation_target"){ # target variables for a given validation set
    
    if (is.null(i)){
      stop("validation_target: provide index i.")  
    } else {
      validation <- x[["validation"]]
      if (i >= 1 & i <= length(validation)){
        result <- x[["target"]][validation[[i]], ]
        return(result)
      } else {
        stop("index i out of range.")
      }
    }
    
  } else if (attr == "test_target"){ # target variables for a given test set
    
    if (is.null(i)){
      stop("test_target: provide index i.")  
    } else {
      test <- x[["test"]]
      if (i >= 1 & i <= length(test)){
        result <- x[["target"]][test[[i]], ]
        return(result)
      } else {
        stop("index i out of range.")
      }
    }
    
  } else { # In case attr is not found
    stop(attr, " not found.")
  }
  
}  

#' Check if all resample indexes elements exist
checkAllResampleIndexesExists <- function(folder_path){
  
  exists <- (file.exists(file.path(folder_path, "resample_dataset.txt")) &
               file.exists(file.path(folder_path, "resample_target.txt")) &
               file.exists(file.path(folder_path, "resample_training.txt")) &
               file.exists(file.path(folder_path, "resample_validation.txt")) &
               file.exists(file.path(folder_path, "resample_test.txt")) &
               file.exists(file.path(folder_path, "resample_type.txt")) &
               file.exists(file.path(folder_path, "resample_tag.txt")) &
               file.exists(file.path(folder_path, "number_replicates")))
  return(exists)
}

#' Check if any of the resample indexes elements exist
checkAnyResampleIndexesExists <- function(folder_path){
  
  exists <- (file.exists(file.path(folder_path, "resample_dataset.txt")) |
               file.exists(file.path(folder_path, "resample_target.txt")) |
               file.exists(file.path(folder_path, "resample_training.txt")) |
               file.exists(file.path(folder_path, "resample_validation.txt")) |
               file.exists(file.path(folder_path, "resample_test.txt")) |
               file.exists(file.path(folder_path, "resample_type.txt")) |
               file.exists(file.path(folder_path, "resample_tag.txt")) |
               file.exists(file.path(folder_path, "number_replicates")))
  return(exists)
}

#' Save a resample_indexes object into txt files.
#' @export
saveResampleIndexes <- function(resample_indexes, folder_path, optional_name = NULL){
  
  if (is.null(optional_name)){
    prefix <- "resample_"
  } else {
    prefix <- paste(optional_name, "_resample_", sep="")
  }
  
  number_replicates <- mcGet(resample_indexes, "number_replicates")
  
  folder_name <- substr(x = prefix, start = 1, stop = nchar(prefix) - 1)
  dir.create(file.path(folder_path, folder_name), showWarnings = FALSE)
  
  write.table(x = mcGet(resample_indexes, "dataset"), file = file.path(folder_path, folder_name, paste(prefix, "dataset.txt", sep="")), 
              quote = FALSE, sep = "\t", row.names = FALSE)
  write.table(x = mcGet(resample_indexes, "target"), file = file.path(folder_path, folder_name, paste(prefix, "target.txt", sep="")), 
              quote = FALSE, sep = "\t", row.names = FALSE)
  for (i in 1:number_replicates){
    write.table(x = mcGet(resample_indexes, "training", i), file = file.path(folder_path, folder_name, paste(prefix, "training_", i, ".txt", sep="")), 
                quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE)
  }
  for (i in 1:number_replicates){
    write.table(x = mcGet(resample_indexes, "validation", i), file = file.path(folder_path, folder_name, paste(prefix, "validation_", i, ".txt", sep="")), 
                quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE)
  }
  for (i in 1:number_replicates){
    write.table(x = mcGet(resample_indexes, "test", i), file = file.path(folder_path, folder_name, paste(prefix, "test_", i, ".txt", sep="")), 
                quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE)
  }
  write(x = mcGet(resample_indexes, "type"), file = file.path(folder_path, folder_name, paste(prefix, "type.txt", sep="")), ncolumns = 1)
  write(x = mcGet(resample_indexes, "tag"), file = file.path(folder_path, folder_name, paste(prefix, "tag.txt", sep="")), ncolumns = 1)
  write(x = number_replicates, file = file.path(folder_path, folder_name, paste(prefix, "number_replicates.txt", sep="")), ncolumns = 1)
  
}

#' Load a resample_indexes object from txt files.
#' @export
loadResampleIndexes <- function(folder_path, optional_name = NULL, stringsAsFactors = FALSE){

  if (is.null(optional_name)){
    prefix <- "resample_"
  } else {
    prefix <- paste(optional_name, "_resample_", sep="")
  }
  
  folder_name <- substr(x = prefix, start = 1, stop = nchar(prefix) - 1)
  
  number_replicates <- scan(file = file.path(folder_path, folder_name, paste(prefix, "number_replicates.txt", sep="")), 
                            what = double(), quiet = TRUE)
  
  test_indexes <- list()
  test_indexes$dataset <- read.delim(file = file.path(folder_path, folder_name, paste(prefix, "dataset.txt", sep="")), 
                                     header = TRUE, sep = "\t", stringsAsFactors = stringsAsFactors)
  test_indexes$target <- read.delim(file = file.path(folder_path, folder_name, paste(prefix, "target.txt", sep="")), 
                                     header = TRUE, sep = "\t")
  test_indexes$training <- list()
  for (i in 1:number_replicates){
    test_indexes$training[[i]] <- as.numeric(unlist(read.delim(file = file.path(folder_path, folder_name, paste(prefix, "training_", i, ".txt", sep="")), 
                                                        header = FALSE, sep = "\t")))
  }
  test_indexes$validation <- list()
  for (i in 1:number_replicates){
    test_indexes$validation[[i]] <- as.numeric(unlist(read.delim(file = file.path(folder_path, folder_name, paste(prefix, "validation_", i, ".txt", sep="")), 
                                       header = FALSE, sep = "\t")))
  }  
  test_indexes$test <- list()
  for (i in 1:number_replicates){
    test_indexes$test[[i]] <- as.numeric(unlist(read.delim(file = file.path(folder_path, folder_name, paste(prefix, "test_", i, ".txt", sep="")), 
                                       header = FALSE, sep = "\t")))
  }  
  test_indexes$type <- scan(file = file.path(folder_path, folder_name, paste(prefix, "type.txt", sep="")), 
                            what = character(), quiet = TRUE)
  test_indexes$tag <- scan(file = file.path(folder_path, folder_name, paste(prefix, "tag.txt", sep="")), 
                            what = character(), quiet = TRUE)
  test_indexes$number_replicates <- number_replicates
  class(test_indexes) <- "datasetResample"
  
  return(test_indexes)
}

