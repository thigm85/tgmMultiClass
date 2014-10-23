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
  
  train_matrix <- matrix(integer(0), size_train, number_replicates)
  v_matrix <- matrix(integer(0), size_v, number_replicates)
  test_matrix <- matrix(integer(0), size_test, number_replicates)
  
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
    
    train_matrix[, i] <- train
    v_matrix[, i] <- v
    test_matrix[, i] <- test
  }
  
  result <- list(training = train_matrix,
                 validation = v_matrix,
                 test = test_matrix)
  
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

generateTestIndexes <- function(dataset, target_names, type = "3way", options, ...){
    
  target <- dataset[, target_names]
  
  type <- match.arg(type)
  
  if (type == "3way"){
    parsed_options <- parseThreeWayOptions(options)
    number_lines <- nrow(dataset)
    indexes <- generateIndexThreeWay(prop_v = parsed_options$prop_v, 
                                     prop_test = parsed_options$prop_test, 
                                     number_lines = number_lines, 
                                     number_replicates = parsed_options$number_replicates) 
  }
  
  result <- list(target = target,
                 training = indexes$training,
                 validation = indexes$validation,
                 test = indexes$test,
                 type = type)
  return(result)  
}

saveTestIndexes <- function(test_indexes, folder_path){
  
  write.table(x = test_indexes$target, file = file.path(folder_path, "target.txt"), 
              quote = FALSE, sep = "\t", row.names = FALSE)
  write.table(x = test_indexes$training, file = file.path(folder_path, "training.txt"), 
              quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE)
  write.table(x = test_indexes$validation, file = file.path(folder_path, "validation.txt"), 
              quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE)
  write.table(x = test_indexes$test, file = file.path(folder_path, "test.txt"), 
              quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE)
  write.table(x = test_indexes$type, file = file.path(folder_path, "type_validation.txt"), 
              quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE)
}

loadTestIndexes <- function(folder_path){
  
  test_indexes <- list()
  test_indexes$target <- read.delim(file = file.path(folder_path, "target.txt"), 
                                    header = TRUE, sep = "\t")
  test_indexes$training <- read.delim(file = file.path(folder_path, "training.txt"), 
                                      header = FALSE, sep = "\t")
  test_indexes$validation <- read.delim(file = file.path(folder_path, "validation.txt"), 
                                        header = FALSE, sep = "\t")
  test_indexes$test <- read.delim(file = file.path(folder_path, "test.txt"), 
                                  header = FALSE, sep = "\t")
  test_indexes$type <- read.delim(file = file.path(folder_path, "type_validation.txt"), 
                                  header = FALSE, sep = "\t")
  
  return(test_indexes)
}

checkIndexesExists <- function(folder_path){
  
  exists <- (file.exists(file.path(folder_path, "target.txt")) &
               file.exists(file.path(folder_path, "training.txt")) &
               file.exists(file.path(folder_path, "validation.txt")) &
               file.exists(file.path(folder_path, "test.txt")) &
               file.exists(file.path(folder_path, "type_validation.txt")))
  return(exists)
}
