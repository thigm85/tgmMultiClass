context("mappingIndexes")

test_that("mappingIndexes works as expected", {
  
  mapping <- data.frame(from = rep(1:5, times = 4), to = 1:20)
  unit_index_matrix <- matrix(c(1,3,5), nrow = 3, ncol = 2)
  
  expected <- NULL
  for (i in 1:2){
    condition <- mapping[, "from"] %in% unit_index_matrix[,i]
    expected <- cbind(expected, mapping[condition, "to"])
  }
  
  observed <- mappingIndexes(unit_index_matrix = unit_index_matrix, 
                             mapping_from = mapping$from, 
                             mapping_to = mapping$to)
  
  expect_equal(object = observed, expected = expected)
  
})
  
context("generateTestIndexes")

test_that("startandard generateTestIndexes with 3way type works as expected", {
  
  # Load data
  data <- read.table(file = system.file("extdata", "head_20_soccer_game.csv", package="tgmMultiClass"), 
                     header = TRUE, sep = ";")
  
  # generate 4 dataset replications, where in each replication 20% of the data
  # will be assigned to a validation set and another 20% to a test set.
  number_lines <- nrow(data)
  type <- "3way"
  indexes <- generateTestIndexes(dataset = data, 
                                 target_names = c("home.win", "home.draw", "home.lose"), 
                                 type = type, 
                                 options = list(prop_v = 0.2, 
                                                prop_test = 0.2,
                                                number_replicates = 4))
  
  # dataset
  obs_dataset <- mcGet(indexes, "dataset")
  expect_equal(object = obs_dataset, expected = data)
  # target
  obs_target <- mcGet(indexes, "target")
  expect_equal(object = obs_target, expected = data[, c("home.win", "home.draw", "home.lose")])
  #training, validation, test
  obs_training <- mcGet(indexes, "training")
  obs_validation <- mcGet(indexes, "validation")
  obs_test <- mcGet(indexes, "test")
  ## number_replicates
  expect_true((ncol(obs_training) == ncol(obs_validation)) & 
                (ncol(obs_validation) == ncol(obs_test)) & 
                (ncol(obs_test) == 4))
  ## empty intersection
  for (i in 1:4){
    expect_false(any(obs_training[, i] %in% obs_validation[, i]))
    expect_false(any(obs_training[, i] %in% obs_test[, i]))
    expect_false(any(obs_validation[, i] %in% obs_test[, i]))
  }
  ## valid indexes
  for (i in 1:4){
    expect_true(all(1:number_lines %in% c(obs_training[, i], obs_validation[, i], obs_test[, i])))
  }
  ## type
  obs_type <- mcGet(indexes, "type")
  expect_equal(object = obs_type, expected = type)
  ## number_replicates  
  obs_number_replicates <- mcGet(indexes, "number_replicates")
  expect_equal(object = obs_number_replicates, expected = 4)
  
})    

test_that("generateTestIndexes with 3way type and observational_unit works as expected", {

  # Load data
  data <- read.table(file = system.file("extdata", "head_20_soccer_game_multiple_rounds.csv", package="tgmMultiClass"), 
                     header = TRUE, sep = ";")
  number_lines <- nrow(data)
  # generate 4 dataset replications, where in each replication 20% of the data
  # will be assigned to a validation set and another 20% to a test set. 
  # All games that belong to the same round will be assigned to the same set, 
  # training, validation ot test. This is defined by 'observational_unit = "round"'
  indexes <- generateTestIndexes(dataset = data, 
                                 target_names = c("home.win", "home.draw", "home.lose"), 
                                 type = "3way",
                                 observational_unit = "round",
                                 options = list(prop_v = 0.2, 
                                                prop_test = 0.2,
                                                number_replicates = 4))
  
  #training, validation, test
  obs_training <- mcGet(indexes, "training")
  obs_validation <- mcGet(indexes, "validation")
  obs_test <- mcGet(indexes, "test")
  ## number_replicates
  expect_true((ncol(obs_training) == ncol(obs_validation)) & 
                (ncol(obs_validation) == ncol(obs_test)) & 
                (ncol(obs_test) == 4))
  ## empty intersection
  for (i in 1:4){
    expect_false(any(obs_training[, i] %in% obs_validation[, i]))
    expect_false(any(obs_training[, i] %in% obs_test[, i]))
    expect_false(any(obs_validation[, i] %in% obs_test[, i]))
  }
  ## valid indexes
  for (i in 1:4){
    expect_true(all(1:number_lines %in% c(obs_training[, i], obs_validation[, i], obs_test[, i])))
  }
  ## empty round intersection
  for (i in 1:4){
    expect_false(any(data[obs_training[, i], "round"] %in% data[obs_validation[, i], "round"]))
    expect_false(any(data[obs_training[, i], "round"] %in% data[obs_test[, i], "round"]))
    expect_false(any(data[obs_validation[, i], "round"] %in% data[obs_test[, i], "round"]))
  }
  
})

context("Load and Save")

test_that("saveResampleIndexes and loadResampleIndexes works as expected", {
  
  # Load data
  data <- read.table(file = system.file("extdata", "head_20_soccer_game.csv", package="tgmMultiClass"), 
                     header = TRUE, sep = ";")
  
  # generate 4 dataset replications, where in each replication 20% of the data
  # will be assigned to a validation set and another 20% to a test set.
  number_lines <- nrow(data)
  type <- "3way"
  indexes <- generateTestIndexes(dataset = data, 
                                 target_names = c("home.win", "home.draw", "home.lose"), 
                                 type = type, 
                                 options = list(prop_v = 0.2, 
                                                prop_test = 0.2,
                                                number_replicates = 4))
  
  saveResampleIndexes(resample_indexes = indexes, 
                      folder_path = system.file("extdata", package="tgmMultiClass"), 
                      optional_name = "SaveLoadUnitTest")
  
  indexes_loaded <- loadResampleIndexes(folder_path = system.file("extdata", package="tgmMultiClass"), 
                                        optional_name = "SaveLoadUnitTest")  
  
  expect_equal(object = indexes_loaded, expected = indexes)
  
})