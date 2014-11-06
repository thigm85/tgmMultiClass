context("Resample indexes")

test_that("generateTestIndexes works as expected", {
  
  # Load data
  data <- read.table(file = system.file("extdata", "head_20_soccer_game.csv", package="tgmMultiClass"), 
                     header = TRUE, sep = ";")
  
  # generate 4 dataset replications, where in each replication 20% of the data
  # will be assigned to a validation set and another 20% to a test set.
  indexes <- generateTestIndexes(dataset = data, 
                                 target_names = c("home.win", "home.draw", "home.lose"), 
                                 type = "3way", 
                                 options = list(prop_v = 0.2, 
                                                prop_test = 0.2,
                                                number_replicates = 4))
  
  # dataset
  expect_equal(object = indexes$dataset, expected = data)
  # target
  expect_equal(object = indexes$target, expected = data[, c("home.win", "home.draw", "home.lose")])
  #training, validation, test
  expect_true((ncol(indexes$training) == ncol(indexes$validation)) & 
                (ncol(indexes$validation) == ncol(indexes$test)) & 
                (ncol(indexes$test) == 4))
  expect_false(any(indexes$training[, 1] %in% indexes$validation[, 1]))
  expect_false(any(indexes$training[, 1] %in% indexes$test[, 1]))
  # CONTINUAR ACIMA
  
})    
