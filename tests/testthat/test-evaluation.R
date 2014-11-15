context("Main evaluation function")

test_that("evaluateProbClass with log_score works as expected", {
  
  if (FALSE){
    
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
    
    save(indexes, file = file.path(system.file("extdata", package="tgmMultiClass"), "head_20_soccer_game_resample_indexes"))
    
  } else {
    
    head_20_soccer_game_resample_indexes <- get(load(file.path(system.file("extdata", package="tgmMultiClass"), "head_20_soccer_game_resample_indexes")))
    
  }
  
  if (FALSE){
    
    pred_obj <- predict_r_vgam(resample_indexes = head_20_soccer_game_resample_indexes,
                               formula = cbind(home.win, home.draw, home.lose) ~ 1 + fair.odd.home + fair.odd.draw + fair.odd.away,
                               family = "multinomial")
    save(pred_obj, file = file.path(system.file("extdata", package="tgmMultiClass"), "head_20_soccer_game_vgam_predictions"))
    
  } else {
    
    head_20_soccer_game_vgam_predictions <- get(load(file.path(system.file("extdata", package="tgmMultiClass"), "head_20_soccer_game_vgam_predictions")))
    
  }
  
  expected <- list(c(log(0.1619565), log(0.0002236837), log(0.8739541)),
                   c(log(0.2792624), log(0.18168595), log(0.4349983)),
                   c(log(0.4895213), log(1.074798e-05), log(0.5191920)),
                   c(log(0.4040598), log(0.4127275), log(0.1598506)))
  class(expected) <- "multiClassScores"
  
  eval <- evaluateProbClass(resample_indexes = indexes, 
                            pred_obj = pred_obj, 
                            type = "log_score")
  
  expect_equal(object = eval, expected = expected, tolerance = 1e-5)
})

