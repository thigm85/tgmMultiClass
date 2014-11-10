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
  
  expected <- matrix(   
    c(log(0.2298175), log(0.3093257), log(0.4207831),
      log(0.3884281), log(0.3979297), log(0.4713690),
      log(0.2941116), log(0.4442579), log(0.3896409),
      log(0.2956390), log(0.3215099), log(0.2717192)), 3, 4)
  
  eval <- evaluateProbClass(resample_indexes = indexes, 
                            pred_obj = pred_obj, 
                            type = "log_score")
  
  expect_equal(object = eval, expected = expected, tolerance = 1e-5)
})

