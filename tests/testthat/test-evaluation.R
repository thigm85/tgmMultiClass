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
    c(log(0.3677809), log(0.4403966), log(0.5213381),
      log(6.766397e-13), log(3.857948e-01), log(0.43939621),
      log(0.4734683), log(0.4319461), log(0.4477690),
      log(0.1795848), log(0.2801943), log(0.4719646)), 3, 4)
  
  eval <- evaluateProbClass(resample_indexes = indexes, 
                            pred_obj = pred_obj, 
                            type = "log_score")
  
  expect_equal(object = eval, expected = expected, tolerance = 1e-5)
})

