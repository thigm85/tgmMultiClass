tgmMultiClass
=============

Some wrap and utility functions for multi-class classification.

## Resampling dataset

```
# Load data
data(soccer_game)

# generate 4 dataset replications, where in each replication 20% of the data
# will be assigned to a validation set and another 20% to a test set.
indexes <- generateTestIndexes(dataset = soccer_game, 
                               target_names = c("home.win", "home.draw", "home.lose"), 
                               type = "3way", 
                               options = list(prop_v = 0.2, 
                                              prop_test = 0.2,
                                              number_replicates = 4))
                                              
# We can use `observational_unit` argument if we want that games happening 
# in the same `round` be put together in training, validation or test set.
indexes <- generateTestIndexes(dataset = soccer_game,
                               target_names = c("home.win", "home.draw", "home.lose"),
                               type = "3way",
                               observational_unit = "round",
                               options = list(prop_v = 0.2,
                                              prop_test = 0.2,
                                              number_replicates = 4))                                              
```

## Multiclass prediction

* VGAM

```
pred_obj <- predict_r_vgam(resample_indexes = indexes,
                           formula = cbind(home.win, home.draw, home.lose) ~ 1 + fair.odd.home + fair.odd.draw + fair.odd.away,
                           family = "multinomial")  
```

## Evaluation metric

* Log score

```
eval <- evaluateProbClass(resample_indexes = indexes, 
                          pred_obj = pred_obj, 
                          type = "log_score")
```



