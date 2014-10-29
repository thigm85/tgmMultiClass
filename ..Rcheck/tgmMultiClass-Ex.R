pkgname <- "tgmMultiClass"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('tgmMultiClass')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("generateTestIndexes")
### * generateTestIndexes

flush(stderr()); flush(stdout())

### Name: generateTestIndexes
### Title: Generate indexes necessary to evaluate models
### Aliases: generateTestIndexes

### ** Examples

data(soccer_game)
indexes <- generateTestIndexes(dataset = soccer_game,
                               target_names = c("home.win", "home.draw", "home.lose"),
                               type = "3way",
                               options = list(prop_v = 0.2,
                                              prop_test = 0.2,
                                              number_replicates = 4))



cleanEx()
nameEx("predict_r_vgam")
### * predict_r_vgam

flush(stderr()); flush(stdout())

### Name: predict_r_vgam
### Title: multi-class prediction probability with 'vgam'
### Aliases: predict_r_vgam

### ** Examples

data(soccer_game)
indexes <- generateTestIndexes(dataset = soccer_game,
                               target_names = c("home.win", "home.draw", "home.lose"),
                               type = "3way",
                               options = list(prop_v = 0.2,
                                              prop_test = 0.2,
                                              number_replicates = 4))
pred_obj <- predict_r_vgam(data = soccer_game,
                           resample_indexes = indexes,
                           formula = cbind(home.win, home.draw, home.lose) ~ 1 + fair.odd.home + fair.odd.draw + fair.odd.away,
                           family = "multinomial")

all_predictions <- mcGet(pred_obj, "prob")
second_replication_only <- mcGet(pred_obj, "prob", 2)



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
