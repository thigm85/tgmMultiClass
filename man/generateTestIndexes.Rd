\name{generateTestIndexes}
\alias{generateTestIndexes}
\title{Generate indexes necessary to evaluate models}
\usage{
generateTestIndexes(dataset, target_names, type = "3way",
  observational_unit = NULL, options, include_dataset = TRUE, ...)
}
\arguments{
  \item{dataset}{Data.frame with the complete dataset
  available.}

  \item{target_names}{Character vector with the name of the
  columns in \code{dataset} that contain the target
  variables. For a classification problem with K classes,
  there should be K column names.}

  \item{type}{Type of sampling used. Currently, only '3way'
  is implemented, see details.}

  \item{observational_unit}{Column name within
  \code{dataset} that will be used as reference to assign
  rows to training, validation and test sets. The same
  observational unit cannot be assigned to more than one of
  training, validation or test sets. Default is set to
  \code{NULL}, meaning that each row is an observational
  unit.}

  \item{options}{List with parameters to be passed to the
  resampling function.  It depends on the parameter
  \code{type}, see details.}

  \item{include_dataset}{Boolean variable that indicates if
  the dataset should also be returned.}

  \item{...}{further arguments. Currently not used.}
}
\value{
S3 object of class 'datasetResample'.
}
\description{
Create dataset replications with training, validation and
test sets.
}
\examples{
data(soccer_game)

indexes <- generateTestIndexes(dataset = soccer_game,
                               target_names = c("home.win", "home.draw", "home.lose"),
                               type = "3way",
                               options = list(prop_v = 0.2,
                                              prop_test = 0.2,
                                              number_replicates = 4))

# Observation unit: Means that rows that belong to the same observation unit will be placed
# together in one of training, validation or test set. The next example uses the 'round' variable
# as observational unit, meaning that all games from an specific round will be clustered into the
# same set.

indexes <- generateTestIndexes(dataset = soccer_game,
                               target_names = c("home.win", "home.draw", "home.lose"),
                               type = "3way",
                               observational_unit = "round",
                               options = list(prop_v = 0.2,
                                              prop_test = 0.2,
                                              number_replicates = 4))
}

