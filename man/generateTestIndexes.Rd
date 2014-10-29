\name{generateTestIndexes}
\alias{generateTestIndexes}
\title{Generate indexes necessary to evaluate models}
\usage{
generateTestIndexes(dataset, target_names, type = "3way", options, ...)
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

  \item{options}{List with parameters to be passed to the
  resampling function.  It depends on the parameter
  \code{type}, see details.}

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
}

