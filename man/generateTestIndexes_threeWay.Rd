\name{generateTestIndexes_threeWay}
\alias{generateTestIndexes_threeWay}
\title{Three Way wrapper function to be used within \code{generateTestIndexes}.}
\usage{
generateTestIndexes_threeWay(dataset, options, observational_unit = NULL)
}
\arguments{
  \item{options}{It is a list with: \code{prop_v}, which is
  a number between 0 and 1 that describes the proportion of
  the data that goes into the validation set,
  \code{prop_test}, which is a number between 0 and 1 that
  describes the proportion of the data that goes into the
  test set and \code{number_replicates} to indicate how
  many data replications we want.}

  \item{dataset}{Data.frame with the complete dataset
  available.}

  \item{observational_unit}{Column name within
  \code{dataset} that will be used as reference to assign
  rows to training, validation and test sets. The same
  observational unit cannot be assigned to more than one of
  training, validation or test sets. Default is set to
  \code{NULL}, meaning that each row is an observational
  unit.}
}
\description{
Three Way wrapper function to be used within
\code{generateTestIndexes}.
}

