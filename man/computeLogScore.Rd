\name{computeLogScore}
\alias{computeLogScore}
\title{Compute the Log Score of predictions}
\usage{
computeLogScore(prediction, target)
}
\arguments{
  \item{prediction}{Should be a matrix where the number of
  columns is equal to the number of classes in the problem
  and each column contains the probability of a specific
  class being true, each row should sum to 1.}

  \item{target}{Should be a matrix with same dimension as
  \code{prediction} where each cell of the matrix count the
  number of successes of that particular class within that
  observational unit. Each cell of the matrix should be an
  integer greater or equal to zero.}
}
\value{
Numeric vector with length equal to the number of rows of
\code{prediction} and \code{target} containing the log
scores for each prediction point.
}
\description{
Compute the log score from multi-class probability
predictions.
}
\details{
Given a set of \code{prediction} and \code{target} values
it will compute the log score function for each prediction
point.
}

