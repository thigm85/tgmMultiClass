\name{computeMultiClassScore}
\alias{computeMultiClassScore}
\title{Compute scores from multi-class probability predictions.}
\usage{
computeMultiClassScore(prediction, target, type, ...)
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

  \item{type}{Which score function to use.}
}
\value{
Numeric vector with length equal to the number of rows of
\code{prediction} and \code{target} containing the scores
for each prediction point.
}
\description{
Compute scores from multi-class probability predictions.
}
\details{
Given a set of \code{prediction} and \code{target} values
it will compute the score function for each prediction
point.
}

