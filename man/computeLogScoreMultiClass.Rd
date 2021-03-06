\name{computeLogScoreMultiClass}
\alias{computeLogScoreMultiClass}
\title{Compute prediction log scores for a \code{multiClass} object.}
\usage{
computeLogScoreMultiClass(resample_indexes, pred_obj)
}
\arguments{
  \item{resample_indexes}{A \code{datasetResample} object,
  usually generated by the \code{generateTestIndexes}
  function.}

  \item{pred_obj}{A \code{multiClass} object, usually
  generated from the \code{datasetResample} object with
  some predictive model with an interface function to the
  \code{tgmMultiClass} package.}
}
\value{
A matrix where each column represents the prediction log
scores for a given replication. The number of rows is equal
to the number of prediction points.
}
\description{
Compute prediction log scores for a \code{multiClass}
object.
}
\details{
Given a \code{datasetResample} object and a compatible
\code{multiClass} prediction object it compute log scores
of the predictions for each replication.
}

