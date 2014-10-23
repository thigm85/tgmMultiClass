\name{predict_r_vgam}
\alias{predict_r_vgam}
\title{multi-class prediction probability with 'vgam'}
\usage{
predict_r_vgam(data, training, validation, test, ...)
}
\arguments{
  \item{data}{A data.frame containing the response variable
  and the features used for fitting and predictions.}

  \item{training}{A matrix where each column contain
  integers that indicate which lines of 'data' will be used
  for training.}

  \item{validation}{Similar to \code{training} but contain
  the lines used for validation, in case there are tuning
  parameters.}

  \item{test}{Similar to \code{training} but contain the
  lines used for test.}

  \item{...}{Arguments to be used in the \code{vgam}
  function.}
}
\value{
It return an S3 object of class c("multiClass_vgam",
"multiClass"). This object contains the predicted
probabilities for each test set. See the examples to get
more info about how to extract the desired info from this
object.
}
\description{
\code{predict_r_vgam} apply \code{vgam} function to
different training and test sets.
}
\details{
Different datasets are formed from \code{data} according to
the indexes contained in \code{training}, \code{validation}
and \code{test}. \code{predict_r_vgam} will then train with
the training and validation part of the data using
\code{vgam} and output the predicted probabilities on the
test sets.
}

