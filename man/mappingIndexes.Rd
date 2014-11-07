\name{mappingIndexes}
\alias{mappingIndexes}
\title{Creates a new index matrix based on a mapping data frame}
\usage{
mappingIndexes(unit_index_matrix, mapping_from, mapping_to)
}
\description{
Creates a new index matrix based on a mapping data frame
}
\details{
As an example, this function is used to convert resampling
indexes that were constructed wrt observational units back
to the rows mapping
}
\examples{
mapping <- data.frame(from = rep(1:5, times = 4), to = 1:20)
unit_index_matrix <- matrix(c(1,3,5), nrow = 3, ncol = 2)

new_index_matrix <- mappingIndexes(unit_index_matrix = unit_index_matrix,
                                   mapping_from = mapping$from,
                                   mapping_to = mapping$to)
}

