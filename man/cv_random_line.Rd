% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{cv_random_line}
\alias{cv_random_line}
\title{Random line cross validation folds generation}
\usage{
cv_random_line(lines, folds_number = 5, testing_proportion = 0.2)
}
\arguments{
\item{lines}{(\code{vector}) The vector of all lines.}

\item{folds_number}{(\code{numeric(1)}) The number of folds to generate.}

\item{testing_proportion}{(\code{numeric(1)}) The proportion of lines to be taken
to be the testing set in each fold.}
}
\value{
A \code{list} with \code{folds_number} elements where each element is a named \code{list}
with the elements \code{training} wich includes the indices of those records to be
part of the training set and \code{testing} wich includes the indices of those
records to be part of the testing set. Training and testing sets of each fold
are exhaustive and mutually exclusive.
}
\description{
This method is designed in the context of genomic selection where we have a
vector of lines and we want to generate folds for cross validation. In each
fold a proportion of lines is taken to be the testing set and the remaining
ones to be the training set.
}
\examples{
\dontrun{
# Generates random data
lines <- rep(paste0("line", 1:10), 4)
folds <- cv_random_line(lines, 5, 0.2)
# Indices of training set in fold 1
folds[[1]]$training
# Indices of testing set in fold 1
folds[[1]]$testing
}

}
