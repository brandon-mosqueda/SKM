% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{cv_one_env_out}
\alias{cv_one_env_out}
\title{Leave one environment out cross validation folds generation}
\usage{
cv_one_env_out(envs, envs_proportion = 1, folds_per_env = 5)
}
\arguments{
\item{envs}{(\code{vector}) The vector of all environments.}

\item{envs_proportion}{(\code{numeric(1)}) The proportion of data within each
environment to be taken randomly to be the testing set in each fold. If 1,
then all the whole environment is taken to be the testing set. 1 by
default.}

\item{folds_per_env}{(\code{numeric(1)}) The number of folds to generate for each
environment when \code{envs_proportion} is less than 1. 5 by default.}
}
\value{
A \code{list} with \code{length(unique(envs)) * folds_per_env} elements where each
element is a named \code{list} with the elements \code{training} wich includes the
indices of those records to be part of the training set and \code{testing} wich
includes the indices of those records to be part of the testing set.
Training and testing sets of each fold are exhaustive and mutually exclusive.
}
\description{
Generates folds for the leave one environment out cross validation. In each
fold on environment is taken to be the testing set and all the remaining ones
to be the training set. It is a special case of the \code{cv_one_group_out} where
the groups are the environments.
}
\examples{
\dontrun{
# Generates random data
envs <- rep(paste0("env", 1:4), 10)
folds <- cv_one_env_out(envs)
# Indices of training set in fold 1
folds[[1]]$training
# Indices of testing set in fold 1
folds[[1]]$testing
}

}
