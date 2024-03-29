% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RBO_Utility_Max.R
\name{Utility_Max}
\alias{Utility_Max}
\title{Utility Maximization Function}
\usage{
Utility_Max(DT_bounds, GP, acq = "ucb", y_max, kappa, eps)
}
\arguments{
\item{DT_bounds}{hyperparameters lower and upper bounds to limit the search of the acq max}

\item{GP}{an object of class GP}

\item{acq}{Acquisition function type to be used}

\item{y_max}{The current maximum known value of the target utility function}

\item{kappa}{tunable parameter kappa to balance exploitation against exploration}

\item{eps}{tunable parameter epsilon to balance exploitation against exploration}
}
\value{
The arg max of the acquisition function
}
\description{
Utility Maximization.
}
\keyword{internal}
