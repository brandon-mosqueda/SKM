% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/genomic_selection.R
\name{gs_summaries}
\alias{gs_summaries}
\title{Summaries for Genomic Selection}
\usage{
gs_summaries(predictions, save_at = NULL, digits = 4)
}
\arguments{
\item{predictions}{(\code{data.frame}) The genomic, observed and predicted data.
The \code{data.frame} must contain the columns \code{Fold}, \code{Line}, \code{Env}, \code{Observed}
and \code{Predicted}. If the response variable is categorical make sure to set
Observed and Predicted as factor. For categorical response variables it is
also necessary to include a column for each class in the response variable
with the predicted probabilities, the column must have the class as name.
If \code{predictions} contains a column nameed \code{Trait}, the summaries will be
computed and saved per trait.}

\item{save_at}{(\code{character(1)}) The directory's name where the summaries and
predictions are going to be saved as CSV. If \code{NULL} is provided, the
results are only returned but not saved. \code{NULL} by default.}

\item{digits}{(\code{numeric(1)}) Digits of precision to use in the summaries. 4
by default.}
}
\value{
A \code{list} with 3 summaries: \code{"line"}, \code{"env"} and \code{"fold"}.
}
\description{
Generates summaries of predictions from different folds for Genomic Selection
analysis.
}
\examples{
\dontrun{
# For a continuous response ------------------------------------------------
set.seed(1)

# Simulated data
predictions <- data.frame(
  Fold = rep(c("F1", "F2"), each = 24),
  Env = rep(c("E1", "E2"), each = 12),
  Line = rep(c("L1", "L2", "L3"), 16),
  Observed = rnorm(48, 10, 1),
  Predicted = rnorm(48, 10, 1)
)

summaries <- gs_summaries(predictions, save_at = "numeric_summaries")
summaries$line
summaries$env
summaries$fold

# For a categorical response ------------------------------------------------
set.seed(2)

# Simulated data
predictions <- data.frame(
    Fold = rep(c("F1", "F2"), each = 24),
    Env = rep(c("E1", "E2"), each = 12),
    Line = rep(c("L1", "L2", "L3"), 16),
    Observed = sample(c("A", "B"), 24, TRUE),
    A = runif(48, 0, 1)
  ) \%>\%
  dplyr::mutate(
    B = 1 - A,
    Predicted = factor(ifelse(A > 0.5, "A", "B")),
    Observed = factor(Observed)
  )
summaries <- gs_summaries(predictions, save_at = "categorical_summaries")
summaries$line
summaries$env
summaries$fold
}

}
