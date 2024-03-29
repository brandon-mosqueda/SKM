% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metrics.R
\name{brier_score}
\alias{brier_score}
\title{Brier Score}
\usage{
brier_score(observed, probabilities, remove_na = TRUE)
}
\arguments{
\item{observed}{(\code{factor}) The observed values. It has to have the same
length as rows \code{probabilities}.}

\item{probabilities}{(\code{data.frame}) The probability of each class for each
individual. It is required the columns names of \code{probabilities} corresponds
to all classes (levels) in \code{observed} and that \code{probabilities} has as many
rows as records \code{observed}.}

\item{remove_na}{(\code{logical(1)}) Should \code{NA} values be removed?. \code{TRUE} by
default.}
}
\value{
A single numeric value with the Brier Score.
}
\description{
Given the observed values and the predicted probabilites of categorical data
(of at least two classes) computes the Brier Score.
}
\details{
Brier score is computed as:

\figure{brier_score.png}{(1 / N) *  sum(sum((f_{ti} - o_{ti})^2))}

Where R is the number of possible classes in which the event can fall, and N
the overall number of instances of all classes. f_{ti} is the predicted
probability for class i. o_{ti} is 1 if it is i-th class in instant t, 0
otherwise.
}
\examples{
\dontrun{
probs <- data.frame(a = c(0.7, 0.2), b = c(0.3, 0.8))
brier_score(factor(c("a", "b")), probs)

probs <- data.frame(
  a = c(0.2, 0.8, 0.3),
  b = c(0.3, 0.1, 0.3),
  c = c(0.5, 0.1, 0.4),
)

brier_score(factor(c("a", "a", "c")), probs)


probs <- data.frame(a = 1, b = 0)
brier_score("a", probs)
}

}
\seealso{
Other categorical_metrics: 
\code{\link{accuracy}()},
\code{\link{categorical_summary}()},
\code{\link{confusion_matrix}()},
\code{\link{f1_score}()},
\code{\link{kappa_coeff}()},
\code{\link{math_mode}()},
\code{\link{matthews_coeff}()},
\code{\link{pccc}()},
\code{\link{pcic}()},
\code{\link{pr_auc}()},
\code{\link{precision}()},
\code{\link{recall}()},
\code{\link{roc_auc}()},
\code{\link{sensitivity}()},
\code{\link{specificity}()}
}
\concept{categorical_metrics}
