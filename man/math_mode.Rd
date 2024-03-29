% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metrics.R
\name{math_mode}
\alias{math_mode}
\title{Mathematical Mode}
\usage{
math_mode(x, remove_na = TRUE, allow_multimodal = TRUE)
}
\arguments{
\item{x}{(\code{factor}) A vector of values to compute the mode.}

\item{remove_na}{(\code{logical(1)}) Should \code{NA} values be removed and not
include them in the frequencies counting? \code{TRUE} by default.}

\item{allow_multimodal}{(\code{logical(1)}) If there is more than one mode, should
all the most frequent values be returned? \code{TRUE} by default.}
}
\value{
A \code{character} vector with the most frequent values if \code{allow_multimodal} is
\code{TRUE} or the first most frequent value (after ordering lexicographically) if
\code{allow_multimodal} is \code{FALSE}.

The returned vector contains the attribute \code{"frequency"} with an integer of
the greatest frequency.
}
\description{
Obtain the most frecuent values (mode) with the frequency from a vector. By
default all values with the greatest frequency are returned but you can
limit the result to the first one, in lexicographical order.
}
\examples{
\dontrun{
# Multimodal
x <- math_mode(c("C", "A", "C", "A"))
# Obtain the greatest frequency
attr(x, "frequency")

# Single mode
x <- math_mode(c("C", "A", "C", "A"), allow_multimodal = FALSE)

# Multimodal
x <- math_mode(iris$Species)
attr(x, "frequency")

values <- c("A", "B", NA, "C", "A", NA, "B")
# Multimodal without NAs
x <- math_mode(values)
# Multimodal with NAs
x <- math_mode(values, remove_na = FALSE)
}

}
\seealso{
Other categorical_metrics: 
\code{\link{accuracy}()},
\code{\link{brier_score}()},
\code{\link{categorical_summary}()},
\code{\link{confusion_matrix}()},
\code{\link{f1_score}()},
\code{\link{kappa_coeff}()},
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
