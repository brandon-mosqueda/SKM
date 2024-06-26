% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bayesian_model_model.R
\name{predict.BayesianModel}
\alias{predict.BayesianModel}
\title{Predict Bayesian model}
\usage{
\method{predict}{BayesianModel}(model, indices, format = "list")
}
\arguments{
\item{model}{(\code{Model}) An object of a fitted model.}

\item{indices}{(\code{numeric}) A numeric vector with the indices of the elements
used to fit the model you want the predictions.}

\item{format}{(\code{character(1)}) The expected format of the predictions. The
available options are \code{"list"} and \code{"data.frame"}. \code{"data.frame"} is more
useful with multivariate models if you want in a tabular structure the
predicted values. See Value section below for more information. \code{"list"}
by default.}
}
\value{
\subsection{When \code{format} is \code{"list"}}{

For univariate models a named \code{list} with the element \code{"predicted"} which
contains the predicted values is returned. For categorical variables the
returned \code{list} includes the element \code{"probabilities"} too with a
\code{data.frame} of the predicted probabilities of each class.

For multivariate models a named \code{list} is returned where there is an named
element for each response variable in the fitted model. Each element of this
list contains a inner \code{list} in the same format as described for the
univariate case, so for categorical variables, a \code{data.frame} with the
predicted probabilities is included too.
}

\subsection{When \code{format} is \code{"data.frame"}}{

For univariate models a \code{data.frame} with the column \code{predicted} which
contains the predicted values. For categorical variables, a column for each
class with the predicted probability of this class is included too.

For multivariate models a \code{data.frame} with a column for each response
variable with the predicted values of each response.
}
}
\description{
Obtains the predictions using a fitted model object of class \code{BayesianModel}.
}
\examples{
\dontrun{
# Univariate analysis -------------------------------------------------------
x <- list(list(x = to_matrix(iris[, -5]), model = "BRR"))
y <- iris$Species
model <- bayesian_model(x, y, testing_indices = c(1:5, 51:55, 101:105))

# Predict using the fitted model (of the specified testing indices)
predictions <- predict(model, indices = c(1:5, 51:55, 101:105))
# Obtain the predicted values
predictions$predicted
# Obtain the predicted probabilities
predictions$probabilities

# Predict using the fitted model (with different indices)
predictions <- predict(model, indices = 1:50)
predictions$predicted

# Multivariate analysis -----------------------------------------------------
x <- list(list(x = to_matrix(iris[, -c(1, 2)]), model = "fixed"))
y <- iris[, c(1, 2)]
y[c(5, 55, 105), 1] <- NA
y[c(6, 66, 106), 2] <- NA
model <- bayesian_model(x, y, iterations_number = 1000)

# Predict using the fitted model, with different indices and data.frame
# format
predictions <- predict(model, indices = c(10, 20, 30), format = "data.frame")
head(predictions)
}

}
