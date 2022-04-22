#' @return
#' ## When `format` is `"list"`
#'
#' For univariate models a named `list` with the element `"predicted"` which
#' contains the predicted values is returned. For categorical variables the
#' returned `list` includes the element `"probabilities"` too with a
#' `data.frame` of the predicted probabilities of each class.
#'
#' For multivariate models a named `list` is returned where there is an named
#' element for each response variable in the fitted model. Each element of this
#' list contains a inner `list` in the same format as described for the
#' univariate case, so for categorical variables, a `data.frame` with the
#' predicted probabilities is included too.
#'
#' ## When `format` is `"data.frame"`
#'
#' For univariate models a `data.frame` with the column `predicted` which
#' contains the predicted values. For categorical variables, a column for each
#' class with the predicted probability of this class is included too.
#'
#' For multivariate models a `data.frame` with a column for each response
#' variable with the predicted values of each response.
