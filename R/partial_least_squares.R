#' @include utils.R
#' @include validator.R
#' @include partial_least_squares_model.R

#' @title Fit a Partial Least Squares Regression Model (PLSR)
#'
#' @templateVar ClassName PartialLeastSquaresModel
#' @templateVar XType `matrix`
#' @templateVar YType `vector` or `m̀atrix`
#' @templateVar refFunction pls::plsr()
#'
#' @description
#' `partial_least_squares()` is a wrapper of the [pls::plsr()] function to fit
#' a partial least squares regression model. You can fit univariate and
#' multivariate models for numeric responses only.
#'
#' @template x-matrix-param
#' @param y (`data.frame` | `vector` | `matrix`) The response (dependent)
#'   variable(s). If it is a `data.frame` or a `matrix` with 2 or more columns,
#'   a multivariate model is assumed, a univariate model otherwise. All the
#'   variables are coerced to numeric before training the model.
#' @param method (`character(1)`) (case not sensitive) The type of model to fit.
#'   The available options are the kernel algorithm (`"kernel"`), the wide
#'   kernel algorithm (`"wide_kernel"`), SIMPLS (`"simpls"`), and the classical
#'   orthogonal scores algorithm (`"orthogonal"`). `"kernel"` by default.
#' @param scale (`logical`) A logical vector indicating the variables in `x`
#'   to be scaled. If `scale` is of length 1, the value is recycled as many
#'   times as needed. `TRUE` by default.
#' @template other-base-params
#'
#' @details
#' You have to consider that all columns without variance (where all
#' the records has the same value) are removed. Such columns positions are
#' returned in the removed_x_cols field of the returned object.
#'
#' This function performs random cross validation with 10 folds in order to find
#' the optimal number of components to use. This optimal value is used when
#' you call `predict` using the fitted model but you can specify other number
#' of components to make the predictions.
#' @template details-remove-nas
#'
#' @return
#' An object of class `"PartialLeastSquaresModel"` that inherits from classes
#' `"Model"` and `"R6"` with the fields:
#'
#' * `fitted_model`: An object of class [pls::plsr()] with the model.
#' * `x`: The final `m̀atrix` used to fit the model.
#' * `y`: The final `vector` or `m̀atrix` used to fit the model.
#' * `optimal_components_num`: A `numeric` value with the optimal number of
#'   components obtained with cross validation and used to fit the model.
#' * `execution_time`: A `difftime` object with the total time taken to tune and
#'   fit the model.
#' * `removed_rows`: A `numeric` vector with the records' indices (in the
#'   provided position) that were deleted and not taken in account in tunning
#'   nor training.
#' * `removed_x_cols`: A `numeric` vector with the columns' indices (in the
#'   provided positions) that were deleted and not taken in account in tunning
#'   nor training.
#' * `...`: Some other parameters for internal use.
#'
#' @seealso [predict.PartialLeastSquaresModel()], [coef.Model()]
#' @family models
#'
#' @example inst/examples/partial_least_squares.R
#'
#' @export
partial_least_squares <- function(x, y,

                                  method = "kernel",
                                  scale = FALSE,

                                  validate_params = TRUE,
                                  seed = NULL,
                                  verbose = TRUE) {
  assert_logical(validate_params, len = 1, any.missing = FALSE)
  is_multivariate <- NCOL(y) > 1

  if (validate_params) {
    validate_partial_least_squares(
      x = x,
      y = y,

      is_multivariate = is_multivariate,
      method = method,
      scale = scale,

      seed = seed,
      verbose = verbose
    )
  }

  old_random_state <- NULL
  if (!is.null(seed)) {
    old_random_state <- get_rand_state()

    set.seed(seed)
  }
  on.exit(set_rand_state(old_random_state))

  start_time <- Sys.time()

  model <- PartialLeastSquaresModel$new(
    x = x,
    y = y,
    is_multivariate = is_multivariate,

    method = method,
    scale = scale
  )

  wrapper_function <- get_verbose_function(verbose)
  wrapper_function(model$fit())

  end_time <- Sys.time()
  model$execution_time <- difftime(end_time, start_time)

  wrapper_function(print_model_time_execution(model$execution_time))

  return(model)
}
