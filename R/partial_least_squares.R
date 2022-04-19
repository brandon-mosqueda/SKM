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
#' @seealso [predict.PartialLeastSquaresModel()]
#' @family models
#'
#' @examples
#' \dontrun{
#' # Use all default hyperparameters -------------------------------------------
#' x <- to_matrix(iris[, -1])
#' y <- iris$Sepal.Length
#' model <- partial_least_squares(x, y)
#'
#' # Obtain the optimal number of components to use with predict
#' model$optimal_components_num
#'
#' # Obtain the model's coefficients
#' coef(model)
#'
#' # Predict using the fitted model
#' predictions <- predict(model, x)
#' # Obtain the predicted values
#' predictions$predicted
#'
#' # Predict with a non optimal number of components ---------------------------
#' x <- to_matrix(iris[, -1])
#' y <- iris$Sepal.Length
#' model <- partial_least_squares(x, y, method = "orthogonal")
#'
#' # Obtain the optimal number of components to use with predict
#' model$optimal_components_num
#'
#' # Predict using the fitted model with the optimal number of components
#' predictions <- predict(model, x)
#' # Obtain the predicted values
#' predictions$predicted
#'
#' # Predict using the fitted model without the optimal number of components
#' predictions <- predict(model, x, components_num = 2)
#' # Obtain the predicted values
#' predictions$predicted
#'
#' # Obtain the model's coefficients
#' coef(model)
#'
#' # Obtain the execution time taken to tune and fit the model
#' model$execution_time
#'
#' # Multivariate analysis -----------------------------------------------------
#' x <- to_matrix(iris[, -c(1, 2)])
#' y <- iris[, c(1, 2)]
#' model <- partial_least_squares(x, y, method = "wide_kernel")
#'
#' # Predict using the fitted model
#' predictions <- predict(model, x)
#' # Obtain the predicted values of the first response variable
#' predictions$Sepal.Length$predicted
#' # Obtain the predicted values of the second response variable
#' predictions$Sepal.Width$predicted
#'
#' # Obtain the predictions in a data.frame not in a list
#' predictions <- predict(model, x, format = "data.frame")
#' head(predictions)
#' }
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
