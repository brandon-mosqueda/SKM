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
#' `partial_least_squares()` is a wrapper of the [pls::plsr()] function with
#' It fits univariate models for continuous response variables and multivariate
#' models for numeric responses only.
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
#' @param components_num (`numeric`) The number of components to include in the
#'   model. `NULL` by default which chooses the best number of components based
#'   on the standard error of the cross-validation residuals heuristic in
#'   univariate models and the maximal number of components in multivariate
#'   models.
#' @param scale (`logical`) A logical vector indicating the variables in `x`
#'   to be scaled. If `scale` is of length 1, the value is recycled as many
#'   times as needed. `TRUE` by default.
#' @template other-base-params
#'
#' @template details-no-variance
#' @template details-remove-nas
#' @template return-model
#'
#' @seealso [predict.Model()]
#' @family models
#'
#' @examples
#' \dontrun{
#' # Fit with all default parameters
#' model <- partial_least_squares(to_matrix(iris[, -5]), iris$Species)
#'
#' predictions <- predict(model, to_matrix(iris))
#' predictions$predicted
#' }
#'
#' @export
partial_least_squares <- function(x, y,

                                  method = "kernel",
                                  components_num = NULL,
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
      components_num = components_num,
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

    method = method,
    components_num = components_num,
    scale = scale
  )

  wrapper_function <- get_verbose_function(verbose)
  wrapper_function(model$fit())

  end_time <- Sys.time()
  model$execution_time <- difftime(end_time, start_time)

  wrapper_function(print_model_time_execution(model$execution_time))

  return(model)
}
