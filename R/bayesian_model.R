#' @include utils.R
#' @include validator.R
#' @include bayesian_model_model.R

#' @title Fit a Bayesian Generalized Linear Regression Model (BGLR)
#'
#' @templateVar ClassName BayesianModel
#' @templateVar XType list
#' @templateVar YType `vector` or `matrix`
#' @templateVar refFunction BGLR::BGLR()
#'
#' @description
#' `bayesian_model()` is a wrapper of the [BGLR::BGLR()] and
#' [BGLR::Multitrait()] functions. It fits univariate models for numeric and
#' categorical response variables and multivariate models for numeric responses
#' only.
#'
#' @param x (`list`) The predictor (independent) variable(s). It is expected a
#'   `list` with nested `list`'s where each inner `list` is named and represents
#'   a predictor effect. Such inner `list`'s must have the two names:
#'   `x` (`matrix`) with the predictor variable that is going to be converted to
#'   `numeric` and `model` (`character(1)`) (case not sensitive) with the type
#'   of model to apply to this predictor term, the available models are
#'   `"FIXED"`, `"BGBLUP"`, `"BRR"`, `"Bayes_Lasso"`, `"Bayes_A"`, `"Bayes_B"`
#'   and `"Bayes_C"`. In multivariate models you can only use `"FIXED"`,
#'   `"BGBLUP"` and `"BRR"`. `"BRR"` by default.
#' @param y (`data.frame` | `vector` | `matrix`) The response (dependent)
#'   variable(s). If it is a `data.frame` or a `matrix` with 2 or more columns,
#'   a multivariate model is assumed, a univariate model otherwise. In
#'   univariate models if `y` is `character`, `logical` or `factor` a
#'   categorical response is assumed, numeric otherwise. In multivariate models
#'   all responses are coerced to numeric. `y` can contain missing values (`NA`)
#'   which represent the observations to be used as testing set along the
#'   provided indices in `testing_indices` parameter.
#' @param iterations_number (`numeric(1)`) Number of iterations to fit the
#'   model. 1500 by default.
#' @param burn_in (`numeric(1)`) Number of items to burn at the beginning of the
#'   model. 500 by default.
#' @param thinning (`numeric(1)`) Number of items to thin the model. 5 by
#'   default.
#' @param covariance_structure (`list`) (Only for multivariate models) A named
#'   `list` used to define the co-variance matrix for model residuals. This list
#'   must have the fileds `type` (`character(1)`) (case not sensitive) with one
#'   of the following values `"Unstructured"`, `"Diagonal"`, `"Factor_analytic"`
#'   or `"Recursive"`, `df0` (`numeric(1)`) with the degrees of freedom and `S0`
#'   with the covariance matrix of size `t x t`, where `t` is the number of
#'   response variables. By default the next `list` is used:
#'   `list(df0 = 5, S0 = NULL, type = "Unstructured")`.
#' @param records_weights (`numeric`) (only for univariate models with a numeric
#'   response variables) A vector of weights. If weights are provided the
#'   residual variance of each data-point is set to be proportional to the
#'   inverse of the squared-weight. `NULL` by default.
#' @param response_groups (`factor`) (only for univariate models) A vector of
#'   the same length as `y` that associates observations with groups, each group
#'   will have an associated variance component for the error term. `NULL` by
#'   default.
#' @param testing_indices (`numeric`) The records' indices to be used as testing
#'   set along all that contain missing values in `y`. `NULL` by default.
#' @template other-base-params
#'
#' @details
#' Since [BGLR] functions works a little different than other most common R
#' packages for machine learning `bayesian_model` functions adapts to it. Unlike
#' other functions, if you want to fit a bayesian model and make some
#' predictions you have to provide the whole data (for training and testing) and
#' the records' indices to be used as testing (`testing_indices`). All records
#' with `NA` values in `y` are considered as part of testing set too.
#' After fitting the model, the predicted values can be obtained with the
#' `predict` function, with no more parameter than the model.
#'
#' @template return-model
#'
#' @seealso [predict.BayesianModel()]
#' @family models
#'
#' @examples
#' \dontrun{
#' # Fit with all default parameters
#' x <- list(list(x = to_matrix(iris[, -5]), model = "BRR"))
#' model <- bayesian_model(x, iris$Species, testing_indices = 1:50)
#' predictions <- predict(model)
#'
#' # Multivariate analysis
#' x <- list(list(x = to_matrix(iris[, -c(1, 5)]), model = "BRR"))
#' y <- iris[, c(1, 5)]
#' model <- bayesian_model(
#'   x = x,
#'   y = y,
#'   iterations_number = 100,
#'   burn_in = 50
#' )
#' }
#'
#' @export
bayesian_model <- function(x, y,

                           iterations_number = 1500,
                           burn_in = 500,
                           thinning = 5,
                           covariance_structure = list(
                             df0 = 5,
                             S0 = NULL,
                             type = "Unstructured"
                           ),
                           records_weights = NULL,
                           response_groups = NULL,
                           testing_indices = NULL,

                           validate_params = TRUE,
                           seed = NULL,
                           verbose = TRUE) {
  assert_logical(validate_params, len = 1, any.missing = FALSE)

  is_multivariate <- NCOL(y) > 1

  if (validate_params) {
    validate_bayesian_model(
      x = x,
      y = y,
      is_multivariate = is_multivariate,

      iterations_number = iterations_number,
      burn_in = burn_in,
      thinning = thinning,
      covariance_structure = covariance_structure,
      records_weights = records_weights,
      response_groups = response_groups,
      testing_indices = testing_indices,

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

  model <- BayesianModel$new(
    x = x,
    y = y,
    is_multivariate = is_multivariate,

    iterations_number = iterations_number,
    burn_in = burn_in,
    thinning = thinning,
    covariance_structure = covariance_structure,
    records_weights = records_weights,
    response_groups = response_groups,
    testing_indices = testing_indices
  )

  wrapper_function <- get_verbose_function(verbose)
  wrapper_function(model$fit())

  end_time <- Sys.time()
  model$execution_time <- difftime(end_time, start_time)

  wrapper_function(print_model_time_execution(model$execution_time))

  return(model)
}
