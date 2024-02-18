#' @include utils.R
#' @include validator.R
#' @include uvcov_model_model.R

#' @title Fit a UVCov Model (lme4GS)
#'
#' @templateVar ClassName UVCovModel
#' @templateVar XType list
#' @templateVar YType `vector`
#' @templateVar refFunction lme4GS::lmerUvcov()
#'
#' @description
#' `uvcov_model()` is a wrapper of the `lme4GS::lmerUvcov()` and function to fit
#' models for Genomic Selection. It only supports univariate models with a
#' numeric response variable.
#'
#' @param x (`list`) The predictor (independent) variable(s). It is expected a
#'   `list` with nested `list`'s where each inner `list` is named and represents
#'   a predictor effect. Such inner `list`'s must have a field `x` with the:
#'   square `matrix` of predictor variables.
#' @param y (`numeric`) The response (dependent) variable(s). As this function
#'   only works for univariate analysis, a numeric vector is always expected.
#'   `y` can contain missing values (`NA`) which represent the observations to
#'   be used as testing set along with the provided indices in `testing_indices`
#'   parameter.
#' @param testing_indices (`numeric`) The records' indices to be used as testing
#'   set along all that contain missing values in `y`. `NULL` by default.
#' @template other-base-params
#'
#' @details
#' This functions has a similar work as the `bayesian_model` function. Unlike
#' other models, if you want to fit a UvCov model and make some
#' predictions you have to provide the whole data (for training and testing) and
#' the records' indices to be used as testing (`testing_indices`). All records
#' with `NA` values in `y` are considered as part of testing set too.
#' After fitting the model, the predicted values can be obtained with the
#' `predict` function, with no more parameter than the model, see Examples
#' section below for more information.
#'
#' @return
#' An object of class `"UVCovModel"` that inherits from classes
#' `"Model"` and `"R6"` with the fields:
#'
#' * `fitted_model`: An object of class `lme4GS::lmerUvcov()` with the model.
#' * `x`: The final `list` used to fit the model.
#' * `y`: The final `vector` or `matrix` used to fit the model.
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
#' @seealso [predict.UVCovModel()]
#' @family models
#'
#' @example inst/examples/uvcov_model.R
#'
#' @export
uvcov_model <- function(x, y,
                        testing_indices = NULL,

                        validate_params = TRUE,
                        seed = NULL,
                        verbose = TRUE) {
  assert_logical(validate_params, len = 1, any.missing = FALSE)

  if (validate_params) {
    validate_uvcov_model(
      x = x,
      y = y,

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

  model <- UVCovModel$new(
    x = x,
    y = y,
    is_multivariate = FALSE,

    testing_indices = testing_indices
  )

  wrapper_function <- get_verbose_function(verbose)
  wrapper_function(model$fit())

  end_time <- Sys.time()
  model$execution_time <- difftime(end_time, start_time)

  wrapper_function(print_model_time_execution(model$execution_time))

  return(model)
}
