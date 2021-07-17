#' @include model.R
#' @include validator.R

#' @export
generalized_linear_model <- function(x, y,
                                     is_multivariate = length(dim(y)) > 1,

                                     alpha = 1,

                                     tune_cv_type = "K_fold",
                                     tune_folds_number = 5,
                                     tune_testing_proportion = 0.2,

                                     lambda = NULL,
                                     lambdas_number = 100,
                                     lambda_min_ratio = ifelse(
                                       nrow(x) < ncol(x),
                                       0.01,
                                       1e-04
                                     ),
                                     records_weights = NULL,
                                     standardize = TRUE,
                                     fit_intercept = TRUE,

                                     validate_params = TRUE,
                                     seed = NULL,
                                     verbose = TRUE) {
  assert_logical(validate_params, len = 1, any.missing = FALSE)

  if (validate_params) {
    validate_generalized_linear_model(
      x = x,
      y = y,
      is_multivariate = is_multivariate,

      alpha = alpha,
      lambda = lambda,

      tune_cv_type = tune_cv_type,
      tune_folds_number = tune_folds_number,
      tune_testing_proportion = tune_testing_proportion,

      lambdas_number = lambdas_number,
      lambda_min_ratio = lambda_min_ratio,
      records_weights = records_weights,
      standardize = standardize,
      fit_intercept = fit_intercept,

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

  model <- GeneralizedLinearModel$new(
    x = x,
    y = y,
    is_multivariate = is_multivariate,

    alpha = alpha,
    lambda = lambda,

    tune_cv_type = tune_cv_type,
    tune_folds_number = tune_folds_number,
    tune_testing_proportion = tune_testing_proportion,

    lambdas_number = lambdas_number,
    lambda_min_ratio = lambda_min_ratio,
    records_weights = records_weights,
    standardize = standardize,
    fit_intercept = fit_intercept
  )

  wrapper_function <- get_verbose_function(verbose)
  wrapper_function(model$fit())

  end_time <- Sys.time()
  model$execution_time <- difftime(end_time, start_time)

  wrapper_function(print_model_time_execution(model$execution_time))

  return(model)
}
