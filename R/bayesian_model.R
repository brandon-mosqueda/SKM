#' @include utils.R
#' @include validator.R
#' @include bayesian_model_model.R

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
