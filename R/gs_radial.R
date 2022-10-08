#' @include utils.R
#' @include validator.R
#' @include gs_radial_model.R

gs_radial <- function(lines,
                      envs,
                      y,
                      Geno = NULL,
                      Markers = NULL,
                      predictors = list(Line = "BGBLUP"),

                      rho = NULL,
                      iterations_number = 1500,
                      burn_in = 500,
                      thinning = 5,
                      testing_indices = NULL,

                      tune_type = "Grid_search",
                      tune_cv_type = "K_fold",
                      tune_folds_number = 5,
                      tune_testing_proportion = 0.2,
                      tune_folds = NULL,
                      tune_loss_function = NULL,
                      tune_grid_proportion = 1,
                      tune_bayes_samples_number = 10,
                      tune_bayes_iterations_number = 10,

                      validate_params = TRUE,
                      seed = NULL,
                      verbose = TRUE) {
  assert_logical(validate_params, len = 1, any.missing = FALSE)

  is_multivariate <- NCOL(y) > 1

  if (validate_params) {
    validate_gs_radial(
      is_multivariate = is_multivariate,
      lines = lines,
      envs = envs,
      y = y,
      Geno = Geno,
      Markers = Markers,
      predictors = predictors,

      rho = rho,
      iterations_number = iterations_number,
      burn_in = burn_in,
      thinning = thinning,
      testing_indices = testing_indices,

      tune_type = tune_type,
      tune_cv_type = tune_cv_type,
      tune_folds_number = tune_folds_number,
      tune_testing_proportion = tune_testing_proportion,
      tune_folds = tune_folds,
      tune_loss_function = tune_loss_function,
      tune_grid_proportion = tune_grid_proportion,
      tune_bayes_samples_number = tune_bayes_samples_number,
      tune_bayes_iterations_number = tune_bayes_iterations_number,

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

  model <- GSRadialModel$new(
    is_multivariate = is_multivariate,
    lines = lines,
    envs = envs,
    y = y,
    Geno = Geno,
    Markers = Markers,
    predictors = predictors,

    rho = rho,
    iterations_number = iterations_number,
    burn_in = burn_in,
    thinning = thinning,
    testing_indices = testing_indices,

    tune_type = tune_type,
    tune_cv_type = tune_cv_type,
    tune_folds_number = tune_folds_number,
    tune_testing_proportion = tune_testing_proportion,
    tune_folds = tune_folds,
    tune_loss_function = tune_loss_function,
    tune_grid_proportion = tune_grid_proportion,
    tune_bayes_samples_number = tune_bayes_samples_number,
    tune_bayes_iterations_number = tune_bayes_iterations_number
  )

  wrapper_function <- get_verbose_function(verbose)
  wrapper_function(model$fit())

  end_time <- Sys.time()
  model$execution_time <- difftime(end_time, start_time)

  wrapper_function(print_model_time_execution(model$execution_time))

  return(model)
}
