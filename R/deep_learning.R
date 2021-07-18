#' @include utils.R
#' @include validator.R
#' @include deep_learning_model.R

#' @export
deep_learning <- function(x, y,

                          learning_rate = 0.001,
                          epochs_number = 500,
                          batch_size = 32,
                          layers = list(
                            list(
                              neurons_number = 0.5,
                              activation = "relu",
                              dropout = 0,
                              ridge_penalty = 0,
                              lasso_penalty = 0
                            )
                          ),
                          output_penalties = list(
                            ridge_penalty = 0,
                            lasso_penalty = 0
                          ),

                          tune_cv_type = "K_fold",
                          tune_folds_number = 5,
                          tune_testing_proportion = 0.2,

                          early_stop = FALSE,
                          early_stop_patience = 50,

                          validate_params = TRUE,
                          seed = NULL,
                          verbose = TRUE) {
  assert_logical(validate_params, len = 1, any.missing = FALSE)

  is_multivariate <- length(dim(y)) > 1

  if (validate_params) {
    validate_deep_learning(
      x = x,
      y = y,
      is_multivariate = is_multivariate,

      learning_rate = learning_rate,
      epochs_number = epochs_number,
      batch_size = batch_size,
      layers = layers,
      output_penalties = output_penalties,

      tune_cv_type = tune_cv_type,
      tune_folds_number = tune_folds_number,
      tune_testing_proportion = tune_testing_proportion,

      early_stop = early_stop,
      early_stop_patience = early_stop_patience,

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

  model <- DeepLearningModel$new(
    x = x,
    y = y,
    is_multivariate = is_multivariate,

    learning_rate = learning_rate,
    epochs_number = epochs_number,
    batch_size = batch_size,
    layers = layers,
    output_penalties = output_penalties,

    tune_cv_type = tune_cv_type,
    tune_folds_number = tune_folds_number,
    tune_testing_proportion = tune_testing_proportion,

    early_stop = early_stop,
    early_stop_patience = early_stop_patience
  )

  wrapper_function <- get_verbose_function(verbose)
  wrapper_function(model$fit())

  end_time <- Sys.time()
  model$execution_time <- difftime(end_time, start_time)

  wrapper_function(print_model_time_execution(model$execution_time))

  return(model)
}
