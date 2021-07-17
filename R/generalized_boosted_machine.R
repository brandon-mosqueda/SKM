#' @include utils.R
#' @include validator.R
#' @include random_forest_model.R

#' @export
generalized_boosted_machine <- function(x, y,

                                        trees_number = 100,
                                        max_depth = 1,
                                        node_size = 10,
                                        shrinkage = 0.1,
                                        sampled_records_proportion = 0.5,

                                        tune_cv_type = "K_fold",
                                        tune_folds_number = 5,
                                        tune_testing_proportion = 0.2,

                                        records_weights = NULL,
                                        predictors_relationship = NULL,
                                        cores_number = NULL,

                                        validate_params = TRUE,
                                        seed = NULL,
                                        verbose = TRUE) {
  assert_logical(validate_params, len = 1, any.missing = FALSE)

  if (validate_params) {
    validate_generalized_boosted_machine(
      x = x,
      y = y,

      trees_number = trees_number,
      max_depth = max_depth,
      node_size = node_size,
      shrinkage = shrinkage,
      sampled_records_proportion = sampled_records_proportion,

      tune_cv_type = tune_cv_type,
      tune_folds_number = tune_folds_number,
      tune_testing_proportion = tune_testing_proportion,

      records_weights = records_weights,
      predictors_relationship = predictors_relationship,
      cores_number = cores_number,

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

  model <- GeneralizedBoostedMachineModel$new(
    x = x,
    y = y,

    trees_number = trees_number,
    max_depth = max_depth,
    node_size = node_size,
    shrinkage = shrinkage,
    sampled_records_proportion = sampled_records_proportion,

    tune_cv_type = tune_cv_type,
    tune_folds_number = tune_folds_number,
    tune_testing_proportion = tune_testing_proportion,

    records_weights = records_weights,
    predictors_relationship = predictors_relationship,
    cores_number = cores_number
  )

  wrapper_function <- get_verbose_function(verbose)
  wrapper_function(model$fit())

  end_time <- Sys.time()
  model$execution_time <- difftime(end_time, start_time)

  wrapper_function(print_model_time_execution(model$execution_time))

  return(model)
}
