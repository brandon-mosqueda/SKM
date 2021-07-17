#' @include utils.R
#' @include validator.R
#' @include random_forest_model.R

#' @export
random_forest <- function(x, y,

                          trees_number = 500,
                          node_size = 5,
                          node_depth = NULL,
                          sampled_x_vars_number = NULL,

                          tune_cv_type = "K_fold",
                          tune_folds_number = 5,
                          tune_testing_proportion = 0.2,

                          split_rule = NULL,
                          splits_number = 10,
                          importance = TRUE,
                          x_vars_weights = NULL,
                          records_weights = NULL,
                          na_action = "omit",

                          validate_params = TRUE,
                          seed = NULL,
                          verbose = TRUE) {
  assert_logical(validate_params, len = 1, any.missing = FALSE)

  is_multivariate <- length(dim(y)) > 1

  if (validate_params) {
    validate_random_forest(
      x = x,
      y = y,
      is_multivariate = is_multivariate,

      trees_number = trees_number,
      node_size = node_size,
      node_depth = node_depth,
      sampled_x_vars_number = sampled_x_vars_number,

      tune_cv_type = tune_cv_type,
      tune_folds_number = tune_folds_number,
      tune_testing_proportion = tune_testing_proportion,

      split_rule = split_rule,
      splits_number = splits_number,
      importance = importance,
      x_vars_weights = x_vars_weights,
      records_weights = records_weights,
      na_action = na_action,

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

  model <- RandomForestModel$new(
    x = x,
    y = y,
    is_multivariate = is_multivariate,

    trees_number = trees_number,
    node_size = node_size,
    node_depth = node_depth,
    sampled_x_vars_number = sampled_x_vars_number,

    tune_cv_type = tune_cv_type,
    tune_folds_number = tune_folds_number,
    tune_testing_proportion = tune_testing_proportion,

    split_rule = split_rule,
    splits_number = splits_number,
    importance = importance,
    x_vars_weights = x_vars_weights,
    records_weights = records_weights,
    na_action = na_action
  )

  wrapper_function <- get_verbose_function(verbose)
  wrapper_function(model$fit())

  end_time <- Sys.time()
  model$execution_time <- difftime(end_time, start_time)

  wrapper_function(print_model_time_execution(model$execution_time))

  return(model)
}
