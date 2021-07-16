#' @include model.R
#' @include validator.R

#' @export
support_vector_machine <- function(x, y,

                                   kernel = "linear",
                                   degree = 3,
                                   gamma = if (is.vector(x)) 1 else 1 / ncol(x),
                                   coef0 = 0,
                                   cost = 1,

                                   tune_cv_type = "K_fold",
                                   tune_folds_number = 5,
                                   tune_testing_proportion = 0.2,

                                   scale = TRUE,
                                   class_weights = NULL,
                                   cache_size = 40,
                                   tolerance = 0.001,
                                   epsilon = 0.1,
                                   shrinking = TRUE,
                                   fitted = TRUE,

                                   validate_params = TRUE,
                                   seed = NULL,
                                   verbose = TRUE) {
  assert_logical(validate_params, len = 1, any.missing = FALSE)

  if (validate_params) {
    validate_support_vector_machine(
      x = x,
      y = y,

      kernel = kernel,
      degree = degree,
      gamma = gamma,
      coef0 = coef0,
      cost = cost,

      tune_cv_type = tune_cv_type,
      tune_folds_number = tune_folds_number,
      tune_testing_proportion = tune_testing_proportion,

      scale = scale,
      class_weights = class_weights,
      cache_size = cache_size,
      tolerance = tolerance,
      epsilon = epsilon,
      shrinking = shrinking,
      fitted = fitted,

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

  model <- SupportVectorMachineModel$new(
    x = x,
    y = y,

    kernel = kernel,
    degree = degree,
    gamma = gamma,
    coef0 = coef0,
    cost = cost,

    tune_cv_type = tune_cv_type,
    tune_folds_number = tune_folds_number,
    tune_testing_proportion = tune_testing_proportion,

    scale = scale,
    class_weights = class_weights,
    cache_size = cache_size,
    tolerance = tolerance,
    epsilon = epsilon,
    shrinking = shrinking,
    fitted = fitted
  )

  wrapper_function <- get_verbose_function(verbose)
  wrapper_function(model$fit())

  end_time <- Sys.time()
  model$execution_time <- difftime(end_time, start_time)

  wrapper_function(print_model_time_execution(model$execution_time))

  return(model)
}