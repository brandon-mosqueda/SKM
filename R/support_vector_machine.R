#' @include utils.R
#' @include validator.R
#' @include support_vector_machine_model.R

#' @title Fit a support vector machine
#'
#' @description
#' `support_vector_machine()` is a wrapper of the [e1071::svm()] function with
#' the ability to tune the hyperparameters (grid search) in a simple way. It
#' fits univariate models for numeric and categorical response variables.
#' @template tunable-description
#'
#' @template x-matrix-param
#' @param y (`data.frame` | `vector` | `matrix`) The response (dependent)
#'   variable. If it is a `data.frame` or a `matrix` it must have only one
#'   column. If y is `character`, `logical` or `factor`, then a classification
#'   machine is fitted, otherwise a regression machine.
#' @param kernel (`character(1)`) (case not sensitive) The kernel used in the
#' support vector machine.
#'
#' * linear:
#'
#'     ![](linear_kernel.png "Linear kernel")
#'
#' * polynomial:
#'
#'     ![](polynomial_kernel.png "Polynomial kernel")
#'
#' * radial:
#'
#'     ![](radial_kernel.png "Radial kernel")
#'
#' * sigmoid:
#'
#'     ![](sigmoid_kernel.png "Sigmoid kernel")
#'
#' You can use `gamma`, `coef0` and `degree` parameters to adjust the kernel.
#' `"linear"` by default.
#' @param degree (`numeric`) (__tunable__) Parameter needed for `"polynomial"`
#'   kernel. 3 by default.
#' @param gamma (`numeric`) (__tunable__) Parameter needed for all kernels
#'   except `"linear"`. `if (is.vector(x)) 1 else 1 / ncol(x)` by default.
#' @param coef0 (`numeric`) (__tunable__) Parameter needed for `"polynomial"`
#'   and `"sigmoid"` kernels. 0 by default.
#' @template cv-tune-params
#' @param scale (`logical`) A logical vector indicating the variables in `x`
#'   to be scaled. If `scale` is of length 1, the value is recycled as many
#'   times as needed. `TRUE` by default.
#' @param class_weights (`numeric` | `character`) For categorical responses only.
#'   A named vector of weights for the different classes, used for asymmetric
#'   class sizes. Not all factor levels have to be supplied (default weight: 1).
#'   All components have to be named. Specifying "inverse" (case not sensitive)
#'   will choose the weights inversely proportional to the class distribution.
#'   `NULL` by default.
#' @param cache_size (`numeric(1)`) Cache memory in MB. 40 by default.
#' @param tolerance (`numeric(1)`) Tolerance of termination criterion. 0.001
#'   by default.
#' @param epsilon (`numeric(1)`) Epsilon in the insensitive-loss function. 0.1
#'   by default.
#' @param shrinking (`logical(1)`) Do you want to use the shrinking-heuristics?
#'   `TRUE` by default.
#' @param fitted (`logical(1)`) Should the fitted values be computed and
#'   included in the model? `TRUE` by default.
#' @template other-base-params
#'
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
                                   tune_grid_proportion = 1,

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
      tune_grid_proportion = tune_grid_proportion,

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
    tune_grid_proportion = tune_grid_proportion,

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
