#' @importFrom e1071 svm

#' @include model.R
#' @include validator.R

sk_svm <- function(x, y,
                   kernel = NULL,
                   degree = 3,
                   gamma = 1 / ncol(x),
                   coef0 = 0,
                   rows_proportion = 1,
                   arc_cosine_deep = 1,

                   svm_kernel = "linear",
                   svm_degree = 3,
                   svm_gamma = if (is.vector(x)) 1 else 1 / ncol(x),
                   svm_coef0 = 0,
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
                   verbose = TRUE) {
  expect_logical(validate_params, len = 1, any.missing = FALSE)

  if (validate_params) {
    validate_sk_svm(
      x = x,
      y = y,

      kernel = kernel,
      degree = degree,
      gamma = gamma,
      coef0 = coef0,
      rows_proportion = rows_proportion,
      arc_cosine_deep = arc_cosine_deep,

      svm_kernel = svm_kernel,
      svm_degree = svm_degree,
      svm_gamma = svm_gamma,
      svm_coef0 = svm_coef0,
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
      verbose = verbose
    )
  }

  model <- SVMModel$new(
    x = x,
    y = y,

    kernel = kernel,
    degree = degree,
    gamma = gamma,
    coef0 = coef0,
    rows_proportion = rows_proportion,
    arc_cosine_deep = arc_cosine_deep,

    svm_kernel = svm_kernel,
    svm_degree = svm_degree,
    svm_gamma = svm_gamma,
    svm_coef0 = svm_coef0,
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
    verbose = verbose
  )

  model$fit()

  return(model)
}
