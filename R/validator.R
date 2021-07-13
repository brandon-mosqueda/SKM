#' @import checkmate

#' @include globals.R
#' @include utils.R
#' @include kernels.R

# Checkmate extension --------------------------------------------------

checkSubsetString <- function(x, choices, empty.ok, ignore.case) {
  if (length(x) == 0L) {
    if (!empty.ok) {
      return(sprintf(
        "Must be a subset of {%s}, not empty",
        set_collapse(choices)
      ))
    }

    return(TRUE)
  }

  if (length(choices) == 0L) {
    if (length(x) == 0L) {
      return(TRUE)
    }

    return("Must be a subset of the empty set, i.e. also empty")
  }

  if (!is.character(x)) {
    return(paste0("Must be a character, not ", typeof(x)))
  }

  if (ignore.case) {
    if (!all(tolower(x) %in% tolower(choices))) {
      return(sprintf(
        "Must be a subset of {%s}, but is {%s}. Note: Case not sensitive.",
        set_collapse(choices),
        set_collapse(x)
      ))
    }
  } else {
    if (!all(x %in% choices)) {
      return(sprintf(
        "Must be a subset of {%s}, but is {%s}. Note: Case sensitive.",
        set_collapse(choices),
        set_collapse(x)
      ))
    }
  }

  return(TRUE)
}

assert_subset_string <- function(x,
                                 choices,
                                 ignore.case=FALSE,
                                 empty.ok=TRUE,
                                 info=NULL,
                                 label=vname(x)) {
  if (missing(x)) {
    stop(sprintf("Argument '%s' is missing", label))
  }
  res <- checkSubsetString(x, choices, empty.ok, ignore.case)
  makeAssertion(x, res, info, label)
}

# Helpers --------------------------------------------------

assert_valid_tune_cv <- function(tune_cv_type,
                                 tune_folds_number,
                                 tune_testing_proportion) {
  tune_cv_type <- tolower(tune_cv_type)
  assert_subset_string(tune_cv_type, TUNE_CV_TYPES, ignore.case = TRUE)
  min_folds_number <- if (tune_cv_type == "k_fold") 2 else 1
  assert_number(tune_folds_number, finite = TRUE, lower = min_folds_number)

  if (tune_cv_type == "random") {
    assert_number(tune_testing_proportion, lower = 1e-3, upper = 1 - 1e-3)
  }
}

validate_base_params <- function(x,
                                 y,
                                 accept_multivariate,
                                 kernel,
                                 arc_cosine_deep,
                                 rows_proportion,
                                 degree,
                                 gamma,
                                 coef0,
                                 tune_cv_type,
                                 tune_folds_number,
                                 tune_testing_proportion,
                                 verbose) {
  validate_xy(x, y, accept_multivariate = accept_multivariate)

  assert_sparse_kernel(
    kernel = kernel,
    arc_cosine_deep = arc_cosine_deep,
    rows_proportion = rows_proportion,
    degree = degree,
    gamma = gamma,
    coef0 = coef0
  )

  assert_valid_tune_cv(
    tune_cv_type = tune_cv_type,
    tune_folds_number = tune_folds_number,
    tune_testing_proportion = tune_testing_proportion
  )

  assert_logical(verbose, any.missing = FALSE, len = 1)
}

validate_xy <- function(x, y, accept_multivariate) {
  if (!is.vector(x) && !is.data.frame(x) && !is.matrix(x)) {
    stop("x must be data.frame, a matrix or a vector")
  }

  if (!is.vector(y) && !is.factor(y) && !is.matrix(y) && !is.data.frame(y)) {
    stop("y must be a data.frame, a matrix or a vector")
  }

  if (accept_multivariate && (!is.data.frame(y) && !is.matrix(y))) {
    stop("y must be a data.frame or a matrix in multivariate models")
  }

  if (get_length(x) != get_length(y)) {
    stop("x and y must have the same number of observations")
  }
}

assert_sparse_kernel <- function(kernel,
                                 arc_cosine_deep,
                                 rows_proportion,
                                 degree,
                                 gamma,
                                 coef0) {
  if (!is.null(kernel)) {
    assert_string(kernel)
    assert_subset_string(
      kernel,
      c(SPARSE_KERNELS, CONVENTIONAL_KERNELS),
      empty.ok = FALSE,
      ignore.case = TRUE
    )

    if (is_arc_cosine_kernel(kernel)) {
      assert_int(arc_cosine_deep, lower = 2)
    }

    assert_number(rows_proportion, lower = 0.001, upper = 1)

    assert_numeric(degree, finite = TRUE, any.missing = FALSE)
    assert_numeric(gamma, finite = TRUE, any.missing = FALSE)
    assert_numeric(coef0, finite = TRUE, any.missing = FALSE)
  }
}

assert_svm_kernel <- function(kernel) {
  assert_string(kernel)

  assert_subset_string(
    kernel,
    SVM_KERNELS,
    empty.ok = FALSE,
    ignore.case = TRUE
  )
}

# Single fit functions --------------------------------------------------

validate_sk_svm <- function(x,
                            y,

                            kernel,
                            degree,
                            gamma,
                            coef0,
                            rows_proportion,
                            arc_cosine_deep,

                            svm_kernel,
                            svm_degree,
                            svm_gamma,
                            svm_coef0,
                            cost,

                            tune_cv_type,
                            tune_folds_number,
                            tune_testing_proportion,

                            scale,
                            class_weights,
                            cache_size,
                            tolerance,
                            epsilon,
                            shrinking,
                            cross,
                            probability,
                            fitted,
                            na_action,
                            verbose) {
  validate_base_params(
    x = x,
    y = y,
    accept_multivariate = FALSE,
    kernel = kernel,
    arc_cosine_deep = arc_cosine_deep,
    rows_proportion = rows_proportion,
    degree = degree,
    gamma = gamma,
    coef0 = coef0,
    tune_cv_type = tune_cv_type,
    tune_folds_number = tune_folds_number,
    tune_testing_proportion = tune_testing_proportion,
    verbose = verbose
  )

  assert_logical(scale, any.missing = FALSE)

  assert_svm_kernel(svm_kernel)
  assert_numeric(svm_degree, finite = TRUE, any.missing = FALSE)
  assert_numeric(svm_gamma, finite = TRUE, any.missing = FALSE)
  assert_numeric(svm_coef0, finite = TRUE, any.missing = FALSE)
  assert_numeric(cost, finite = TRUE, any.missing = FALSE)

  assert_numeric(
    class_weights,
    finite = TRUE,
    any.missing = FALSE,
    null.ok = TRUE
  )
  assert_number(cache_size, finite = TRUE)
  assert_number(tolerance, finite = TRUE)
  assert_logical(shrinking, len = 1, any.missing = FALSE)
  assert_logical(fitted, len = 1, any.missing = FALSE)
}
