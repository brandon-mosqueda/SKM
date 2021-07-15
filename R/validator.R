#' @import checkmate

#' @include globals.R
#' @include utils.R
#' @include kernels.R

# Checkmate extension --------------------------------------------------

checkSubsetString <- function(x, choices, empty.ok, ignore.case, len) {
  if (length(x) == 0L) {
    if (!empty.ok) {
      return(sprintf(
        "Must be a subset of {%s}, not empty",
        set_collapse(choices)
      ))
    }

    return(TRUE)
  } else if (!is.null(len) && length(x) != len) {
    return(sprintf(
      "Must have lenght %s, but has length %s",
      len,
      length(x)
    ))
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
                                 len=NULL,
                                 info=NULL,
                                 label=vname(x)) {
  if (missing(x)) {
    stop(sprintf("Argument '%s' is missing", label))
  }
  res <- checkSubsetString(x, choices, empty.ok, ignore.case, len)
  makeAssertion(x, res, info, label)
}

# Helpers --------------------------------------------------

assert_valid_tune_cv <- function(tune_cv_type,
                                 tune_folds_number,
                                 tune_testing_proportion) {
  tune_cv_type <- tolower(tune_cv_type)
  assert_subset_string(tune_cv_type, TUNE_CV_TYPES, ignore.case = TRUE, len = 1)
  min_folds_number <- if (tune_cv_type == "k_fold") 2 else 1
  assert_number(tune_folds_number, finite = TRUE, lower = min_folds_number)

  if (tune_cv_type == "random") {
    assert_number(tune_testing_proportion, lower = 1e-3, upper = 1 - 1e-3)
  }
}

validate_base_params <- function(x,
                                 y,
                                 is_multivariate,
                                 kernel,
                                 arc_cosine_deep,
                                 rows_proportion,
                                 degree,
                                 gamma,
                                 coef0,
                                 tune_cv_type,
                                 tune_folds_number,
                                 tune_testing_proportion,
                                 seed,
                                 verbose) {
  validate_xy(x, y, is_multivariate = is_multivariate)

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

  assert_number(seed, null.ok = TRUE, na.ok = FALSE, finite = TRUE)
  assert_logical(verbose, any.missing = FALSE, len = 1)
}

validate_xy <- function(x, y, is_multivariate) {
  if (!is.vector(x) && !is.data.frame(x) && !is.matrix(x)) {
    stop("x must be data.frame, a matrix or a vector")
  }

  if (!is.vector(y) && !is.factor(y) && !is.matrix(y) && !is.data.frame(y)) {
    stop("y must be a data.frame, a matrix or a vector")
  }

  if (is_multivariate) {
    if (!is.data.frame(y) && !is.matrix(y)) {
      stop("y must be a data.frame or a matrix in multivariate models")
    } else if (ncol(y) < 2) {
      stop("y must have at least two columns in multivariate models")
    }
  } else {
    if (has_dims(y) && ncol(y) > 1) {
      stop(
        "In univariate models y can be a data.frame or a matrix but ",
        "it must have only one column"
      )
    }
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
      ignore.case = TRUE,
      len = 1
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
    ignore.case = TRUE,
    len = 1
  )
}

assert_forest_split_rule <- function(split_rule) {
  assert_subset_string(
    split_rule,
    RANDOM_FOREST_SPLIT_RULES,
    empty.ok = TRUE,
    ignore.case = TRUE,
    len = 1
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

                            seed,
                            verbose) {
  validate_base_params(
    x = x,
    y = y,
    is_multivariate = FALSE,
    kernel = kernel,
    arc_cosine_deep = arc_cosine_deep,
    rows_proportion = rows_proportion,
    degree = degree,
    gamma = gamma,
    coef0 = coef0,
    tune_cv_type = tune_cv_type,
    tune_folds_number = tune_folds_number,
    tune_testing_proportion = tune_testing_proportion,
    seed = seed,
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

validate_sk_random_forest <- function(x,
                                      y,
                                      is_multivariate,

                                      kernel,
                                      degree,
                                      gamma,
                                      coef0,
                                      rows_proportion,
                                      arc_cosine_deep,

                                      trees_number,
                                      node_size,
                                      node_depth,
                                      sampled_x_vars_number,

                                      tune_cv_type,
                                      tune_folds_number,
                                      tune_testing_proportion,

                                      split_rule,
                                      splits_number,
                                      importance,
                                      x_vars_weights,
                                      records_weights,

                                      seed,
                                      verbose) {
  validate_base_params(
    x = x,
    y = y,
    is_multivariate = is_multivariate,
    kernel = kernel,
    arc_cosine_deep = arc_cosine_deep,
    rows_proportion = rows_proportion,
    degree = degree,
    gamma = gamma,
    coef0 = coef0,
    tune_cv_type = tune_cv_type,
    tune_folds_number = tune_folds_number,
    tune_testing_proportion = tune_testing_proportion,
    seed = seed,
    verbose = verbose
  )

  assert_numeric(trees_number, lower = 1, finite = TRUE, any.missing = FALSE)
  assert_numeric(node_size, lower = 1, finite = TRUE, any.missing = FALSE)
  assert_numeric(
    node_depth,
    lower = 1,
    finite = TRUE,
    any.missing = FALSE,
    null.ok = TRUE
  )
  assert_numeric(
    sampled_x_vars_number,
    lower = 1e-3,
    finite = TRUE,
    any.missing = FALSE,
    null.ok = TRUE
  )

  assert_forest_split_rule(split_rule)

  assert_number(splits_number, lower = 0, finite = TRUE)

  assert_logical(importance, len = 1, any.missing = FALSE)

  assert_numeric(
    x_vars_weights,
    len = ncol(x),
    null.ok = TRUE,
    finite = TRUE,
    lower = 0
  )

  assert_numeric(
    records_weights,
    len = nrow(x),
    null.ok = TRUE,
    finite = TRUE,
    lower = 0
  )
}
