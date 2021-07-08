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
  makeAssertation(x, res, info, label)
}

# Helpers --------------------------------------------------

validate_xy <- function(x, y, accept_multivariate) {
  if (!is.vector(x) && !is.data.frame(x) && !is.matrix(x)) {
    stop("x must be data.frame, a matrix or a vector")
  }

  if (!is.vector(y) && !is.matrix(y) && !is.data.frame(y)) {
    stop("y must be a data.frame, a matrix or a vector")
  }

  if (accept_multivariate && (!is.data.frame(y) && !is.matrix(y))) {
    stop("y must be a data.frame or a matrix in multivariate models")
  }

  if (get_length(x) != get_length(y)) {
    stop("x and y must have the same number of observations")
  }
}

assert_sparse_kernel <- function(kernel, arc_cosine_deep, rows_proportion) {
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
                            sparse_kernel,
                            rows_proportion,
                            arc_cosine_deep,
                            scale,
                            kernel,
                            degree,
                            gamma,
                            coef0,
                            cost,
                            class_weights,
                            cachesize,
                            tolerance,
                            epsilon,
                            shrinking,
                            cross,
                            probability,
                            fitted,
                            na_action) {
  validate_xy(x, y, accept_multivariate = FALSE)
  assert_sparse_kernel(sparse_kernel, arc_cosine_deep, rows_proportion)

  assert_logical(scale, any.missing = FALSE)

  assert_svm_kernel(kernel)
  assert_number(degree, finite = TRUE)
  assert_number(gamma, finite = TRUE)
  assert_number(coef0, finite = TRUE)
  assert_number(cost, finite = TRUE)

  assert_numeric(
    class_weights,
    finite = TRUE,
    any.missing = FALSE,
    null.ok = TRUE
  )
  assert_number(cache_size, finite = TRUE)
  assert_number(tolerance, finite = TRUE)
  assert_logical(shrinking, len = 1, any.missing = FALSE)
  assert_number(cross, finite = TRUE, lower = 0)
  assert_logical(probability, len = 1, any.missing = FALSE)
  assert_logical(fitted, len = 1, any.missing = FALSE)
  assert_function(na_action, nargs = 1)
}