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
                                 label=vname(x)) {
  if (missing(x)) {
    stop(sprintf("Argument '%s' is missing", label))
  }
  res <- checkSubsetString(x, choices, empty.ok, ignore.case, len)
  makeAssertion(x, res, label, NULL)
}

# Helpers --------------------------------------------------

assert_tune_cv <- function(tune_cv_type,
                           tune_folds_number,
                           tune_testing_proportion,
                           tune_grid_proportion) {
  tune_cv_type <- tolower(tune_cv_type)
  assert_subset_string(tune_cv_type, TUNE_CV_TYPES, ignore.case = TRUE, len = 1)

  min_folds_number <- if (tune_cv_type == "k_fold") 2 else 1
  assert_number(tune_folds_number, finite = TRUE, lower = min_folds_number)

  assert_number(tune_testing_proportion, lower = 1e-3, upper = 1 - 1e-3)
  assert_number(tune_grid_proportion, lower = 1e-3, upper = 1)
}

assert_base_params <- function(x,
                               y,
                               is_multivariate,
                               tune_cv_type,
                               tune_folds_number,
                               tune_testing_proportion,
                               tune_grid_proportion,
                               seed,
                               verbose) {
  assert_xy(x, y, is_multivariate = is_multivariate)

  assert_tune_cv(
    tune_cv_type = tune_cv_type,
    tune_folds_number = tune_folds_number,
    tune_testing_proportion = tune_testing_proportion,
    tune_grid_proportion = tune_grid_proportion
  )

  assert_seed(seed)
  assert_verbose(verbose)
}

assert_seed <- function(seed) {
  assert_number(seed, null.ok = TRUE, na.ok = FALSE, finite = TRUE)
}

assert_verbose <- function(verbose) {
  assert_logical(verbose, any.missing = FALSE, len = 1)
}

assert_x <- function(x) {
  if (!is.vector(x) && !is.data.frame(x) && !is.matrix(x)) {
    stop("x must be data.frame, a matrix or a vector")
  } else if (is_empty(x)) {
    stop("x must not be empty")
  }
}

assert_y <- function(y, is_multivariate) {
  if (!is.vector(y) && !is.factor(y) && !is.matrix(y) && !is.data.frame(y)) {
    stop("y must be a data.frame, a matrix or a vector")
  } else if (is_empty(y)) {
    stop("y must not be empty")
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
}

assert_same_length <- function(x, y, x_label = vname(x), y_label = vname(y)) {
  if (get_length(x) != get_length(y)) {
    stop(
      x_label,
      " and ",
      y_label,
      " must have the same number of observations"
    )
  }
}

assert_xy <- function(x, y, is_multivariate) {
  assert_x(x)
  assert_y(y, is_multivariate)

  assert_same_length(x, y)
}

assert_bayesian_model <- function(model, is_multivariate) {
  valid_models <- BAYESIAN_MODELS
  if (is_multivariate) {
    valid_models <- MULTIVARIATE_BAYESIAN_MODELS
  }

  assert_subset_string(
    model,
    valid_models,
    ignore.case = TRUE,
    empty.ok = TRUE,
    len = 1
  )
}

assert_bayesian_x <- function(x, y, is_multivariate) {
  assert_list(x, min.len = 1, any.missing = FALSE)

  for (x_list in x) {
    assert_list(x_list, any.missing = FALSE, min.len = 1)

    assert_x(x_list$x)
    assert_same_length(x_list$x, y)
    assert_bayesian_model(
      model = x_list$model,
      is_multivariate = is_multivariate
    )
  }
}

assert_bayesian_xy <- function(x, y, is_multivariate) {
  assert_y(y, is_multivariate)
  assert_bayesian_x(x = x, y = y, is_multivariate = is_multivariate)
}

assert_covariance_structure <- function(covariance_structure,
                                        responses_number) {
  assert_list(covariance_structure, len = 3)
  assert_subset_string(
    covariance_structure$type,
    BAYESIAN_COVARIANCE_STRUCTURE_TYPES,
    ignore.case = TRUE,
    empty.ok = FALSE,
    len = 1
  )

  assert_number(covariance_structure$df0, lower = 0, finite = TRUE)

  assert_matrix(
    covariance_structure$S0,
    nrows = responses_number,
    ncols = responses_number,
    any.missing = FALSE,
    null.ok = TRUE
  )
}

assert_sparse_kernel <- function(kernel,
                                 arc_cosine_deep,
                                 rows_proportion,
                                 degree,
                                 gamma,
                                 coef0,
                                 params_length = NULL) {
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

    assert_numeric(
      degree,
      finite = TRUE,
      any.missing = FALSE,
      len = params_length
    )
    assert_numeric(
      gamma,
      finite = TRUE,
      any.missing = FALSE,
      len = params_length
    )
    assert_numeric(
      coef0,
      finite = TRUE,
      any.missing = FALSE,
      len = params_length
    )
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

assert_random_forest_na_action <- function(na_action) {
  assert_string(na_action)

  assert_subset_string(
    na_action,
    RANDOM_FOREST_NA_ACTIONS,
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

assert_penalty <- function(penalty, null.ok = TRUE) {
  assert_numeric(
    penalty,
    lower = 0,
    upper = 1,
    null.ok = null.ok,
    any.missing = FALSE
  )
}

assert_layers <- function(layers) {
  assert_list(layers, min.len = 1)

  for (layer in layers) {
    assert_list(layer)

    assert_numeric(
      layer$neurons_number,
      null.ok = TRUE,
      any.missing = FALSE,
      lower = 1e-10
    )

    assert_subset_string(
      layer$activation,
      VALID_ACTIVATION_FUNCTIONS,
      empty.ok = TRUE,
      ignore.case = TRUE
    )

    assert_penalty(layer$dropout)

    assert_penalty(layer$ridge_penalty)
    assert_penalty(layer$lasso_penalty)
  }
}

assert_output_penalties <- function(output_penalties) {
  assert_list(output_penalties, len = 2, any.missing = FALSE)

  assert_penalty(output_penalties$ridge_penalty, null.ok = FALSE)
  assert_penalty(output_penalties$lasso_penalty, null.ok = FALSE)
}

assert_cv_kfold <- function(records_number, k) {
  assert_number(records_number, lower = 2, finite = TRUE)
  assert_number(k, lower = 2, upper = records_number)
}

assert_cv_random <- function(records_number, folds_number, testing_proportion) {
  assert_number(records_number, lower = 2, finite = TRUE)
  assert_number(folds_number, lower = 1, finite = TRUE)
  assert_number(testing_proportion, lower = 1e-3, upper = 1 - 1e-3)
}

# Single fit functions --------------------------------------------------

validate_support_vector_machine <- function(x,
                                            y,

                                            kernel,
                                            degree,
                                            gamma,
                                            coef0,
                                            cost,

                                            tune_cv_type,
                                            tune_folds_number,
                                            tune_testing_proportion,
                                            tune_grid_proportion,

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
  assert_base_params(
    x = x,
    y = y,
    is_multivariate = FALSE,
    tune_cv_type = tune_cv_type,
    tune_folds_number = tune_folds_number,
    tune_testing_proportion = tune_testing_proportion,
    tune_grid_proportion = tune_grid_proportion,
    seed = seed,
    verbose = verbose
  )

  assert_logical(scale, any.missing = FALSE, max.len = ncol(x))

  assert_svm_kernel(kernel)
  assert_numeric(degree, finite = TRUE, any.missing = FALSE)
  assert_numeric(gamma, finite = TRUE, any.missing = FALSE)
  assert_numeric(coef0, finite = TRUE, any.missing = FALSE)
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

validate_random_forest <- function(x,
                                   y,
                                   is_multivariate,

                                   trees_number,
                                   node_size,
                                   node_depth,
                                   sampled_x_vars_number,

                                   tune_cv_type,
                                   tune_folds_number,
                                   tune_testing_proportion,
                                   tune_grid_proportion,

                                   split_rule,
                                   splits_number,
                                   importance,
                                   x_vars_weights,
                                   records_weights,
                                   na_action,

                                   seed,
                                   verbose) {
  assert_base_params(
    x = x,
    y = y,
    is_multivariate = is_multivariate,
    tune_cv_type = tune_cv_type,
    tune_folds_number = tune_folds_number,
    tune_testing_proportion = tune_testing_proportion,
    tune_grid_proportion = tune_grid_proportion,
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
    finite = TRUE
  )

  assert_numeric(
    records_weights,
    len = nrow(x),
    null.ok = TRUE,
    finite = TRUE
  )

  assert_random_forest_na_action(na_action)
}

validate_generalized_linear_model <- function(x,
                                              y,
                                              is_multivariate,

                                              alpha,
                                              lambda,

                                              tune_cv_type,
                                              tune_folds_number,
                                              tune_testing_proportion,
                                              tune_grid_proportion,

                                              lambdas_number,
                                              lambda_min_ratio,
                                              records_weights,
                                              standardize,
                                              fit_intercept,

                                              seed,
                                              verbose) {
  assert_base_params(
    x = x,
    y = y,
    is_multivariate = is_multivariate,
    tune_cv_type = tune_cv_type,
    tune_folds_number = tune_folds_number,
    tune_testing_proportion = tune_testing_proportion,
    tune_grid_proportion = tune_grid_proportion,
    seed = seed,
    verbose = verbose
  )

  assert_numeric(alpha, lower = 0, upper = 1, any.missing = FALSE)

  assert_numeric(lambda, finite = TRUE, any.missing = FALSE, null.ok = TRUE)
  assert_number(lambdas_number, finite = TRUE, lower = 1)
  assert_number(lambda_min_ratio, finite = TRUE, lower = 0)

  assert_numeric(records_weights, len = nrow(x), null.ok = TRUE, finite = TRUE)

  assert_logical(standardize, any.missing = FALSE, len = 1)
  assert_logical(fit_intercept, any.missing = FALSE, len = 1)
}

validate_generalized_boosted_machine <- function(x,
                                                 y,

                                                 trees_number,
                                                 max_depth,
                                                 node_size,
                                                 shrinkage,
                                                 sampled_records_proportion,

                                                 tune_cv_type,
                                                 tune_folds_number,
                                                 tune_testing_proportion,
                                                 tune_grid_proportion,

                                                 records_weights,
                                                 predictors_relationship,
                                                 cores_number,

                                                 seed,
                                                 verbose) {
  assert_base_params(
    x = x,
    y = y,
    is_multivariate = FALSE,
    tune_cv_type = tune_cv_type,
    tune_folds_number = tune_folds_number,
    tune_testing_proportion = tune_testing_proportion,
    tune_grid_proportion = tune_grid_proportion,
    seed = seed,
    verbose = verbose
  )

  assert_numeric(trees_number, lower = 1, finite = TRUE, any.missing = FALSE)
  assert_numeric(node_size, lower = 1, finite = TRUE, any.missing = FALSE)
  assert_numeric(
    max_depth,
    lower = 1,
    finite = TRUE,
    any.missing = FALSE,
    null.ok = TRUE
  )
  assert_numeric(
    sampled_records_proportion,
    lower = 1e-3,
    upper = 1,
    finite = TRUE,
    any.missing = FALSE
  )
  assert_numeric(shrinkage, finite = TRUE, any.missing = FALSE)

  assert_numeric(
    records_weights,
    finite = TRUE,
    any.missing = FALSE,
    len = nrow(x),
    null.ok = TRUE
  )

  assert_numeric(
    predictors_relationship,
    lower = -1,
    upper = 1,
    any.missing = FALSE,
    len = ncol(x),
    null.ok = TRUE
  )

  assert_number(cores_number, lower = 1, finite = TRUE, null.ok = TRUE)
}

validate_deep_learning <- function(x,
                                   y,
                                   is_multivariate,

                                   learning_rate,
                                   epochs_number,
                                   batch_size,
                                   layers,
                                   output_penalties,

                                   tune_cv_type,
                                   tune_folds_number,
                                   tune_testing_proportion,
                                   tune_grid_proportion,

                                   with_platt_scaling,
                                   platt_proportion,
                                   shuffle,
                                   early_stop,
                                   early_stop_patience,

                                   seed,
                                   verbose) {
  assert_base_params(
    x = x,
    y = y,
    is_multivariate = is_multivariate,
    tune_cv_type = tune_cv_type,
    tune_folds_number = tune_folds_number,
    tune_testing_proportion = tune_testing_proportion,
    tune_grid_proportion = tune_grid_proportion,
    seed = seed,
    verbose = verbose
  )

  assert_numeric(learning_rate, finite = TRUE, any.missing = FALSE)
  assert_numeric(epochs_number, lower = 1, finite = TRUE, any.missing = FALSE)
  assert_numeric(batch_size, lower = 1, finite = TRUE, any.missing = FALSE)

  assert_layers(layers)
  assert_output_penalties(output_penalties)

  assert_logical(with_platt_scaling, len = 1, any.missing = FALSE)
  assert_number(platt_proportion, lower = 1e-3, upper = 1 - 1e-3)

  assert_logical(shuffle, len = 1, any.missing = FALSE)
  assert_logical(early_stop, len = 1, any.missing = FALSE)
  assert_int(early_stop_patience, lower = 1)
}

validate_bayesian_model <- function(x,
                                    y,
                                    is_multivariate,

                                    iterations_number,
                                    burn_in,
                                    thinning,
                                    covariance_structure,
                                    records_weights,
                                    response_groups,
                                    testing_indices,

                                    seed,
                                    verbose) {
  assert_bayesian_xy(x = x, y = y, is_multivariate = is_multivariate)
  assert_seed(seed)
  assert_verbose(verbose)

  assert_numeric(
    iterations_number,
    lower = 1,
    finite = TRUE,
    any.missing = FALSE
  )
  assert_numeric(
    burn_in,
    lower = 1,
    finite = TRUE,
    any.missing = FALSE
  )
  assert_numeric(
    thinning,
    lower = 1,
    finite = TRUE,
    any.missing = FALSE
  )

  if (is_multivariate) {
    assert_covariance_structure(covariance_structure, ncol(y))
  } else {
    assert_numeric(
      records_weights,
      len = get_length(y),
      null.ok = TRUE,
      finite = TRUE
    )

    assert_vector(
      as.vector(response_groups),
      len = get_length(y),
      null.ok = TRUE
    )
  }

  assert_numeric(
    testing_indices,
    lower = 1,
    upper = get_length(y),
    null.ok = TRUE,
    any.missing = FALSE,
    unique = TRUE
  )
}
