#' @import checkmate

#' @include globals.R
#' @include utils.R
#' @include kernels.R

# Checkmate extension --------------------------------------------------

checkSubsetString <- function(x,
                              choices,
                              empty.ok,
                              unique,
                              ignore.case,
                              len) {
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

  if (unique && any(duplicated(x))) {
    return("Must have unique values")
  }

  if (ignore.case) {
    if (!all(tolower(x) %in% tolower(choices))) {
      return(sprintf(
        "Must be a subset of {%s}, but is {%s}. Note: Case not sensitive",
        set_collapse(choices),
        set_collapse(x)
      ))
    }
  } else {
    if (!all(x %in% choices)) {
      return(sprintf(
        "Must be a subset of {%s}, but is {%s}. Note: Case sensitive",
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
                                 unique=FALSE,
                                 len=NULL,
                                 label=vname(x)) {
  if (missing(x)) {
    stop(sprintf("Argument '%s' is missing", label))
  }
  res <- checkSubsetString(x, choices, empty.ok, unique, ignore.case, len)
  makeAssertion(x, res, label, NULL)
}

checkBounds <- function(x,
                        only_ints = FALSE,
                        lower = -Inf,
                        upper = Inf,
                        null.ok = FALSE,
                        na.ok = FALSE) {
  if (null.ok && is.null(x)) return(TRUE)

  check_function <- checkNumber
  if (only_ints) {
    check_function <- checkInt
  }

  if (is.numeric(x)) {
    result <- check_function(
      x,
      lower = lower,
      upper = upper,
      na.ok = na.ok
    )

    return(result)
  }

  result <- checkList(x, any.missing = FALSE)
  if (!identical(result, TRUE)) return(result)

  result <- checkNames(names(x), must.include = c("min", "max"))
  if (!identical(result, TRUE)) return(result)

  min <- x$min
  max <- x$max
  result <- check_function(
    min,
    lower = lower,
    upper = upper,
    na.ok = na.ok
  )
  if (!identical(result, TRUE)) return(result)

  result <- check_function(
    max,
    lower = lower,
    upper = upper,
    na.ok = na.ok
  )
  if (!identical(result, TRUE)) return(result)

  if (max <= min) {
    return(sprintf("Max must be greater than min, but %s <= %s", max, min))
  }

  return(TRUE)
}

assert_bounds <- function(x,
                          only_ints = FALSE,
                          lower = -Inf,
                          upper = Inf,
                          null.ok = FALSE,
                          na.ok = FALSE,
                          label = vname(x)) {
  if (missing(x)) {
    stop(sprintf("Argument '%s' is missing", label))
  }
  res <- checkBounds(x, only_ints, lower, upper, null.ok, na.ok)
  if (is.character(res)) {
    res <- paste0("Using Bayesian_optimization tune type: ", res)
  }

  makeAssertion(x, res, label, NULL)
}

# Helpers --------------------------------------------------

assert_folds <- function(folds, rows_number) {
  assert_list(folds, any.missing = FALSE, min.len = 1)

  for (i in seq_along(folds)) {
    fold <- folds[[i]]
    assert_list(fold, any.missing = FALSE, min.len = 1)

    assert_numeric(
      fold$training,
      upper = rows_number,
      lower = 1,
      any.missing = FALSE,
      min.len = 1,
      .var.name = sprintf("fold[[%s]]$training", i)
    )

    assert_numeric(
      fold$testing,
      upper = rows_number,
      lower = 1,
      any.missing = FALSE,
      min.len = 1,
      .var.name = sprintf("fold[[%s]]$testing", i)
    )
  }
}

assert_tune_cv <- function(tune_type,
                           tune_cv_type,
                           tune_folds_number,
                           tune_testing_proportion,
                           tune_folds,
                           x_nrows,
                           tune_loss_function,
                           tune_grid_proportion,
                           tune_bayes_samples_number,
                           tune_bayes_iterations_number) {
  assert_subset_string(tune_type, TUNE_TYPES, ignore.case = TRUE, len = 1)
  assert_subset_string(
    tune_loss_function,
    c(
      TUNE_NUMERIC_LOSS_FUNCTIONS,
      TUNE_BINARY_LOSS_FUNCTIONS,
      TUNE_CATEGORICAL_LOSS_FUNCTIONS
    ),
    ignore.case = TRUE,
    len = 1
  )

  assert_subset_string(tune_cv_type, TUNE_CV_TYPES, ignore.case = TRUE, len = 1)
  tune_cv_type <- tolower(tune_cv_type)

  min_folds_number <- if (tune_cv_type == "k_fold") 2 else 1
  assert_int(tune_folds_number, lower = min_folds_number)
  assert_number(tune_testing_proportion, lower = 1e-3, upper = 1 - 1e-3)

  if (tolower(tune_type) == "Bayesian_optimization") {
    assert_int(tune_bayes_samples_number, lower = 1)
    assert_int(tune_bayes_iterations_number, lower = 0)
  } else {
    assert_number(tune_grid_proportion, lower = 1e-3, upper = 1)
  }

  if (!is.null(tune_folds)) {
    assert_folds(tune_folds, x_nrows)
  }
}

assert_base_params <- function(x,
                               y,
                               is_multivariate,
                               expect_x_matrix,
                               tune_type,
                               tune_cv_type,
                               tune_folds_number,
                               tune_testing_proportion,
                               tune_folds,
                               tune_loss_function,
                               tune_grid_proportion,
                               tune_bayes_samples_number,
                               tune_bayes_iterations_number,
                               seed,
                               verbose) {
  assert_xy(
    x,
    y,
    is_multivariate = is_multivariate,
    expect_x_matrix = expect_x_matrix
  )

  assert_tune_cv(
    tune_type = tune_type,
    tune_cv_type = tune_cv_type,
    tune_folds_number = tune_folds_number,
    tune_testing_proportion = tune_testing_proportion,
    tune_folds = tune_folds,
    x_nrows = nrow(x),
    tune_loss_function = tune_loss_function,
    tune_grid_proportion = tune_grid_proportion,
    tune_bayes_samples_number = tune_bayes_samples_number,
    tune_bayes_iterations_number = tune_bayes_iterations_number
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

assert_is_multivariate <- function(is_multivariate) {
  assert_logical(is_multivariate, any.missing = FALSE, len = 1)
}

assert_x <- function(x, expected_matrix = TRUE) {
  if (expected_matrix) {
    assert_matrix(x, min.cols = 1, min.rows = 1, all.missing = FALSE)
    assert_numeric(x)
  } else {
    assert_data_frame(x, min.cols = 1, min.rows = 1, all.missing = FALSE)
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
      " must have the same length"
    )
  }
}

assert_observed_probabilities <- function(observed, probabilities) {
  assert_factor(observed, min.len = 1)
  assert_data_frame(probabilities)

  if (is_empty(observed) || is_empty(probabilities)) {
    stop("observed and/or probabilities is empty.")
  } else if (length(observed) != NROW(probabilities)) {
    stop("observed and probabilities must have the same number of records.")
  } else if (is.null(NCOL(probabilities)) || NCOL(probabilities) < 2) {
    stop("probabilities must have at least two columns (classes).")
  } else if (is.null(colnames(probabilities))) {
    stop("probabilities must have the classes' names as columns names.")
  }

  assert_subset(
    levels(observed),
    colnames(probabilities),
    empty.ok = FALSE,
    .var.name = "observed"
  )
}

assert_categorical_obs_pred <- function(observed, predicted) {
  assert_same_length(observed, predicted)

  assert_factor(observed, min.len = 1)
  assert_factor(predicted, min.len = 1)

  if (!is_empty(setdiff(levels(observed), levels(predicted)))) {
    warning("observed and predicted does not have the same classes (levels).")
  }

  if (length(union(levels(observed), levels(predicted))) == 1) {
    warning("There are only one class (level) in observed and predicted data.")
  }
}

assert_positive_class <- function(positive_class, classes) {
  assert_string(positive_class, null.ok = TRUE, na.ok = FALSE)
  assert_subset(positive_class, classes, empty.ok = TRUE)
}

assert_confusion_matrix <- function(confusion_matrix) {
  if (ncol(confusion_matrix) < 2) {
    stop(
      "There must be at least classes in order to compute the metric. ",
      "Try to set all classes in observed and/or ",
      "predicted factor levels."
    )
  }
}

assert_xy <- function(x, y, is_multivariate, expect_x_matrix) {
  assert_x(x, expect_x_matrix)
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

    assert_x(x_list$x, expected_matrix = TRUE)
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
      assert_int(arc_cosine_deep, lower = 1)
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

assert_svm_scale <- function(scale, x_n_cols) {
  assert_logical(scale, any.missing = FALSE)
  scale_length <- length(scale)

  if (scale_length != 1 && scale_length != x_n_cols) {
    stop(
      "scale must have the same length as x columns (",
      x_n_cols,
      ") or 1 but have length ",
      scale_length
    )
  }
}

assert_svm_class_weights <- function(class_weights) {
  if (is.character(class_weights)) {
    assert_subset_string(
      class_weights,
      SVM_CLASS_WEIGHTS,
      ignore.case = TRUE
    )
  } else {
    assert_numeric(
      class_weights,
      finite = TRUE,
      any.missing = FALSE,
      null.ok = TRUE
    )
  }
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

assert_layers <- function(layers, tune_type) {
  assert_list(layers, min.len = 1)

  for (layer in layers) {
    assert_list(layer)

    if (is_bayesian_tuner(tune_type)) {
      assert_bounds(layer$neurons_number, lower = 1e-10, null.ok = TRUE)

      assert_subset_string(
        layer$activation,
        VALID_ACTIVATION_FUNCTIONS,
        len = 1,
        empty.ok = TRUE,
        ignore.case = TRUE
      )

      assert_bounds(layer$neurons_number, lower = 1e-10, null.ok = TRUE)
      assert_bounds(
        layer$dropout,
        lower = 0,
        upper = 1,
        null.ok = TRUE
      )
      assert_bounds(
        layer$ridge_penalty,
        lower = 0,
        upper = 1,
        null.ok = TRUE
      )
      assert_bounds(
        layer$lasso_penalty,
        lower = 0,
        upper = 1,
        null.ok = TRUE
      )
    } else {
      assert_numeric(
        layer$neurons_number,
        null.ok = TRUE,
        any.missing = FALSE,
        lower = 1e-10
      )

      assert_numeric(
        layer$neurons_proportion,
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
}

assert_optimizer <- function(optimizer) {
  assert_subset_string(
    optimizer,
    VALID_OPTIMIZERS,
    empty.ok = FALSE,
    ignore.case = TRUE,
    len = 1
  )
}

assert_deep_learning_loss_function <- function(loss_function) {
  assert_subset_string(
    loss_function,
    VALID_DEEP_LEARNING_LOSS_FUNCTIONS,
    ignore.case = TRUE,
    len = 1
  )
}

assert_output_penalties <- function(output_penalties, tune_type) {
  assert_list(output_penalties, len = 2, any.missing = FALSE)

  if (is_bayesian_tuner(tune_type)) {
    assert_bounds(output_penalties$ridge_penalty, lower = 0, upper = 1)
    assert_bounds(output_penalties$lasso_penalty, lower = 0, upper = 1)
  } else {
    assert_penalty(output_penalties$ridge_penalty, null.ok = FALSE)
    assert_penalty(output_penalties$lasso_penalty, null.ok = FALSE)
  }
}

assert_pls_method <- function(method) {
  assert_subset_string(
    method,
    PARTIAL_LEAST_SQUARES_METHODS,
    ignore.case = TRUE,
    len = 1
  )
}

assert_cv_kfold <- function(records_number, k) {
  assert_number(records_number, lower = 2, finite = TRUE)
  assert_number(k, lower = 2, upper = records_number)
}

assert_cv_kfold_strata <- function(data, k) {
  assert_vector(data, any.missing = FALSE, min.len = 1)
  assert_number(k, lower = 2, upper = length(data))
}

assert_cv_random <- function(records_number, folds_number, testing_proportion) {
  assert_number(records_number, lower = 2, finite = TRUE)
  assert_number(folds_number, lower = 1, finite = TRUE)
  assert_number(testing_proportion, lower = 1e-3, upper = 1 - 1e-3)
}

assert_cv_random_strata <- function(data, folds_number, testing_proportion) {
  assert_vector(data, any.missing = FALSE, min.len = 1)
  assert_number(folds_number, lower = 1, finite = TRUE)
  assert_number(testing_proportion, lower = 1e-3, upper = 1 - 1e-3)
}

assert_cv_random_line <- function(lines, folds_number, testing_proportion) {
  assert_vector(lines, any.missing = FALSE, min.len = 1)
  assert_number(folds_number, lower = 1, finite = TRUE)
  assert_number(testing_proportion, lower = 1e-3, upper = 1 - 1e-3)
}

assert_cv_one_env_out <- function(envs, envs_proportion, folds_per_env) {
  assert_vector(envs, any.missing = FALSE, min.len = 1)
  assert_number(envs_proportion, lower = 1e-3, upper = 1)
  assert_number(folds_per_env, lower = 1, finite = TRUE)
}

assert_cv_leve_one_group_out <- function(x) {
  assert_vector(x, any.missing = FALSE, min.len = 2)
  if (lunique(x) < 2) {
    stop("x only contains data of one group, minimum two groups are required")
  }
}

assert_cv_na <- function(x) {
  assert_vector(x, min.len = 2, all.missing = FALSE)
  if (all(!is.na(x))) {
    stop("For NA cross validation you have to include some NA observations")
  }
}

assert_predict_format <- function(format) {
  assert_subset_string(
    format,
    PREDICT_FORMAT,
    ignore.case = TRUE,
    len = 1
  )
}

assert_lines <- function(lines) {
  if (!is.factor(lines) && !is.character(lines)) {
    stop("lines must be a factor or character vector")
  }
  lines <- as.character(lines)

  assert_character(as.character(lines), any.missing = FALSE, min.len = 1)
}

assert_envs <- function(envs) {
  if (!is.factor(envs) && !is.character(envs)) {
    stop("envs must be a factor or character vector")
  }
  envs <- as.character(envs)

  assert_character(envs, any.missing = FALSE, min.len = 1)
}

assert_pheno <- function(Pheno, traits, is_multivariate) {
  assert_character(traits, any.missing = FALSE, min.len = 1)

  if (is_multivariate && length(traits) < 2) {
    stop("Multivariate analysis requires at least two traits")
  }

  required_cols <- c("Line", "Env")
  assert_data_frame(Pheno, min.rows = 1)
  assert_names(colnames(Pheno), must.include = required_cols)

  assert_vector(Pheno$Line, any.missing = FALSE, min.len = 1)
  assert_vector(Pheno$Env, any.missing = FALSE, min.len = 1)

  assert_names(traits, subset.of = setdiff(colnames(Pheno), required_cols))
}

assert_geno <- function(Geno, lines) {
  assert_matrix(
    Geno,
    any.missing = FALSE,
    min.rows = length(lines),
    min.cols = length(lines)
  )

  if (!is_square(Geno)) {
    stop("Geno must be a square matrix")
  }

  assert_names(rownames(Geno), permutation.of = lines)
  assert_names(colnames(Geno), permutation.of = lines)
}

assert_geno_markers <- function(Geno, Markers, lines) {
  if (is.null(Geno) & is.null(Markers)) {
    stop("Geno and Markers cannot both be NULL, you must provide one of them")
  } else if (!is.null(Geno) & !is.null(Markers)) {
    stop("Only Geno or Markers must be provided and not both of them")
  }

  unique_lines <- as.character(unique(lines))
  unique_lines_num <- length(unique_lines)

  if (!is.null(Geno)) {
    assert_matrix(
      Geno,
      any.missing = FALSE,
      min.rows = unique_lines_num,
      min.cols = unique_lines_num
    )
    assert_names(rownames(Geno), must.include = unique_lines)
    assert_names(colnames(Geno), must.include = unique_lines)
  } else {
    assert_matrix(
      Markers,
      any.missing = FALSE,
      min.rows = unique_lines_num,
      min.cols = 1
    )
    assert_names(rownames(Markers), must.include = unique_lines)
  }
}

assert_predictors <- function(predictors, is_multivariate) {
  assert_list(predictors, any.missing = FALSE, min.len = 1)

  assert_subset_string(
    names(predictors),
    GS_PREDICTORS,
    ignore.case = TRUE,
    empty.ok = FALSE,
    unique = TRUE
  )

  if (!("line" %in% tolower(names(predictors)))) {
    stop("Line is required always in the predictors list")
  }

  for (model in predictors) {
    assert_bayesian_model(model, is_multivariate)
  }
}

# Single fit functions --------------------------------------------------

validate_support_vector_machine <- function(x,
                                            y,

                                            kernel,
                                            degree,
                                            gamma,
                                            coef0,
                                            cost,

                                            tune_type,
                                            tune_cv_type,
                                            tune_folds_number,
                                            tune_testing_proportion,
                                            tune_folds,
                                            tune_loss_function,
                                            tune_grid_proportion,
                                            tune_bayes_samples_number,
                                            tune_bayes_iterations_number,

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
    expect_x_matrix = TRUE,
    tune_type = tune_type,
    tune_cv_type = tune_cv_type,
    tune_folds_number = tune_folds_number,
    tune_testing_proportion = tune_testing_proportion,
    tune_folds = tune_folds,
    tune_loss_function = tune_loss_function,
    tune_grid_proportion = tune_grid_proportion,
    tune_bayes_samples_number = tune_bayes_samples_number,
    tune_bayes_iterations_number = tune_bayes_iterations_number,
    seed = seed,
    verbose = verbose
  )

  assert_svm_scale(scale, ncol(x))

  assert_svm_kernel(kernel)

  if (is_bayesian_tuner(tune_type)) {
    assert_bounds(degree, only_ints = TRUE)
    assert_bounds(gamma)
    assert_bounds(coef0)
    assert_bounds(cost)
  } else {
    assert_numeric(degree, finite = TRUE, any.missing = FALSE)
    assert_numeric(gamma, finite = TRUE, any.missing = FALSE)
    assert_numeric(coef0, finite = TRUE, any.missing = FALSE)
    assert_numeric(cost, finite = TRUE, any.missing = FALSE)
  }

  assert_svm_class_weights(class_weights)
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

                                   tune_type,
                                   tune_cv_type,
                                   tune_folds_number,
                                   tune_testing_proportion,
                                   tune_folds,
                                   tune_loss_function,
                                   tune_grid_proportion,
                                   tune_bayes_samples_number,
                                   tune_bayes_iterations_number,

                                   split_rule,
                                   splits_number,
                                   x_vars_weights,
                                   records_weights,
                                   na_action,

                                   seed,
                                   verbose) {
  assert_base_params(
    x = x,
    y = y,
    is_multivariate = is_multivariate,
    expect_x_matrix = TRUE,
    tune_type = tune_type,
    tune_cv_type = tune_cv_type,
    tune_folds_number = tune_folds_number,
    tune_testing_proportion = tune_testing_proportion,
    tune_folds = tune_folds,
    tune_loss_function = tune_loss_function,
    tune_grid_proportion = tune_grid_proportion,
    tune_bayes_samples_number = tune_bayes_samples_number,
    tune_bayes_iterations_number = tune_bayes_iterations_number,
    seed = seed,
    verbose = verbose
  )

  if (is_bayesian_tuner(tune_type)) {
    assert_bounds(trees_number, lower = 1, only_ints = TRUE)
    assert_bounds(node_size, lower = 1, only_ints = TRUE)
    assert_bounds(node_depth, lower = 1, only_ints = TRUE, null.ok = TRUE)
    assert_bounds(
      sampled_x_vars_number,
      lower = 1e-3,
      only_ints = FALSE,
      null.ok = TRUE
    )
  } else {
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
  }

  assert_forest_split_rule(split_rule)

  assert_number(splits_number, lower = 0, finite = TRUE)

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

                                              tune_type,
                                              tune_cv_type,
                                              tune_folds_number,
                                              tune_testing_proportion,
                                              tune_folds,
                                              tune_loss_function,
                                              tune_grid_proportion,
                                              tune_bayes_samples_number,
                                              tune_bayes_iterations_number,

                                              lambdas_number,
                                              records_weights,
                                              standardize,
                                              fit_intercept,

                                              seed,
                                              verbose) {
  assert_base_params(
    x = x,
    y = y,
    is_multivariate = is_multivariate,
    expect_x_matrix = TRUE,
    tune_type = tune_type,
    tune_cv_type = tune_cv_type,
    tune_folds_number = tune_folds_number,
    tune_testing_proportion = tune_testing_proportion,
    tune_folds = tune_folds,
    tune_loss_function = tune_loss_function,
    tune_grid_proportion = tune_grid_proportion,
    tune_bayes_samples_number = tune_bayes_samples_number,
    tune_bayes_iterations_number = tune_bayes_iterations_number,
    seed = seed,
    verbose = verbose
  )

  assert_int(tune_folds_number, lower = 3)
  assert_subset_string(tune_cv_type, GLM_CV_TYPES, ignore.case = TRUE, len = 1)

  if (is_bayesian_tuner(tune_type)) {
    assert_bounds(alpha, lower = 0, upper = 1)
  } else {
    assert_numeric(alpha, lower = 0, upper = 1, any.missing = FALSE)
  }

  assert_number(lambdas_number, finite = TRUE, lower = 1)

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

                                                 tune_type,
                                                 tune_cv_type,
                                                 tune_folds_number,
                                                 tune_testing_proportion,
                                                 tune_folds,
                                                 tune_loss_function,
                                                 tune_grid_proportion,
                                                 tune_bayes_samples_number,
                                                 tune_bayes_iterations_number,

                                                 predictors_relationship,

                                                 seed,
                                                 verbose) {
  assert_base_params(
    x = x,
    y = y,
    is_multivariate = FALSE,
    expect_x_matrix = TRUE,
    tune_type = tune_type,
    tune_cv_type = tune_cv_type,
    tune_folds_number = tune_folds_number,
    tune_testing_proportion = tune_testing_proportion,
    tune_folds = tune_folds,
    tune_loss_function = tune_loss_function,
    tune_grid_proportion = tune_grid_proportion,
    tune_bayes_samples_number = tune_bayes_samples_number,
    tune_bayes_iterations_number = tune_bayes_iterations_number,
    seed = seed,
    verbose = verbose
  )

  if (is_bayesian_tuner(tune_type)) {
    assert_bounds(trees_number, lower = 1, only_ints = TRUE)
    assert_bounds(node_size, lower = 1, only_ints = TRUE)
    assert_bounds(max_depth, lower = 1, only_ints = TRUE)
    assert_bounds(
      sampled_records_proportion,
      lower = 1e-3,
      upper = 1,
      only_ints = FALSE
    )
    assert_bounds(shrinkage, only_ints = FALSE)
  } else {
    assert_numeric(trees_number, lower = 1, finite = TRUE, any.missing = FALSE)
    assert_numeric(node_size, lower = 1, finite = TRUE, any.missing = FALSE)
    assert_numeric(max_depth, lower = 1, finite = TRUE, any.missing = FALSE)
    assert_numeric(
      sampled_records_proportion,
      lower = 1e-3,
      upper = 1,
      finite = TRUE,
      any.missing = FALSE
    )
    assert_numeric(shrinkage, finite = TRUE, any.missing = FALSE)
  }

  assert_numeric(
    predictors_relationship,
    lower = -1,
    upper = 1,
    any.missing = FALSE,
    len = ncol(x),
    null.ok = TRUE
  )
}

validate_deep_learning <- function(x,
                                   y,
                                   is_multivariate,

                                   learning_rate,
                                   epochs_number,
                                   batch_size,
                                   layers,
                                   output_penalties,

                                   tune_type,
                                   tune_cv_type,
                                   tune_folds_number,
                                   tune_testing_proportion,
                                   tune_folds,
                                   tune_grid_proportion,
                                   tune_bayes_samples_number,
                                   tune_bayes_iterations_number,

                                   optimizer,
                                   loss_function,
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
    expect_x_matrix = TRUE,
    tune_type = tune_type,
    tune_cv_type = tune_cv_type,
    tune_folds_number = tune_folds_number,
    tune_testing_proportion = tune_testing_proportion,
    tune_folds = tune_folds,
    tune_loss_function = NULL,
    tune_grid_proportion = tune_grid_proportion,
    tune_bayes_samples_number = tune_bayes_samples_number,
    tune_bayes_iterations_number = tune_bayes_iterations_number,
    seed = seed,
    verbose = verbose
  )

  if (is_bayesian_tuner(tune_type)) {
    assert_bounds(learning_rate, lower = 1e-100)
    assert_bounds(epochs_number, lower = 1)
    assert_bounds(batch_size, lower = 1)
  } else {
    assert_numeric(
      learning_rate,
      lower = 1e-100,
      finite = TRUE,
      any.missing = FALSE
    )
    assert_numeric(epochs_number, lower = 1, finite = TRUE, any.missing = FALSE)
    assert_numeric(batch_size, lower = 1, finite = TRUE, any.missing = FALSE)
  }

  assert_layers(layers, tune_type)
  assert_output_penalties(output_penalties, tune_type)

  assert_optimizer(optimizer)
  assert_deep_learning_loss_function(loss_function)
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

validate_partial_least_squares <- function(x,
                                           y,
                                           is_multivariate,
                                           method,
                                           scale,
                                           seed,
                                           verbose) {
  assert_base_params(
    x = x,
    y = y,
    is_multivariate = is_multivariate,
    expect_x_matrix = TRUE,
    tune_type = TUNE_TYPES[1],
    tune_cv_type = TUNE_CV_TYPES[1],
    tune_folds_number = 3,
    tune_testing_proportion = 0.2,
    tune_folds = NULL,
    tune_loss_function = TUNE_NUMERIC_LOSS_FUNCTIONS[1],
    tune_grid_proportion = 0.5,
    tune_bayes_samples_number = 10,
    tune_bayes_iterations_number = 10,
    seed = seed,
    verbose = verbose
  )

  assert_pls_method(method)
  assert_svm_scale(scale, NCOL(x))
}

# Genomic selection --------------------------------------------------

assert_gs_summary <- function(predictions, save_at, digits) {
  assert_data_frame(
    predictions,
    all.missing = FALSE,
    min.rows = 1
  )

  assert_names(
    colnames(predictions),
    must.include = c("Observed", "Predicted", "Env", "Line", "Fold"),
    what = "predictions columns' names"
  )

  observed <- predictions$Observed
  if (!is.numeric(observed) && !is.factor(observed)) {
    stop("Both the observed and predicted columns must be numeric or factor.")
  }

  if (
    is.factor(observed) &&
    !all(levels(observed) %in% colnames(predictions))
  ) {
    stop(
      "For categorical variables in Observed, predictions must have a column ",
      "for each class (level) with the predicted probability."
    )
  }

  assert_string(save_at, min.chars = 1, null.ok = TRUE)
  assert_number(digits, lower = 0, finite = TRUE)
}

validate_gs_radial <- function(is_multivariate,
                               lines,
                               envs,
                               y,
                               Geno,
                               Markers,
                               predictors,

                               rho,
                               iterations_number,
                               burn_in,
                               thinning,
                               testing_indices,

                               tune_type,
                               tune_cv_type,
                               tune_folds_number,
                               tune_testing_proportion,
                               tune_folds,
                               tune_loss_function,
                               tune_grid_proportion,
                               tune_bayes_samples_number,
                               tune_bayes_iterations_number,

                               seed,
                               verbose) {
  assert_lines(lines)
  assert_envs(envs)
  assert_same_length(lines, envs)
  assert_y(y, is_multivariate)
  assert_same_length(lines, y)
  assert_geno_markers(Geno, Markers, lines)
  assert_predictors(predictors, is_multivariate)

  assert_tune_cv(
    tune_type = tune_type,
    tune_cv_type = tune_cv_type,
    tune_folds_number = tune_folds_number,
    tune_testing_proportion = tune_testing_proportion,
    tune_folds = tune_folds,
    x_nrows = length(lines),
    tune_loss_function = tune_loss_function,
    tune_grid_proportion = tune_grid_proportion,
    tune_bayes_samples_number = tune_bayes_samples_number,
    tune_bayes_iterations_number = tune_bayes_iterations_number
  )

  if (is_bayesian_tuner(tune_type)) {
    assert_bounds(rho)
  } else {
    assert_numeric(rho, finite = TRUE, any.missing = FALSE)
  }

  assert_numeric(
    testing_indices,
    lower = 1,
    upper = length(lines),
    null.ok = TRUE,
    any.missing = FALSE,
    unique = TRUE
  )
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

  assert_seed(seed)
  assert_verbose(verbose)
}

validate_gs_fast_bayesian <- function(Pheno,
                                      Geno,
                                      traits,

                                      is_multivariate,
                                      predictors,
                                      folds,

                                      seed,
                                      verbose) {
  assert_is_multivariate(is_multivariate)
  assert_pheno(Pheno, traits, is_multivariate)
  assert_geno(Geno, unique(Pheno$Line))
  assert_predictors(predictors)
  assert_folds(folds, nrow(Pheno))

  assert_seed(seed)
  assert_verbose(verbose)
}
