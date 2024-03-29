expect_difftime <- function(x) {
  expect_class(x, "difftime")
  expect_number(as.numeric(x), lower = 0, finite = TRUE)
}

expect_model <- function(model,
                         x,
                         y,
                         hyperparams,
                         responses,
                         tune_grid_proportion,
                         class_name,
                         fitted_class,
                         removed_rows,
                         removed_x_cols,
                         allow_coefficients,
                         is_multivariate,
                         by_category,
                         has_all_row,
                         is_x_matrix = FALSE,
                         has_intercept = FALSE,
                         with_platt_scaling = FALSE) {
  expect_base_params(
    model = model,
    class_name = class_name,
    allow_coefficients = allow_coefficients,
    is_multivariate = is_multivariate,
    fitted_class = fitted_class,
    removed_rows = removed_rows,
    removed_x_cols = removed_x_cols
  )

  expect_y(model = model, y = y, removed_rows = removed_rows)

  expect_x(
    model = model,
    x = x,
    removed_rows = removed_rows,
    removed_x_cols = removed_x_cols,
    is_x_matrix = is_x_matrix
  )

  expect_hyperparams(
    model = model,
    hyperparams = hyperparams,
    tune_grid_proportion = tune_grid_proportion
  )

  expect_responses(model, responses)

  x_testing <- x[sample(nrow(x), nrow(x) * 0.5), ]
  expect_predictions(
    model = model,
    x = x_testing,
    responses = responses,
    is_multivariate = is_multivariate,
    with_platt_scaling = with_platt_scaling
  )

  if (allow_coefficients) {
    expected_names <- colnames(x)
    if (!is_empty(removed_x_cols)) {
      expected_names <- expected_names[-removed_x_cols]
    }
    expect_coefs(
      model = model,
      expected_names = expected_names,
      responses = responses,
      is_multivariate = is_multivariate,
      by_category = by_category,
      has_all_row = has_all_row,
      has_intercept = has_intercept
    )
  }
}

expect_base_params <- function(model,
                               class_name,
                               allow_coefficients,
                               is_multivariate,
                               fitted_class,
                               removed_rows,
                               removed_x_cols) {
  expect_class(model, class_name)
  expect_difftime(model$execution_time)
  expect_identical(model$allow_coefficients, allow_coefficients)
  expect_identical(model$is_multivariate, is_multivariate)
  expect_class(model$fitted_model, fitted_class)
  expect_equal(sort(model$removed_rows), sort(removed_rows))
  expect_equal(sort(model$removed_x_cols), sort(removed_x_cols))
}

expect_y <- function(model, y, removed_rows) {
  if (!is_empty(removed_rows)) {
    y <- get_records(y, -removed_rows)
  }
  expect_equal(model$y, y)
}

expect_x <- function(model, x, removed_rows, removed_x_cols, is_x_matrix) {
  expect_x_function <- expect_data_frame
  if (is_x_matrix) {
    expect_x_function <- expect_matrix
  }
  expect_x_function(
    model$x,
    nrows = nrow(x) - length(removed_rows),
    ncols = ncol(x) - length(removed_x_cols)
  )
}

expect_responses <- function(model, responses) {
  expect_list(model$responses, any.missing = FALSE)
  for (name in names(responses)) {
    expect_identical(model$responses[[name]]$type, responses[[name]]$type)
    expect_identical(model$responses[[name]]$levels, responses[[name]]$levels)
  }
}

expect_hyperparams <- function(model, hyperparams, tune_grid_proportion) {
  all_hyperparams <- expand.grid(hyperparams)
  n <- nrow(all_hyperparams)
  rows <- sample(n, ceiling(n * tune_grid_proportion))
  all_hyperparams <- all_hyperparams[rows, ]

  expect_list(model$hyperparams, any.missing = FALSE)
  expect_data_frame(
    model$hyperparams_grid,
    nrows = nrow(all_hyperparams),
    ncols = ncol(all_hyperparams) + 1
  )
  expect_names(
    colnames(model$hyperparams_grid),
    permutation.of = c("loss", names(hyperparams))
  )

  expect_numeric(
    model$hyperparams_grid$loss,
    finite = TRUE,
    sorted = TRUE
  )

  expect_equal(model$best_hyperparams, as.list(head(model$hyperparams_grid, 1)))
}

expect_numeric_predictions <- function(predictions, len, with_platt_scaling) {
  list_len <- 1
  if (with_platt_scaling) {
    list_len <- 2
  }
  expect_list(predictions, any.missing = FALSE, len = list_len)

  expect_numeric(
    predictions$predicted,
    any.missing = FALSE,
    finite = TRUE,
    len = len
  )

  if (with_platt_scaling) {
    expect_numeric(
      predictions$predicted_platt,
      any.missing = FALSE,
      finite = TRUE,
      len = len
    )
  }
}

expect_class_predictions <- function(predictions,
                                     len,
                                     response,
                                     with_platt_scaling) {
  list_len <- 2
  if (with_platt_scaling) {
    list_len <- 4
  }
  expect_list(predictions, any.missing = FALSE, len = list_len)

  expect_factor(
    predictions$predicted,
    levels = response$levels,
    any.missing = FALSE,
    len = len
  )

  expect_data_frame(
    predictions$probabilities,
    any.missing = FALSE,
    nrows = len,
    ncols = length(response$levels)
  )
  expect_names(
    colnames(predictions$probabilities),
    identical.to = response$levels
  )

  if (with_platt_scaling) {
    expect_factor(
      predictions$predicted_platt,
      levels = response$levels,
      any.missing = FALSE,
      len = len
    )

    expect_data_frame(
      predictions$probabilities_platt,
      any.missing = FALSE,
      nrows = len,
      ncols = length(response$levels)
    )

    expect_names(
      colnames(predictions$probabilities),
      identical.to = response$levels
    )
  }
}

expect_predictions <- function(model,
                               x,
                               responses,
                               is_multivariate,
                               with_platt_scaling) {
  if (is.null(x)) {
    predictions <- predict(model)
  } else {
    x <- na.omit(x)
    predictions <- predict(model, x)
  }

  if (!is_multivariate) {
    predictions <- list(y = predictions)
  }

  expect_list(predictions, len = length(responses), any.missing = FALSE)

  for (name in names(responses)) {
    response <- responses[[name]]

    if (is_numeric_response(response$type)) {
      expect_numeric_predictions(
        predictions[[name]],
        len = nrow(x),
        with_platt_scaling = with_platt_scaling
      )
    } else {
      expect_class_predictions(
        predictions[[name]],
        len = nrow(x),
        response = response,
        with_platt_scaling = with_platt_scaling
      )
    }
  }
}

expect_numeric_coefs <- function(coefs, expected_names, has_intercept) {
  if (has_intercept) {
    expected_names <- c("(Intercept)", expected_names)
  }

  expect_numeric(
    coefs,
    finite = TRUE,
    any.missing = FALSE,
    len = length(expected_names)
  )

  expect_names(names(coefs), identical.to = expected_names)
}

expect_class_coefs <- function(coefs,
                               classes,
                               coefs_names,
                               by_category,
                               has_all_row,
                               has_intercept) {

  if (by_category) {
    if (has_intercept) {
      coefs_names <- c("(Intercept)", coefs_names)
    }

    expect_matrix(
      coefs,
      any.missing = FALSE,
      nrows = length(classes) + as.numeric(has_all_row),
      ncols = length(coefs_names)
    )

    rows_names <- classes
    if (has_all_row) {
      rows_names <- c("all", classes)
    }
    expect_names(rownames(coefs), identical.to = rows_names)
    expect_names(colnames(coefs), identical.to = coefs_names)
  } else {
    expect_numeric_coefs(
      coefs = coefs,
      expected_names = coefs_names,
      has_intercept = has_intercept
    )
  }
}

expect_coefs <- function(model,
                         expected_names,
                         responses,
                         is_multivariate,
                         has_intercept = FALSE,
                         by_category = FALSE,
                         has_all_row = TRUE) {
  coefs <- coef(model)

  if (!is_multivariate) {
    coefs <- list(y = coefs)
  }

  for (name in names(responses)) {
    response <- responses[[name]]

    if (is_numeric_response(response$type)) {
      expect_numeric_coefs(
        coefs = coefs[[name]],
        expected_names = expected_names,
        has_intercept = has_intercept
      )
    } else {
      expect_class_coefs(
        coefs = coefs[[name]],
        classes = response$levels,
        coefs_names = expected_names,
        by_category = by_category,
        has_all_row = has_all_row,
        has_intercept = has_intercept
      )
    }
  }
}

expect_random_forest <- function(model,
                                 x,
                                 y,
                                 hyperparams,
                                 responses,
                                 tune_grid_proportion = 1,
                                 is_regression_model = NULL,
                                 removed_rows = NULL,
                                 removed_x_cols = NULL,
                                 is_multivariate = FALSE) {
  expect_model(
    model = model,
    x = x,
    y = y,
    hyperparams = hyperparams,
    responses = responses,
    tune_grid_proportion = tune_grid_proportion,
    class_name = "RandomForestModel",
    fitted_class = "rfsrc",
    removed_rows = removed_rows,
    removed_x_cols = removed_x_cols,
    allow_coefficients = TRUE,
    is_multivariate = is_multivariate,
    by_category = !is_multivariate,
    has_all_row = TRUE
  )

  expect_equal(model$is_regression_model, is_regression_model)

  expect_list(model$other_params, any.missing = FALSE)
  expect_subset(
    model$other_params$na_action,
    c("na.omit", "na.impute")
  )
  expect_formula(model$other_params$model_formula)
  expect_logical(model$other_params$importance, len = 1, any.missing = FALSE)
  expect_number(model$other_params$splits_number, lower = 1, finite = TRUE)
  expect_numeric(
    model$other_params$x_vars_weights,
    null.ok = TRUE,
    any.missing = FALSE,
    len = ncol(x) - length(removed_x_cols)
  )
  expect_numeric(
    model$other_params$records_weights,
    null.ok = TRUE,
    any.missing = FALSE,
    len = nrow(x) - length(removed_rows)
  )
}

expect_generalized_boosted_machine <- function(model,
                                               x,
                                               y,
                                               hyperparams,
                                               responses,
                                               distribution,
                                               tune_grid_proportion = 1,
                                               removed_rows = NULL,
                                               removed_x_cols = NULL) {
  expect_model(
    model = model,
    x = x,
    y = y,
    hyperparams = hyperparams,
    responses = responses,
    tune_grid_proportion = tune_grid_proportion,
    class_name = "GeneralizedBoostedMachineModel",
    fitted_class = "gbm",
    removed_rows = removed_rows,
    removed_x_cols = removed_x_cols,
    allow_coefficients = FALSE,
    is_multivariate = FALSE,
    by_category = FALSE,
    has_all_row = FALSE
  )

  expect_list(model$other_params, any.missing = FALSE)
  expect_numeric(
    model$other_params$predictors_relationship,
    null.ok = TRUE,
    any.missing = FALSE,
    len = ncol(x) - length(removed_x_cols)
  )
  expect_numeric(
    model$other_params$records_weights,
    null.ok = TRUE,
    any.missing = FALSE,
    len = nrow(x) - length(removed_rows)
  )

  expect_equal(model$other_params$distribution, distribution)
}

expect_generalized_linear_model <- function(model,
                                            x,
                                            y,
                                            hyperparams,
                                            responses,
                                            response_family,
                                            lambdas_number,
                                            tune_grid_proportion = 1,
                                            removed_rows = NULL,
                                            removed_x_cols = NULL,
                                            is_multivariate = FALSE) {
  if (is.null(hyperparams$lambda)) {
    expect_equal(model$other_params$lambdas_number, lambdas_number)
    expect_numeric(
      model$hyperparams$lambda,
      max.len = lambdas_number,
      min.len = 1,
      any.missing = FALSE
    )
    hyperparams$lambda <- model$hyperparams$lambda
  }

  expect_model(
    model = model,
    x = x,
    y = y,
    hyperparams = hyperparams,
    responses = responses,
    tune_grid_proportion = tune_grid_proportion,
    class_name = "GeneralizedLinearModel",
    fitted_class = "glmnet",
    removed_rows = removed_rows,
    removed_x_cols = removed_x_cols,
    allow_coefficients = TRUE,
    is_multivariate = is_multivariate,
    by_category = !is_multivariate && is_categorical_response(responses$y$type),
    has_all_row = FALSE,
    is_x_matrix = TRUE,
    has_intercept = model$other_params$fit_intercept
  )

  expect_logical(model$other_params$fit_intercept, len = 1, any.missing = FALSE)

  expect_list(model$other_params, any.missing = FALSE)
  expect_number(
    model$other_params$lambdas_number,
    lower = 1,
    finite = TRUE,
    null.ok = TRUE
  )
  expect_number(
    model$other_params$lambda_min_ratio,
    lower = 0,
    finite = TRUE,
    null.ok = TRUE
  )
  expect_logical(model$other_params$standardize, len = 1, any.missing = FALSE)

  expect_numeric(
    model$other_params$records_weights,
    null.ok = TRUE,
    any.missing = FALSE,
    len = nrow(x) - length(removed_rows)
  )

  expect_equal(model$other_params$response_family, response_family)
}

expect_support_vector_machine <- function(model,
                                          x,
                                          y,
                                          hyperparams,
                                          responses,
                                          tune_grid_proportion = 1,
                                          removed_rows = NULL,
                                          removed_x_cols = NULL) {
  expect_model(
    model = model,
    x = x,
    y = y,
    hyperparams = hyperparams,
    responses = responses,
    tune_grid_proportion = tune_grid_proportion,
    class_name = "SupportVectorMachineModel",
    fitted_class = "svm",
    removed_rows = removed_rows,
    removed_x_cols = removed_x_cols,
    allow_coefficients = FALSE,
    is_multivariate = FALSE,
    by_category = FALSE,
    has_all_row = FALSE,
    is_x_matrix = TRUE
  )

  expect_list(model$other_params, any.missing = FALSE)

  assert_svm_kernel(model$other_params$kernel)
  expect_numeric(
    model$other_params$class_weights,
    finite = TRUE,
    any.missing = FALSE,
    null.ok = TRUE
  )
  expect_number(model$other_params$cache_size, finite = TRUE)
  expect_number(model$other_params$tolerance, finite = TRUE)
  expect_number(model$other_params$epsilon, finite = TRUE)

  expect_logical(model$other_params$shrinking, len = 1, any.missing = FALSE)
  expect_logical(model$other_params$fitted, len = 1, any.missing = FALSE)

  expect_logical(
    model$other_params$scale,
    any.missing = FALSE,
    max.len = ncol(x) - length(removed_x_cols)
  )
}

expect_deep_learning <- function(model,
                                 x,
                                 y,
                                 hyperparams,
                                 responses,
                                 tune_grid_proportion = 1,
                                 removed_rows = NULL,
                                 removed_x_cols = NULL,
                                 is_multivariate = FALSE,
                                 with_platt_scaling = FALSE) {
  expect_model(
    model = model,
    x = x,
    y = y,
    hyperparams = hyperparams,
    responses = responses,
    tune_grid_proportion = tune_grid_proportion,
    class_name = "DeepLearningModel",
    fitted_class = "keras.engine.training.Model",
    removed_rows = removed_rows,
    removed_x_cols = removed_x_cols,
    allow_coefficients = FALSE,
    is_multivariate = is_multivariate,
    by_category = FALSE,
    has_all_row = FALSE,
    is_x_matrix = TRUE,
    with_platt_scaling = with_platt_scaling
  )

  expect_list(model$other_params, any.missing = FALSE)
  expect_logical(model$other_params$shuffle, len = 1, any.missing = FALSE)
  expect_logical(model$other_params$early_stop, len = 1, any.missing = FALSE)
  expect_number(model$other_params$early_stop_patience, finite = TRUE)

  if (with_platt_scaling) {
    expect_identical(model$other_params$with_platt_scaling, TRUE)
    expect_number(model$other_params$platt_proportion, finite = TRUE)
  }
}

expect_bayesian_model <- function(model,
                                  x,
                                  y,
                                  responses,
                                  bglr_response_type,
                                  testing_indices = NULL,
                                  removed_rows = NULL,
                                  removed_x_cols = NULL,
                                  is_multivariate = FALSE) {
  expect_base_params(
    model = model,
    class_name = "BayesianModel",
    allow_coefficients = TRUE,
    is_multivariate = is_multivariate,
    fitted_class = if (is_multivariate) "list" else "BGLR",
    removed_rows = removed_rows,
    removed_x_cols = removed_x_cols
  )

  expect_y(model = model, y = y, removed_rows = removed_rows)

  for (x_name in names(x)) {
    expect_list(model$x[[x_name]], any.missing = FALSE, len = 2)
    inner_x <- nonull(model$x[[x_name]]$X, model$x[[x_name]]$K)
    original_x <- x[[x_name]]$x

    if (!is_empty(removed_rows)) {
      original_x <- get_records(x[[x_name]]$x, -removed_rows)
    }
    expect_equal(original_x, inner_x)

    expect_string(model$x[[x_name]]$model)

    # Coefficients --------------------------------------------------
    expected_names <- colnames(inner_x)
    if (!is_empty(removed_x_cols)) {
      expected_names <- expected_names[-removed_x_cols]
    }
    coefs <- coef(model)
    if (!is_multivariate) {
      coefs <- list(y = coefs)
    }

    for (response_name in names(responses)) {
      expect_numeric_coefs(
        coefs = coefs[[response_name]][[x_name]],
        expected_names = expected_names,
        has_intercept = FALSE
      )
    }
  }

  expect_responses(model, responses)

  expect_predictions(
    model = model,
    x = NULL,
    responses = responses,
    is_multivariate = is_multivariate,
    with_platt_scaling = FALSE
  )

  expect_list(model$other_params, any.missing = FALSE)

  expect_number(model$other_params$iterations_number, lower = 1, finite = TRUE)
  expect_number(model$other_params$burn_in, lower = 1, finite = TRUE)
  expect_number(model$other_params$thinning, lower = 1, finite = TRUE)

  if (is_multivariate) {
    expect_list(model$other_params$covariance_structure, len = 3)
    expect_number(model$other_params$covariance_structure$df0, finite = TRUE)
    expect_matrix(
      model$other_params$covariance_structure$S0,
      nrows = ncol(y),
      ncols = ncol(y),
      null.ok = TRUE
    )
    expect_string(model$other_params$covariance_structure$type)
  }

  expect_equal(
    sort(model$other_params$testing_indices),
    sort(testing_indices)
  )

  expect_numeric(
    model$other_params$records_weights,
    null.ok = TRUE,
    any.missing = FALSE,
    len = nrow(x) - length(removed_rows)
  )

  expect_numeric(
    model$other_params$response_groups,
    null.ok = TRUE,
    any.missing = FALSE,
    len = nrow(x) - length(removed_rows)
  )

  expect_equal(model$other_params$bglr_response_type, bglr_response_type)
}
