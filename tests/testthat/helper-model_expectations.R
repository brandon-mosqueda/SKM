expect_difftime <- function(x) {
  expect_class(x, "difftime")
  expect_number(as.numeric(x), lower = 0, finite = TRUE)
}

expect_model <- function(model,
                         x,
                         y,
                         x_ncols,
                         hyperparams,
                         responses,
                         class_name,
                         fitted_class,
                         removed_rows,
                         removed_x_cols,
                         allow_coefficients,
                         is_multivariate) {
  expect_class(model, class_name)
  expect_difftime(model$execution_time)
  expect_equal(model$removed_rows, removed_rows)
  expect_equal(model$removed_x_cols, removed_x_cols)

  if (!is_empty(removed_rows)) {
    y <- get_records(y, -removed_rows)
  }
  expect_equal(model$y, y)

  expect_data_frame(
    model$x,
    nrows = nrow(x) - length(removed_rows),
    ncols = x_ncols - length(removed_x_cols)
  )

  all_hyperparams <- expand.grid(hyperparams)

  expect_list(model$hyperparams, any.missing = FALSE)
  expect_data_frame(
    model$hyperparams_grid,
    nrows = nrow(all_hyperparams),
    ncols = ncol(all_hyperparams) + 1
  )

  expect_equal(model$best_hyperparams, as.list(head(model$hyperparams_grid, 1)))

  expect_class(model$fitted_model, fitted_class)

  expect_list(model$responses, any.missing = FALSE)
  for (name in names(responses)) {
    expect_identical(model$responses[[name]]$type, responses[[name]]$type)
    expect_null(model$responses[[name]]$levels, responses[[name]]$levels)
  }

  expect_identical(model$allow_coefficients, allow_coefficients)
  expect_identical(model$is_multivariate, is_multivariate)
}

expect_numeric_predictions <- function(predictions, len) {
  expect_list(predictions, any.missing = FALSE, len = 1)

  expect_numeric(
    predictions$predicted,
    any.missing = FALSE,
    finite = TRUE,
    len = len
  )
}

expect_class_predictions <- function(predictions, len, response) {
  expect_list(predictions, any.missing = FALSE, len = 2)

  expect_factor(
    predictions$predicted,
    levels = response$levels,
    empty.levels.ok = FALSE,
    any.missing = FALSE,
    len = len
  )

  expect_data_frame(
    predictions$probabilities,
    any.missing = FALSE,
    nrows = len,
    colnames = response$levels
  )
}

expect_predictions <- function(model, x, responses) {
  predictions <- predict(model, x)

  for (name in names(responses)) {
    response <- responses[[name]]
    if (is_numeric_response(response$type)) {
      expect_numeric_predictions(predictions, len = nrow(x))
    } else {
      expect_class_predictions(predictions, len = nrow(x), response = response)
    }
  }
}

expect_numeric_coefs <- function(coefs, expected_names) {
  expect_numeric(
    coefs,
    finite = TRUE,
    any.missing = FALSE,
    len = length(expected_names)
  )

  expect_names(names(coefs), identical.to = expected_names)
}

expect_class_coefs <- function(coefs, classes, coefs_names, by_category) {
  if (by_category) {
    expect_matrix(
      coefs,
      any.missing = FALSE,
      nrows = length(classes),
      ncols = length(coefs_names)
    )

    expect_names(rownames(coefs), identical.to = classes)
    expect_names(colnames(coefs), identical.to = coefs_names)
  } else {
    expect_numeric_coefs(coefs = coefs, expected_names = coefs_names)
  }
}

expect_coefs <- function(model,
                         expected_names,
                         responses,
                         is_multivariate,
                         by_category = FALSE) {
  coefs <- coef(model)

  if (!is_multivariate) {
    coefs <- list(y = coefs)
  }

  for (name in names(responses)) {
    response <- responses[[name]]

    if (is_numeric_response(response$type)) {
      expect_numeric_coefs(
        coefs = coefs[[name]],
        expected_names = expected_names
      )
    } else {
      expect_class_coefs(
        coefs = coefs[[name]],
        classes = response$levels,
        coefs_names = expected_names,
        by_category = by_category
      )
    }
  }
}