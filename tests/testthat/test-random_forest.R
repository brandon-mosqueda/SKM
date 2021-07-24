suppressMessages(library(dplyr))

manual_test <- FALSE

data(Iris)

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

expect_random_forest <- function(model,
                                 x,
                                 y,
                                 hyperparams,
                                 responses,
                                 is_regression_model = NULL,
                                 removed_rows = NULL,
                                 removed_x_cols = NULL,
                                 allow_coefficients = TRUE,
                                 is_multivariate = FALSE) {
  expect_model(
    model = model,
    x = x,
    y = y,
    x_ncols = ncol(x),
    hyperparams = hyperparams,
    responses = responses,
    class_name = "RandomForestModel",
    fitted_class = "rfsrc",
    removed_rows = removed_rows,
    removed_x_cols = removed_x_cols,
    allow_coefficients = allow_coefficients,
    is_multivariate = is_multivariate
  )

  expect_equal(model$is_regression_model, is_regression_model)

  expect_list(model$other_params, any.missing = FALSE)
  expect_identical(model$other_params$na_action, "na.omit")
  expect_formula(model$other_params$model_formula)
}

test_that("Univariate numeric (no tuning)", {
  model <- random_forest(x_num, y_num, seed = 1, verbose = FALSE)

  expect_random_forest(
    model = model,
    x = x_num,
    y = y_num,
    hyperparams = list(trees_number = 500, node_size = 5),
    responses = list(
      y = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL)
    )
  )

  predictions <- predict(model, x_num)
  expect_list(predictions, any.missing = FALSE, len = 1)
  expect_numeric(
    predictions$predicted,
    any.missing = FALSE,
    finite = TRUE,
    len = nrow(x_num)
  )

  coefs <- coef(model)
  expect_numeric(
    coefs,
    finite = TRUE,
    any.missing = FALSE,
    len = ncol(x_num)
  )
  expect_names(names(coefs), identical.to = colnames(x_num))
})
