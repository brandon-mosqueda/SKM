suppressMessages(library(dplyr))

manual_test <- FALSE

data(Iris)

expect_random_forest <- function(model,
                                 x,
                                 y,
                                 hyperparams,
                                 responses,
                                 tune_grid_proportion = 1,
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
    tune_grid_proportion = tune_grid_proportion,
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
  expect_logical(model$other_params$importance, len = 1, any.missing = FALSE)
  expect_number(model$other_params$splits_number, lower = 1, finite = TRUE)
  expect_numeric(
    model$other_params$x_vars_weights,
    null.ok = TRUE,
    any.missing = FALSE,
    len = nrow(x)
  )
  expect_numeric(
    model$other_params$records_weights,
    null.ok = TRUE,
    any.missing = FALSE,
    len = nrow(x)
  )

  expect_predictions(model = model, x = x, responses = responses)

  expect_coefs(
    model = model,
    expected_names = colnames(x),
    responses = responses,
    is_multivariate = is_multivariate,
    by_category = TRUE,
    has_all_row = TRUE
  )
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
})

test_that("Univariate numeric (tuning)", {
  hyperparams <- list(
    trees_number = c(10, 20),
    node_size = c(3, 5),
    node_depth = 15,
    sampled_x_vars_number = 0.5
  )

  model <- random_forest(
    x_num,
    y_num,

    trees_number = hyperparams$trees_number,
    node_size = hyperparams$node_size,
    node_depth = hyperparams$node_depth,
    sampled_x_vars_number = hyperparams$sampled_x_vars_number,

    seed = 1,
    verbose = FALSE
  )

  expect_random_forest(
    model = model,
    x = x_num,
    y = y_num,
    hyperparams = hyperparams,
    responses = list(
      y = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL)
    )
  )
})

test_that("Univariate binary (no tuning)", {
  model <- random_forest(x_bin, y_bin, seed = 1, verbose = FALSE)

  coefs <- coef(model)
  expect_random_forest(
    model = model,
    x = x_bin,
    y = y_bin,
    hyperparams = list(trees_number = 500, node_size = 5),
    responses = list(
      y = list(type = RESPONSE_TYPES$BINARY, levels = levels(y_bin))
    )
  )
})

test_that("Univariate binary (tuning)", {
  hyperparams <- list(
    trees_number = c(5, 10),
    node_size = c(5),
    node_depth = c(10, 15),
    sampled_x_vars_number = c(0.5, 0.2)
  )

  model <- random_forest(
    x_bin,
    y_bin,

    trees_number = hyperparams$trees_number,
    node_size = hyperparams$node_size,
    node_depth = hyperparams$node_depth,
    sampled_x_vars_number = hyperparams$sampled_x_vars_number,

    tune_cv_type = "Random",
    tune_folds_number = 2,
    tune_testing_proportion = 0.3,
    tune_grid_proportion = 0.8,

    seed = 1,
    verbose = FALSE
  )

  expect_random_forest(
    model = model,
    x = x_bin,
    y = y_bin,
    hyperparams = hyperparams,
    responses = list(
      y = list(type = RESPONSE_TYPES$BINARY, levels = levels(y_bin))
    ),
    tune_grid_proportion = 0.8
  )
})
