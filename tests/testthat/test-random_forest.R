suppressMessages(library(dplyr))

manual_test <- FALSE

data(Iris)

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

  expect_predictions(model = model, x = x, responses = responses)

  expect_coefs(
    model = model,
    expected_names = colnames(x),
    responses = responses,
    is_multivariate = is_multivariate
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
