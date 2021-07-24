suppressMessages(library(dplyr))

manual_test <- FALSE

data(Iris)

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

test_that("Univariate categorical (no tuning)", {
  model <- random_forest(x_cat, y_cat, seed = 1, verbose = FALSE)

  expect_random_forest(
    model = model,
    x = x_cat,
    y = y_cat,
    hyperparams = list(trees_number = 500, node_size = 5),
    responses = list(
      y = list(type = RESPONSE_TYPES$CATEGORICAL, levels = levels(y_cat))
    )
  )
})

test_that("Univariate categorical (tuning)", {
  hyperparams <- list(
    trees_number = c(5, 10),
    node_size = c(5),
    sampled_x_vars_number = c(0.5, 0.2, 1)
  )

  model <- random_forest(
    x_cat,
    y_cat,
    trees_number = hyperparams$trees_number,
    node_size = hyperparams$node_size,
    node_depth = hyperparams$node_depth,
    sampled_x_vars_number = hyperparams$sampled_x_vars_number,

    tune_cv_type = "K_fold",
    tune_folds_number = 5,
    tune_grid_proportion = 0.5,

    x_vars_weights = runif(ncol(x_cat)),
    records_weights = runif(nrow(x_cat)),

    seed = 1,
    verbose = FALSE
  )

  expect_random_forest(
    model = model,
    x = x_cat,
    y = y_cat,
    hyperparams = hyperparams,
    responses = list(
      y = list(type = RESPONSE_TYPES$CATEGORICAL, levels = levels(y_cat))
    ),
    tune_grid_proportion = 0.5
  )
})

test_that("Univariate numeric (NA no tuning)", {
  x <- x_num
  y <- y_num
  x[2, 3] <- NA
  x[56, 2] <- NA
  x[144, 1] <- NA
  y[100] <- NA
  model <- suppressWarnings(random_forest(x, y, seed = 1, verbose = FALSE))

  expect_random_forest(
    model = model,
    x = x,
    y = y,
    hyperparams = list(trees_number = 500, node_size = 5),
    responses = list(
      y = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL)
    ),
    removed_rows = c(2, 56, 100, 144)
  )
})

test_that("Univariate numeric (NA tuning)", {
  x <- x_num
  y <- y_num
  x[2, 3] <- NA
  x[56, 2] <- NA
  x[144, 1] <- NA
  y[100] <- NA

  hyperparams <- list(
    trees_number = c(10, 20),
    node_size = c(3, 5),
    node_depth = c(15, 5),
    sampled_x_vars_number = c(0.5, 0.3, 0.8)
  )

  model <- suppressWarnings(random_forest(
    x,
    y,

    trees_number = hyperparams$trees_number,
    node_size = hyperparams$node_size,
    node_depth = hyperparams$node_depth,
    sampled_x_vars_number = hyperparams$sampled_x_vars_number,

    tune_cv_type = "Random",
    tune_folds_number = 3,
    tune_grid_proportion = 0.4,

    na_action = "impute",

    seed = 1,
    verbose = FALSE
  ))

  expect_random_forest(
    model = model,
    x = x,
    y = y,
    hyperparams = hyperparams,
    responses = list(
      y = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL)
    ),
    tune_grid_proportion = 0.4
  )
})

test_that("Multivariate numeric (no tuning)", {
  model <- random_forest(x_multi, y_multi, seed = 1, verbose = FALSE)

  expect_random_forest(
    model = model,
    x = x_multi,
    y = y_multi,
    hyperparams = list(trees_number = 500, node_size = 5),
    responses = list(
      y1 = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL),
      y2 = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL)
    ),
    is_regression_model = TRUE,
    is_multivariate = TRUE
  )
})

test_that("Multivariate numeric (tuning)", {
  hyperparams <- list(
    trees_number = 5,
    node_size = c(3, 5),
    node_depth = 10,
    sampled_x_vars_number = 0.4
  )

  model <- random_forest(
    x_multi,
    y_multi,

    trees_number = hyperparams$trees_number,
    node_size = hyperparams$node_size,
    node_depth = hyperparams$node_depth,
    sampled_x_vars_number = hyperparams$sampled_x_vars_number,

    tune_cv_type = "random",
    tune_folds_number = 4,

    seed = 1,
    verbose = FALSE
  )

  expect_random_forest(
    model = model,
    x = x_multi,
    y = y_multi,
    hyperparams = hyperparams,
    responses = list(
      y1 = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL),
      y2 = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL)
    ),
    is_regression_model = TRUE,
    is_multivariate = TRUE
  )
})

test_that("Multivariate combined (no tuning)", {
  model <- random_forest(x_multi_cat, y_multi_cat, seed = 1, verbose = FALSE)

  expect_random_forest(
    model = model,
    x = x_multi_cat,
    y = y_multi_cat,
    hyperparams = list(trees_number = 500, node_size = 5),
    responses = list(
      y1 = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL),
      y2 = list(
        type = RESPONSE_TYPES$CATEGORICAL,
        levels = levels(y_multi_cat$y2)
      )
    ),
    is_regression_model = FALSE,
    is_multivariate = TRUE
  )
})

test_that("Multivariate combined (tuning)", {
  hyperparams <- list(
    trees_number = c(2, 4),
    node_size = c(3, 5),
    node_depth = c(5, 10),
    sampled_x_vars_number = c(0.4, 0.2)
  )

  model <- random_forest(
    x_multi_cat,
    y_multi_cat,

    trees_number = hyperparams$trees_number,
    node_size = hyperparams$node_size,
    node_depth = hyperparams$node_depth,
    sampled_x_vars_number = hyperparams$sampled_x_vars_number,

    tune_cv_type = "random",
    tune_folds_number = 4,
    tune_grid_proportion = 0.3,

    records_weights = runif(nrow(x_multi_cat)),

    seed = 1,
    verbose = FALSE
  )

  expect_random_forest(
    model = model,
    x = x_multi_cat,
    y = y_multi_cat,
    hyperparams = hyperparams,
    responses = list(
      y1 = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL),
      y2 = list(
        type = RESPONSE_TYPES$CATEGORICAL,
        levels = levels(y_multi_cat$y2)
      )
    ),
    tune_grid_proportion = 0.3,
    is_regression_model = FALSE,
    is_multivariate = TRUE
  )
})

test_that("Multivariate numeric (NA no tuning)", {
  x <- x_multi
  y <- y_multi
  x[5, 1] <- NA
  x[10, 1] <- NA
  x[50, 1] <- NA
  y[44, 1] <- NA
  y[22, 1] <- NA
  model <- suppressWarnings(random_forest(x, y, seed = 1, verbose = FALSE))

  expect_random_forest(
    model = model,
    x = x,
    y = y,
    hyperparams = list(trees_number = 500, node_size = 5),
    responses = list(
      y1 = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL),
      y2 = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL)
    ),
    is_regression_model = TRUE,
    is_multivariate = TRUE,
    removed_rows = c(5, 10, 50, 44, 22)
  )
})

test_that("Multivariate numeric (tuning)", {
  x <- x_multi
  y <- y_multi
  x[5, 1] <- NA
  x[10, 1] <- NA
  x[50, 1] <- NA
  y[44, 1] <- NA
  y[22, 1] <- NA

  hyperparams <- list(
    trees_number = 5,
    node_size = c(3, 5),
    node_depth = 10,
    sampled_x_vars_number = 0.4
  )

  model <- random_forest(
    x,
    y,

    trees_number = hyperparams$trees_number,
    node_size = hyperparams$node_size,
    node_depth = hyperparams$node_depth,
    sampled_x_vars_number = hyperparams$sampled_x_vars_number,

    tune_cv_type = "random",
    tune_folds_number = 4,

    na_action = "impute",

    seed = 1,
    verbose = FALSE
  )

  expect_random_forest(
    model = model,
    x = x,
    y = y,
    hyperparams = hyperparams,
    responses = list(
      y1 = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL),
      y2 = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL)
    ),
    is_regression_model = TRUE,
    is_multivariate = TRUE
  )
})
