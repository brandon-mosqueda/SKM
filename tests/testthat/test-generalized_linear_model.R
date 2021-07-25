data(Iris)

test_that("Univariate numeric (no tuning)", {
  model <- generalized_linear_model(x_num, y_num, seed = 1, verbose = FALSE)

  expect_generalized_linear_model(
    model = model,
    x = to_matrix(x_num),
    y = y_num,
    hyperparams = list(alpha = 1),
    responses = list(
      y = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL)
    ),
    lambdas_number = 100,
    response_family = "gaussian"
  )
})

test_that("Univariate numeric (tuning)", {
  hyperparams <- list(
    alpha = c(0, 0.5, 1),
    lambda = c(0.1, 0.2, 0.5)
  )

  model <- generalized_linear_model(
    x_num,
    y_num,

    alpha = hyperparams$alpha,
    lambda = hyperparams$lambda,

    records_weights = runif(nrow(x_num)),

    seed = 1,
    verbose = FALSE
  )

  expect_generalized_linear_model(
    model = model,
    x = to_matrix(x_num),
    y = y_num,
    hyperparams = hyperparams,
    responses = list(
      y = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL)
    ),
    lambdas_number = NULL,
    response_family = "gaussian"
  )
})

test_that("Univariate binary (no tuning)", {
  model <- generalized_linear_model(x_bin, y_bin, seed = 1, verbose = FALSE)

  expect_generalized_linear_model(
    model = model,
    x = to_matrix(x_bin),
    y = y_bin,
    hyperparams = list(alpha = 1),
    responses = list(
      y = list(type = RESPONSE_TYPES$BINARY, levels = levels(y_bin))
    ),
    lambdas_number = 100,
    response_family = "binomial"
  )
})

test_that("Univariate binary (tuning)", {
  hyperparams <- list(
    alpha = seq(0.3, 1, by = 0.1)
  )

  model <- generalized_linear_model(
    x_bin,
    y_bin,

    alpha = hyperparams$alpha,
    lambdas_number = 10,

    tune_cv_type = "Random",
    tune_folds_number = 2,
    tune_testing_proportion = 0.3,
    tune_grid_proportion = 0.8,

    seed = 1,
    verbose = FALSE
  )

  expect_generalized_linear_model(
    model = model,
    x = x_bin,
    y = y_bin,
    hyperparams = hyperparams,
    responses = list(
      y = list(type = RESPONSE_TYPES$BINARY, levels = levels(y_bin))
    ),
    tune_grid_proportion = 0.8,
    lambdas_number = 10,
    response_family = "binomial"
  )
})

test_that("Univariate categorical (no tuning)", {
  model <- generalized_linear_model(x_cat, y_cat, seed = 1, verbose = FALSE)

  expect_generalized_linear_model(
    model = model,
    x = x_cat,
    y = y_cat,
    hyperparams = list(alpha = 1),
    responses = list(
      y = list(type = RESPONSE_TYPES$CATEGORICAL, levels = levels(y_cat))
    ),
    response_family = "multinomial",
    lambdas_number = 100
  )
})

test_that("Univariate categorical (tuning)", {
  hyperparams <- list(
    alpha = c(0.01, 0.5, 0.6, 0.75, 1),
    lambda = c(0.5, 0.2, 1)
  )

  model <- generalized_linear_model(
    x_cat,
    y_cat,

    alpha = hyperparams$alpha,
    lambda = hyperparams$lambda,

    tune_cv_type = "K_fold",
    tune_folds_number = 5,
    tune_grid_proportion = 0.5,

    records_weights = runif(nrow(x_cat)),
    standardize = FALSE,

    seed = 1,
    verbose = FALSE
  )

  expect_generalized_linear_model(
    model = model,
    x = x_cat,
    y = y_cat,
    hyperparams = hyperparams,
    responses = list(
      y = list(type = RESPONSE_TYPES$CATEGORICAL, levels = levels(y_cat))
    ),
    tune_grid_proportion = 0.5,
    response_family = "multinomial",
    lambdas_number = 100
  )
})

test_that("Univariate numeric (NA no tuning)", {
  x <- x_num
  y <- y_num
  x[2, 3] <- NA
  x[56, 2] <- NA
  x[144, 1] <- NA
  y[100] <- NA
  model <- suppressWarnings(generalized_linear_model(
    x,
    y,
    seed = 1,
    verbose = FALSE
  ))

  expect_generalized_linear_model(
    model = model,
    x = to_matrix(x),
    y = y,
    hyperparams = list(alpha = 1),
    responses = list(
      y = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL)
    ),
    removed_rows = c(2, 56, 100, 144),
    response_family = "gaussian",
    lambdas_number = 100
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
    alpha = seq(0, 1, by = 0.2),
    lambda = c(0.5, 0.3, 0.8)
  )

  model <- suppressWarnings(generalized_linear_model(
    x,
    y,

    alpha = hyperparams$alpha,
    lambda = hyperparams$lambda,

    tune_cv_type = "Random",
    tune_folds_number = 3,
    tune_grid_proportion = 0.4,

    seed = 1,
    verbose = FALSE
  ))

  expect_generalized_linear_model(
    model = model,
    x = to_matrix(x),
    y = y,
    hyperparams = hyperparams,
    responses = list(
      y = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL)
    ),
    tune_grid_proportion = 0.4,
    response_family = "gaussian",
    lambdas_number = 100,
    removed_rows = c(2, 56, 144, 100)
  )
})

test_that("Multivariate numeric (no tuning)", {
  model <- generalized_linear_model(x_multi, y_multi, seed = 1, verbose = FALSE)

  y <- to_matrix(y_multi)
  rownames(y) <- NULL
  expect_generalized_linear_model(
    model = model,
    x = to_matrix(x_multi),
    y = y,
    hyperparams = list(alpha = 1),
    responses = list(
      y1 = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL),
      y2 = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL)
    ),
    is_multivariate = TRUE,
    response_family = "mgaussian",
    lambdas_number = 100
  )
})

test_that("Multivariate numeric (tuning)", {
  hyperparams <- list(
    alpha = c(0.5, 0.6, 0.7),
    lambda = c(0.1, 0.002, 0.05)
  )

  model <- generalized_linear_model(
    x_multi,
    y_multi,

    alpha = hyperparams$alpha,
    lambda = hyperparams$lambda,

    tune_cv_type = "random",
    tune_folds_number = 4,

    seed = 1,
    verbose = FALSE
  )

  y <- to_matrix(y_multi)
  rownames(y) <- NULL
  expect_generalized_linear_model(
    model = model,
    x = to_matrix(x_multi),
    y = y,
    hyperparams = hyperparams,
    responses = list(
      y1 = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL),
      y2 = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL)
    ),
    is_multivariate = TRUE,
    response_family = "mgaussian",
    lambdas_number = 100
  )
})

test_that("Multivariate combined (no tuning)", {
  model <- suppressWarnings(generalized_linear_model(
    x_multi_cat,
    y_multi_cat,
    seed = 1,
    verbose = FALSE
  ))

  expect_generalized_linear_model(
    model = model,
    x = x_multi_cat,
    y = data.matrix(y_multi_cat),
    hyperparams = list(alpha = 1),
    responses = list(
      y1 = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL),
      y2 = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL)
    ),
    is_multivariate = TRUE,
    lambdas_number = 100,
    response_family = "mgaussian"
  )
})

test_that("Multivariate combined (tuning)", {
  hyperparams <- list(
    alpha = c(0, 1, 0.3, 0.5)
  )

  model <- suppressWarnings(generalized_linear_model(
    x_multi_cat,
    y_multi_cat,

    alpha = hyperparams$alpha,

    tune_cv_type = "random",
    tune_folds_number = 4,
    tune_grid_proportion = 0.3,

    lambdas_number = 20,
    records_weights = runif(nrow(x_multi_cat)),

    seed = 1,
    verbose = FALSE
  ))

  y <- data.matrix(y_multi_cat)
  rownames(y) <- NULL
  expect_generalized_linear_model(
    model = model,
    x = x_multi_cat,
    y = y,
    hyperparams = hyperparams,
    responses = list(
      y1 = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL),
      y2 = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL)
    ),
    tune_grid_proportion = 0.3,
    is_multivariate = TRUE,
    response_family = "mgaussian",
    lambdas_number = 20
  )
})

test_that("Multivariate numeric (NA no tuning)", {
  x <- x_multi
  y <- data.matrix(y_multi)
  rownames(y) <- NULL
  x[5, 1] <- NA
  x[10, 1] <- NA
  x[50, 1] <- NA
  y[44, 1] <- NA
  y[22, 1] <- NA
  model <- suppressWarnings(generalized_linear_model(
    x,
    y,
    seed = 1,
    verbose = FALSE
  ))

  expect_generalized_linear_model(
    model = model,
    x = to_matrix(x),
    y = y,
    hyperparams = list(alpha = 1),
    responses = list(
      y1 = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL),
      y2 = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL)
    ),
    is_multivariate = TRUE,
    response_family = "mgaussian",
    lambdas_number = 100,
    removed_rows = c(5, 10, 50, 44, 22)
  )
})

test_that("Multivariate numeric (NA tuning)", {
  x <- to_matrix(x_multi)
  y <- data.matrix(y_multi)
  rownames(y) <- NULL
  x[5, 1] <- NA
  x[10, 1] <- NA
  x[50, 1] <- NA
  y[44, 1] <- NA
  y[22, 1] <- NA
  y[5, 2] <- NA
  y[6, 2] <- NA
  x <- cbind(1, x)

  hyperparams <- list(
    alpha = seq(0, 1, by = 0.25),
    lambda = seq(0, 1.5, by = 0.3)
  )

  model <- suppressWarnings(generalized_linear_model(
    x,
    y,

    alpha = hyperparams$alpha,
    lambda = hyperparams$lambda,

    tune_cv_type = "random",
    tune_folds_number = 4,

    records_weights = runif(nrow(x)),

    seed = 1,
    verbose = FALSE
  ))

  expect_generalized_linear_model(
    model = model,
    x = x,
    y = y,
    hyperparams = hyperparams,
    responses = list(
      y1 = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL),
      y2 = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL)
    ),
    is_multivariate = TRUE,
    response_family = "mgaussian",
    lambdas_number = 100,
    removed_rows = c(5, 10, 50, 44, 22, 6),
    removed_x_cols = 1
  )
})
