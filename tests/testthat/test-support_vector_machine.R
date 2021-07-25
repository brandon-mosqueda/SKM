data(Iris)

test_that("Univariate numeric (no tuning)", {
  x <- x_num
  x$temp <- 1
  model <- suppressWarnings(support_vector_machine(
    x,
    y_num,
    seed = 1,
    verbose = FALSE
  ))

  expect_support_vector_machine(
    model = model,
    x = to_matrix(x),
    y = y_num,
    hyperparams = list(
      gamma = 1 / ncol(x),
      degree = 3,
      coef0 = 0,
      cost = 1
    ),
    responses = list(
      y = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL)
    ),
    removed_x_cols = 6
  )
})

test_that("Univariate numeric (tuning)", {
  hyperparams <- list(
    gamma = c(1 / ncol(x_num), 2, 3, 1),
    degree = c(3),
    coef0 = c(0),
    cost = c(0.5, 1)
  )

  model <- support_vector_machine(
    x_num,
    y_num,

    kernel = "radial",

    gamma = hyperparams$gamma,
    degree = hyperparams$degree,
    coef0 = hyperparams$coef0,
    cost = hyperparams$cost,

    seed = 1,
    verbose = FALSE
  )

  expect_support_vector_machine(
    model = model,
    x = to_matrix(x_num),
    y = y_num,
    hyperparams = hyperparams,
    responses = list(
      y = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL)
    )
  )
})

test_that("Univariate binary (no tuning)", {
  model <- support_vector_machine(x_bin, y_bin, seed = 1, verbose = FALSE)

  expect_support_vector_machine(
    model = model,
    x = to_matrix(x_bin),
    y = y_bin,
    hyperparams = list(
      gamma = 1 / ncol(x_bin),
      degree = 3,
      coef0 = 0,
      cost = 1
    ),
    responses = list(
      y = list(type = RESPONSE_TYPES$BINARY, levels = levels(y_bin))
    )
  )
})

test_that("Univariate binary (tuning)", {
  hyperparams <- list(
    gamma = c(1, 2),
    degree = c(2, 1, 3),
    coef0 = c(2, 1),
    cost = c(0.1)
  )

  model <- support_vector_machine(
    to_matrix(x_bin),
    y_bin,

    kernel = "Polynomial",

    gamma = hyperparams$gamma,
    degree = hyperparams$degree,
    coef0 = hyperparams$coef0,
    cost = hyperparams$cost,

    tune_cv_type = "Random",
    tune_folds_number = 2,
    tune_testing_proportion = 0.3,
    tune_grid_proportion = 0.8,

    seed = 1,
    verbose = FALSE
  )

  expect_support_vector_machine(
    model = model,
    x = to_matrix(x_bin),
    y = y_bin,
    hyperparams = hyperparams,
    responses = list(
      y = list(type = RESPONSE_TYPES$BINARY, levels = levels(y_bin))
    ),
    tune_grid_proportion = 0.8
  )
})

test_that("Univariate categorical (no tuning)", {
  model <- support_vector_machine(
    x_cat,
    y_cat,
    seed = 1,
    verbose = FALSE
  )

  expect_support_vector_machine(
    model = model,
    x = to_matrix(x_cat),
    y = y_cat,
    hyperparams = list(
      gamma = 1 / ncol(x_cat),
      degree = 3,
      coef0 = 0,
      cost = 1
    ),
    responses = list(
      y = list(type = RESPONSE_TYPES$CATEGORICAL, levels = levels(y_cat))
    )
  )
})

test_that("Univariate categorical (tuning)", {
  hyperparams <- list(
    gamma = c(2, 1),
    degree = c(1),
    coef0 = c(1, 0.5, 0),
    cost = c(0.1, 0.2)
  )

  class_weights <- c(0.5, 0.1, 1)
  names(class_weights) <- levels(y_cat)

  model <- suppressWarnings(support_vector_machine(
    x_cat,
    y_cat,

    kernel = "sigmoid",

    gamma = hyperparams$gamma,
    degree = hyperparams$degree,
    coef0 = hyperparams$coef0,
    cost = hyperparams$cost,

    tune_cv_type = "K_fold",
    tune_folds_number = 5,
    tune_grid_proportion = 0.5,

    class_weights = class_weights,
    cache_size = 50,
    fitted = FALSE,

    seed = 1,
    verbose = FALSE
  ))

  expect_support_vector_machine(
    model = model,
    x = to_matrix(x_cat),
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
  model <- suppressWarnings(support_vector_machine(
    x,
    y,
    seed = 1,
    verbose = FALSE
  ))

  expect_support_vector_machine(
    model = model,
    x = to_matrix(x),
    y = y,
    hyperparams = list(
      gamma = 1 / ncol(x),
      degree = 3,
      coef0 = 0,
      cost = 1
    ),
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
    gamma = c(1),
    degree = c(1),
    coef0 = c(2),
    cost = c(0.3, 0.5, 0.6)
  )

  model <- suppressWarnings(support_vector_machine(
    x,
    y,

    kernel = "LINEAR",

    gamma = hyperparams$gamma,
    degree = hyperparams$degree,
    coef0 = hyperparams$coef0,
    cost = hyperparams$cost,

    tune_cv_type = "Random",
    tune_folds_number = 3,
    tune_grid_proportion = 0.4,

    seed = 1,
    verbose = FALSE
  ))

  expect_support_vector_machine(
    model = model,
    x = to_matrix(x),
    y = y,
    hyperparams = hyperparams,
    responses = list(
      y = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL)
    ),
    tune_grid_proportion = 0.4,
    removed_rows = c(2, 56, 144, 100)
  )
})
