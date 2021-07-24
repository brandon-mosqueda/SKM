data(Iris)

test_that("Univariate numeric (no tuning)", {
  model <- generalized_boosted_machine(x_num, y_num, seed = 1, verbose = FALSE)

  expect_generalized_boosted_machine(
    model = model,
    x = x_num,
    y = y_num,
    hyperparams = list(
      trees_number = 100,
      max_depth = 1,
      node_size = 10,
      shrinkage = 0.1,
      sampled_records_proportion = 0.5
    ),
    responses = list(
      y = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL)
    ),
    distribution = "gaussian"
  )
})

test_that("Univariate numeric (tuning)", {
  hyperparams <- list(
    trees_number = c(5, 10),
    max_depth = c(1, 2),
    node_size = c(5, 10),
    shrinkage = 0.1,
    sampled_records_proportion = 0.5
  )

  model <- generalized_boosted_machine(
    x_num,
    y_num,

    trees_number = hyperparams$trees_number,
    max_depth = hyperparams$max_depth,
    node_size = hyperparams$node_size,
    shrinkage = hyperparams$shrinkage,
    sampled_records_proportion = hyperparams$sampled_records_proportion,

    seed = 1,
    verbose = FALSE
  )

  expect_generalized_boosted_machine(
    model = model,
    x = x_num,
    y = y_num,
    hyperparams = hyperparams,
    responses = list(
      y = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL)
    ),
    distribution = "gaussian"
  )
})

test_that("Univariate binary (no tuning)", {
  model <- generalized_boosted_machine(x_bin, y_bin, seed = 1, verbose = FALSE)

  expect_generalized_boosted_machine(
    model = model,
    x = x_bin,
    y = ifelse(y_bin == levels(y_bin)[1], 1, 0),
    hyperparams = list(
      trees_number = 100,
      max_depth = 1,
      node_size = 10,
      shrinkage = 0.1,
      sampled_records_proportion = 0.5
    ),
    responses = list(
      y = list(type = RESPONSE_TYPES$BINARY, levels = levels(y_bin))
    ),
    distribution = "bernoulli"
  )
})

test_that("Univariate binary (tuning)", {
  hyperparams <- list(
    trees_number = c(5, 10),
    max_depth = c(1),
    node_size = c(3, 2),
    shrinkage = c(0.1, 0.2),
    sampled_records_proportion = c(0.5, 0.2)
  )

  model <- generalized_boosted_machine(
    x_bin,
    y_bin,

    trees_number = hyperparams$trees_number,
    max_depth = hyperparams$max_depth,
    node_size = hyperparams$node_size,
    shrinkage = hyperparams$shrinkage,
    sampled_records_proportion = hyperparams$sampled_records_proportion,

    tune_cv_type = "Random",
    tune_folds_number = 2,
    tune_testing_proportion = 0.3,
    tune_grid_proportion = 0.8,

    seed = 1,
    verbose = FALSE
  )

  expect_generalized_boosted_machine(
    model = model,
    x = x_bin,
    y = ifelse(y_bin == levels(y_bin)[1], 1, 0),
    hyperparams = hyperparams,
    responses = list(
      y = list(type = RESPONSE_TYPES$BINARY, levels = levels(y_bin))
    ),
    distribution = "bernoulli",
    tune_grid_proportion = 0.8
  )
})

test_that("Univariate categorical (no tuning)", {
  model <- suppressWarnings(generalized_boosted_machine(
    x_cat,
    y_cat,
    seed = 1,
    verbose = FALSE
  ))

  expect_generalized_boosted_machine(
    model = model,
    x = x_cat,
    y = y_cat,
    hyperparams = list(
      trees_number = 100,
      max_depth = 1,
      node_size = 10,
      shrinkage = 0.1,
      sampled_records_proportion = 0.5
    ),
    responses = list(
      y = list(type = RESPONSE_TYPES$CATEGORICAL, levels = levels(y_cat))
    ),
    distribution = "multinomial"
  )
})

test_that("Univariate categorical (tuning)", {
  hyperparams <- list(
    trees_number = c(5, 10),
    max_depth = c(1),
    node_size = c(5, 10),
    shrinkage = c(0.1),
    sampled_records_proportion = c(0.5)
  )

  model <- suppressWarnings(generalized_boosted_machine(
    x_cat,
    y_cat,

    trees_number = hyperparams$trees_number,
    max_depth = hyperparams$max_depth,
    node_size = hyperparams$node_size,
    shrinkage = hyperparams$shrinkage,
    sampled_records_proportion = hyperparams$sampled_records_proportion,

    tune_cv_type = "K_fold",
    tune_folds_number = 5,
    tune_grid_proportion = 0.5,

    predictors_relationship = sample(c(0, 1, -1), ncol(x_cat), replace = TRUE),
    records_weights = runif(nrow(x_cat)),

    seed = 1,
    verbose = FALSE
  ))

  expect_generalized_boosted_machine(
    model = model,
    x = x_cat,
    y = y_cat,
    hyperparams = hyperparams,
    responses = list(
      y = list(type = RESPONSE_TYPES$CATEGORICAL, levels = levels(y_cat))
    ),
    distribution = "multinomial",
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
  model <- suppressWarnings(generalized_boosted_machine(
    x,
    y,
    seed = 1,
    verbose = FALSE
  ))

  expect_generalized_boosted_machine(
    model = model,
    x = x,
    y = y,
    hyperparams = list(
      trees_number = 100,
      max_depth = 1,
      node_size = 10,
      shrinkage = 0.1,
      sampled_records_proportion = 0.5
    ),
    responses = list(
      y = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL)
    ),
    removed_rows = c(2, 56, 100, 144),
    distribution = "gaussian"
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
    trees_number = c(5, 10),
    max_depth = c(1),
    node_size = c(2),
    shrinkage = c(0.3, 0.5),
    sampled_records_proportion = c(0.5, 1)
  )

  model <- suppressWarnings(generalized_boosted_machine(
    x,
    y,

    trees_number = hyperparams$trees_number,
    max_depth = hyperparams$max_depth,
    node_size = hyperparams$node_size,
    shrinkage = hyperparams$shrinkage,
    sampled_records_proportion = hyperparams$sampled_records_proportion,

    tune_cv_type = "Random",
    tune_folds_number = 3,
    tune_grid_proportion = 0.4,

    seed = 1,
    verbose = FALSE
  ))

  expect_generalized_boosted_machine(
    model = model,
    x = x,
    y = y,
    hyperparams = hyperparams,
    responses = list(
      y = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL)
    ),
    tune_grid_proportion = 0.4,
    removed_rows = c(2, 56, 144, 100),
    distribution = "gaussian"
  )
})
