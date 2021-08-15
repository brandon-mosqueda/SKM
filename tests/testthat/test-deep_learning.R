data(Iris)

test_that("Univariate numeric (no tuning)", {
  model <- deep_learning(
    x_num,
    y_num,
    epochs_number = 5,
    verbose = FALSE
  )

  expect_deep_learning(
    model = model,
    x = to_matrix(x_num),
    y = y_num,
    hyperparams = list(
      learning_rate = 0.001,
      epochs_number = 5,
      batch_size = 32,
      neurons_number_1 = 0.5,
      activation_1 = "relu",
      dropout_1 = 0,
      ridge_penalty_1 = 0,
      lasso_penalty_1 = 0,
      output_ridge_penalty = 0,
      output_lasso_penalty = 0
    ),
    responses = list(
      y = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL)
    )
  )
})

test_that("Univariate numeric (tuning)", {
  hyperparams <- list(
    learning_rate = 0.001,
    epochs_number = c(5, 8),
    batch_size = 32,
    neurons_number_1 = c(5, 12, 3, 1.5),
    activation_1 = "relu",
    dropout_1 = 0,
    ridge_penalty_1 = 0,
    lasso_penalty_1 = 0,
    output_ridge_penalty = 0,
    output_lasso_penalty = 0
  )

  model <- deep_learning(
    to_matrix(x_num),
    y_num,

    learning_rate = hyperparams$learning_rate,
    epochs_number = hyperparams$epochs_number,
    batch_size = hyperparams$batch_size,
    layers = list(
      list(
        neurons_number = hyperparams$neurons_number_1[1:2],
        neurons_proportion = hyperparams$neurons_number_1[3:4],
        activation = hyperparams$activation_1,
        dropout = hyperparams$dropout_1,
        ridge_penalty = hyperparams$ridge_penalty_1,
        lasso_penalty = hyperparams$lasso_penalty_1
      )
    ),
    output_penalties = list(
      ridge_penalty = hyperparams$output_ridge_penalty,
      lasso_penalty = hyperparams$output_lasso_penalty
    ),

    tune_folds_number = 3,

    verbose = FALSE
  )

  expect_deep_learning(
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
  model <- deep_learning(x_bin, y_bin, epochs_number = 5, verbose = FALSE)

  expect_deep_learning(
    model = model,
    x = to_matrix(x_bin),
    y = as.numeric(y_bin) - 1,
    hyperparams = list(
      learning_rate = 0.001,
      epochs_number = 5,
      batch_size = 32,
      neurons_number_1 = 0.5,
      activation_1 = "relu",
      dropout_1 = 0,
      ridge_penalty_1 = 0,
      lasso_penalty_1 = 0,
      output_ridge_penalty = 0,
      output_lasso_penalty = 0
    ),
    responses = list(
      y = list(type = RESPONSE_TYPES$BINARY, levels = levels(y_bin))
    )
  )
})

test_that("Univariate binary (tuning)", {
  hyperparams <- list(
    learning_rate = c(0.001, 0.1),
    epochs_number = c(5),
    batch_size = 32,
    neurons_number_1 = c(1, 2),
    activation_1 = c("relu", "sigmoid"),
    dropout_1 = 0,
    ridge_penalty_1 = c(0.1, 0.2),
    lasso_penalty_1 = 0,
    output_ridge_penalty = 0,
    output_lasso_penalty = 0
  )

  model <- deep_learning(
    x_bin,
    y_bin,

    learning_rate = hyperparams$learning_rate,
    epochs_number = hyperparams$epochs_number,
    batch_size = hyperparams$batch_size,
    layers = list(
      list(
        neurons_proportion = hyperparams$neurons_number_1,
        activation = hyperparams$activation_1,
        dropout = hyperparams$dropout_1,
        ridge_penalty = hyperparams$ridge_penalty_1,
        lasso_penalty = hyperparams$lasso_penalty_1
      )
    ),
    output_penalties = list(
      ridge_penalty = hyperparams$output_ridge_penalty,
      lasso_penalty = hyperparams$output_lasso_penalty
    ),

    tune_cv_type = "Random",
    tune_folds_number = 2,
    tune_testing_proportion = 0.3,
    tune_grid_proportion = 0.8,

    verbose = FALSE
  )

  expect_deep_learning(
    model = model,
    x = to_matrix(x_bin),
    y = as.numeric(y_bin) - 1,
    hyperparams = hyperparams,
    responses = list(
      y = list(type = RESPONSE_TYPES$BINARY, levels = levels(y_bin))
    ),
    tune_grid_proportion = 0.8
  )
})

test_that("Univariate categorical (no tuning)", {
  model <- deep_learning(x_cat, y_cat, epochs_number = 5, verbose = FALSE)

  y <- to_categorical(as.numeric(y_cat) - 1)
  colnames(y) <- levels(y_cat)

  expect_deep_learning(
    model = model,
    x = to_matrix(x_cat),
    y = y,
    hyperparams = list(
      learning_rate = 0.001,
      epochs_number = 5,
      batch_size = 32,
      neurons_number_1 = 0.5,
      activation_1 = "relu",
      dropout_1 = 0,
      ridge_penalty_1 = 0,
      lasso_penalty_1 = 0,
      output_ridge_penalty = 0,
      output_lasso_penalty = 0
    ),
    responses = list(
      y = list(type = RESPONSE_TYPES$CATEGORICAL, levels = levels(y_cat))
    )
  )
})

test_that("Univariate categorical (tuning)", {
  hyperparams <- list(
    learning_rate = c(0.001, 0.1),
    epochs_number = c(5),
    batch_size = 32,
    neurons_number_1 = c(0.5),
    activation_1 = c("relu"),
    dropout_1 = c(0.2),
    ridge_penalty_1 = c(0.1, 0.2),
    lasso_penalty_1 = 0,
    output_ridge_penalty = 0,
    output_lasso_penalty = c(1, 0)
  )

  model <- deep_learning(
    to_matrix(x_cat),
    y_cat,
    learning_rate = hyperparams$learning_rate,
    epochs_number = hyperparams$epochs_number,
    batch_size = hyperparams$batch_size,
    layers = list(
      list(
        neurons_number = hyperparams$neurons_number_1,
        activation = hyperparams$activation_1,
        dropout = hyperparams$dropout_1,
        ridge_penalty = hyperparams$ridge_penalty_1,
        lasso_penalty = hyperparams$lasso_penalty_1
      )
    ),
    output_penalties = list(
      ridge_penalty = hyperparams$output_ridge_penalty,
      lasso_penalty = hyperparams$output_lasso_penalty
    ),

    tune_cv_type = "K_fold",
    tune_folds_number = 5,
    tune_grid_proportion = 0.5,

    shuffle = FALSE,
    early_stop = TRUE,
    early_stop_patience = 2,

    verbose = FALSE
  )

  y <- to_categorical(as.numeric(y_cat) - 1)
  colnames(y) <- levels(y_cat)

  expect_deep_learning(
    model = model,
    x = to_matrix(x_cat),
    y = y,
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
  model <- suppressWarnings(deep_learning(
    x,
    y,
    epochs_number = 5,
    verbose = FALSE
  ))

  expect_deep_learning(
    model = model,
    x = to_matrix(x),
    y = y,
    hyperparams = list(
      learning_rate = 0.001,
      epochs_number = 5,
      batch_size = 32,
      neurons_number_1 = 0.5,
      activation_1 = "relu",
      dropout_1 = 0,
      ridge_penalty_1 = 0,
      lasso_penalty_1 = 0,
      output_ridge_penalty = 0,
      output_lasso_penalty = 0
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
  y[2] <- NA

  hyperparams <- list(
    learning_rate = c(0.001, 0.1),
    epochs_number = c(5),
    batch_size = c(32, 50),
    neurons_number_1 = c(0.5),
    activation_1 = c("relu", "selu"),
    dropout_1 = c(0.2),
    ridge_penalty_1 = c(0.1, 0.2),
    lasso_penalty_1 = 0,
    output_ridge_penalty = 0,
    output_lasso_penalty = c(1, 0)
  )

  model <- suppressWarnings(deep_learning(
    x,
    y,

    learning_rate = hyperparams$learning_rate,
    epochs_number = hyperparams$epochs_number,
    batch_size = hyperparams$batch_size,
    layers = list(
      list(
        neurons_number = hyperparams$neurons_number_1,
        activation = hyperparams$activation_1,
        dropout = hyperparams$dropout_1,
        ridge_penalty = hyperparams$ridge_penalty_1,
        lasso_penalty = hyperparams$lasso_penalty_1
      )
    ),
    output_penalties = list(
      ridge_penalty = hyperparams$output_ridge_penalty,
      lasso_penalty = hyperparams$output_lasso_penalty
    ),

    tune_cv_type = "Random",
    tune_folds_number = 3,
    tune_grid_proportion = 0.4,

    verbose = FALSE
  ))

  expect_deep_learning(
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

test_that("Multivariate numeric (no tuning)", {
  model <- deep_learning(x_multi, y_multi, epochs_number = 5, verbose = FALSE)

  y <- to_matrix(y_multi)
  rownames(y) <- NULL

  expect_deep_learning(
    model = model,
    x = to_matrix(x_multi),
    y = y,
    hyperparams = list(
      learning_rate = 0.001,
      epochs_number = 5,
      batch_size = 32,
      neurons_number_1 = 0.5,
      activation_1 = "relu",
      dropout_1 = 0,
      ridge_penalty_1 = 0,
      lasso_penalty_1 = 0,
      output_ridge_penalty = 0,
      output_lasso_penalty = 0
    ),
    responses = list(
      y1 = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL),
      y2 = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL)
    ),
    is_multivariate = TRUE
  )
})

test_that("Multivariate numeric (tuning)", {
  hyperparams <- list(
    learning_rate = c(0.1),
    epochs_number = c(5),
    batch_size = c(32, 50),
    neurons_number_1 = c(0.5, 2, 3),
    activation_1 = c("relu", "selu"),
    dropout_1 = c(0.2),
    ridge_penalty_1 = c(0.1, 0.2),
    lasso_penalty_1 = 0,
    output_ridge_penalty = 0,
    output_lasso_penalty = c(1, 0)
  )

  model <- deep_learning(
    x_multi,
    y_multi,

    learning_rate = hyperparams$learning_rate,
    epochs_number = hyperparams$epochs_number,
    batch_size = hyperparams$batch_size,
    layers = list(
      list(
        neurons_number = hyperparams$neurons_number_1[1],
        neurons_proportion = hyperparams$neurons_number_1[2:3],
        activation = hyperparams$activation_1,
        dropout = hyperparams$dropout_1,
        ridge_penalty = hyperparams$ridge_penalty_1,
        lasso_penalty = hyperparams$lasso_penalty_1
      )
    ),
    output_penalties = list(
      ridge_penalty = hyperparams$output_ridge_penalty,
      lasso_penalty = hyperparams$output_lasso_penalty
    ),

    tune_cv_type = "random",
    tune_folds_number = 2,

    verbose = FALSE
  )

  y <- to_matrix(y_multi)
  rownames(y) <- NULL

  expect_deep_learning(
    model = model,
    x = to_matrix(x_multi),
    y = y,
    hyperparams = hyperparams,
    responses = list(
      y1 = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL),
      y2 = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL)
    ),
    is_multivariate = TRUE
  )
})

test_that("Multivariate combined (no tuning)", {
  model <- deep_learning(
    x_multi_cat,
    y_multi_cat,
    epochs_number = 5,
    verbose = FALSE
  )

  y <- cbind(y_multi_cat$y1, to_categorical(as.numeric(y_multi_cat$y2) - 1))
  colnames(y) <- c("y1", "y2.setosa", "y2.versicolor", "y2.virginica")

  expect_deep_learning(
    model = model,
    x = to_matrix(x_multi_cat),
    y = y,
    hyperparams = list(
      learning_rate = 0.001,
      epochs_number = 5,
      batch_size = 32,
      neurons_number_1 = 0.5,
      activation_1 = "relu",
      dropout_1 = 0,
      ridge_penalty_1 = 0,
      lasso_penalty_1 = 0,
      output_ridge_penalty = 0,
      output_lasso_penalty = 0
    ),
    responses = list(
      y1 = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL),
      y2 = list(
        type = RESPONSE_TYPES$CATEGORICAL,
        levels = levels(y_multi_cat$y2)
      )
    ),
    is_multivariate = TRUE
  )
})

test_that("Multivariate combined (tuning)", {
  hyperparams <- list(
    learning_rate = c(0.1),
    epochs_number = c(5),
    batch_size = c(32, 50),
    neurons_number_1 = c(0.5, 1),
    activation_1 = c("relu", "selu"),
    dropout_1 = c(0.2),
    ridge_penalty_1 = c(0.1, 0.2),
    lasso_penalty_1 = 0,
    output_ridge_penalty = 0,
    output_lasso_penalty = c(1, 0)
  )

  model <- deep_learning(
    x_multi_cat,
    y_multi_cat,

    learning_rate = hyperparams$learning_rate,
    epochs_number = hyperparams$epochs_number,
    batch_size = hyperparams$batch_size,
    layers = list(
      list(
        neurons_proportion = hyperparams$neurons_number_1,
        activation = hyperparams$activation_1,
        dropout = hyperparams$dropout_1,
        ridge_penalty = hyperparams$ridge_penalty_1,
        lasso_penalty = hyperparams$lasso_penalty_1
      )
    ),
    output_penalties = list(
      ridge_penalty = hyperparams$output_ridge_penalty,
      lasso_penalty = hyperparams$output_lasso_penalty
    ),

    tune_cv_type = "random",
    tune_folds_number = 2,
    tune_grid_proportion = 0.3,

    verbose = FALSE
  )

  y <- cbind(y_multi_cat$y1, to_categorical(as.numeric(y_multi_cat$y2) - 1))
  colnames(y) <- c("y1", "y2.setosa", "y2.versicolor", "y2.virginica")

  expect_deep_learning(
    model = model,
    x = to_matrix(x_multi_cat),
    y = y,
    hyperparams = hyperparams,
    responses = list(
      y1 = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL),
      y2 = list(
        type = RESPONSE_TYPES$CATEGORICAL,
        levels = levels(y_multi_cat$y2)
      )
    ),
    tune_grid_proportion = 0.3,
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
  model <- suppressWarnings(deep_learning(
    x,
    y,
    epochs_number = 5,
    verbose = FALSE
  ))

  y <- to_matrix(y)
  rownames(y) <- NULL

  expect_deep_learning(
    model = model,
    x = to_matrix(x),
    y = y,
    hyperparams = list(
      learning_rate = 0.001,
      epochs_number = 5,
      batch_size = 32,
      neurons_number_1 = 0.5,
      activation_1 = "relu",
      dropout_1 = 0,
      ridge_penalty_1 = 0,
      lasso_penalty_1 = 0,
      output_ridge_penalty = 0,
      output_lasso_penalty = 0
    ),
    responses = list(
      y1 = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL),
      y2 = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL)
    ),
    is_multivariate = TRUE,
    removed_rows = c(5, 10, 50, 44, 22)
  )
})

test_that("Multivariate numeric (NA tuning)", {
  x <- to_matrix(x_multi)
  y <- y_multi
  x[5, 1] <- NA
  x[10, 2] <- NA
  x[50, 3] <- NA
  y[44, 1] <- NA
  y[22, 1] <- NA
  x <- cbind(1, 2, x)

  hyperparams <- list(
    learning_rate = c(0.1),
    epochs_number = c(3, 4),
    batch_size = c(32),
    neurons_number_1 = c(0.5),
    activation_1 = c("relu"),
    dropout_1 = c(0.2, 0.25),
    ridge_penalty_1 = c(0),
    lasso_penalty_1 = 0,
    output_ridge_penalty = 0,
    output_lasso_penalty = c(1, 0)
  )

  model <- suppressWarnings(deep_learning(
    x,
    y,

    learning_rate = hyperparams$learning_rate,
    epochs_number = hyperparams$epochs_number,
    batch_size = hyperparams$batch_size,
    layers = list(
      list(
        neurons_number = hyperparams$neurons_number_1,
        activation = hyperparams$activation_1,
        dropout = hyperparams$dropout_1,
        ridge_penalty = hyperparams$ridge_penalty_1,
        lasso_penalty = hyperparams$lasso_penalty_1
      )
    ),
    output_penalties = list(
      ridge_penalty = hyperparams$output_ridge_penalty,
      lasso_penalty = hyperparams$output_lasso_penalty
    ),

    tune_cv_type = "k_fold",
    tune_folds_number = 3,

    verbose = FALSE
  ))

  y <- to_matrix(y)
  rownames(y) <- NULL

  expect_deep_learning(
    model = model,
    x = to_matrix(x),
    y = y,
    hyperparams = hyperparams,
    responses = list(
      y1 = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL),
      y2 = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL)
    ),
    is_multivariate = TRUE,
    removed_rows = c(5, 10, 50, 44, 22),
    removed_x_cols = c(1, 2)
  )
})

test_that("Numeric platt (no tuning)", {
  model <- deep_learning(
    x_num,
    y_num,
    epochs_number = 5,

    with_platt_scaling = TRUE,

    verbose = FALSE
  )

  expect_deep_learning(
    model = model,
    x = to_matrix(x_num),
    y = y_num,
    hyperparams = list(
      learning_rate = 0.001,
      epochs_number = 5,
      batch_size = 32,
      neurons_number_1 = 0.5,
      activation_1 = "relu",
      dropout_1 = 0,
      ridge_penalty_1 = 0,
      lasso_penalty_1 = 0,
      output_ridge_penalty = 0,
      output_lasso_penalty = 0
    ),
    responses = list(
      y = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL)
    ),
    with_platt_scaling = TRUE
  )
})

test_that("Numeric platt (tuning)", {
  hyperparams <- list(
    learning_rate = 0.001,
    epochs_number = c(5, 8),
    batch_size = 32,
    neurons_number_1 = c(0.5, 12),
    activation_1 = "relu",
    dropout_1 = 0,
    ridge_penalty_1 = 0,
    lasso_penalty_1 = 0,
    output_ridge_penalty = 0,
    output_lasso_penalty = 0
  )

  model <- deep_learning(
    to_matrix(x_num),
    y_num,

    learning_rate = hyperparams$learning_rate,
    epochs_number = hyperparams$epochs_number,
    batch_size = hyperparams$batch_size,
    layers = list(
      list(
        neurons_number = hyperparams$neurons_number_1,
        activation = hyperparams$activation_1,
        dropout = hyperparams$dropout_1,
        ridge_penalty = hyperparams$ridge_penalty_1,
        lasso_penalty = hyperparams$lasso_penalty_1
      )
    ),
    output_penalties = list(
      ridge_penalty = hyperparams$output_ridge_penalty,
      lasso_penalty = hyperparams$output_lasso_penalty
    ),

    tune_folds_number = 3,

    with_platt_scaling = TRUE,
    platt_proportion = 0.5,

    verbose = FALSE
  )

  expect_deep_learning(
    model = model,
    x = to_matrix(x_num),
    y = y_num,
    hyperparams = hyperparams,
    responses = list(
      y = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL)
    ),
    with_platt_scaling = TRUE
  )
})

test_that("Binary platt (no tuning)", {
  model <- suppressWarnings(deep_learning(
    x_bin,
    y_bin,
    epochs_number = 5,
    with_platt_scaling = TRUE,
    verbose = FALSE
  ))

  expect_deep_learning(
    model = model,
    x = to_matrix(x_bin),
    y = as.numeric(y_bin) - 1,
    hyperparams = list(
      learning_rate = 0.001,
      epochs_number = 5,
      batch_size = 32,
      neurons_number_1 = 0.5,
      activation_1 = "relu",
      dropout_1 = 0,
      ridge_penalty_1 = 0,
      lasso_penalty_1 = 0,
      output_ridge_penalty = 0,
      output_lasso_penalty = 0
    ),
    responses = list(
      y = list(type = RESPONSE_TYPES$BINARY, levels = levels(y_bin))
    ),
    with_platt_scaling = TRUE
  )
})

test_that("Binary platt (tuning)", {
  hyperparams <- list(
    learning_rate = c(0.001, 0.1),
    epochs_number = c(5),
    batch_size = 32,
    neurons_number_1 = c(0.5),
    activation_1 = c("relu", "sigmoid"),
    dropout_1 = 0,
    ridge_penalty_1 = c(0.1, 0.2),
    lasso_penalty_1 = 0,
    output_ridge_penalty = 0,
    output_lasso_penalty = 0
  )

  model <- suppressWarnings(deep_learning(
    x_bin,
    y_bin,

    learning_rate = hyperparams$learning_rate,
    epochs_number = hyperparams$epochs_number,
    batch_size = hyperparams$batch_size,
    layers = list(
      list(
        neurons_number = hyperparams$neurons_number_1,
        activation = hyperparams$activation_1,
        dropout = hyperparams$dropout_1,
        ridge_penalty = hyperparams$ridge_penalty_1,
        lasso_penalty = hyperparams$lasso_penalty_1
      )
    ),
    output_penalties = list(
      ridge_penalty = hyperparams$output_ridge_penalty,
      lasso_penalty = hyperparams$output_lasso_penalty
    ),

    tune_cv_type = "Random",
    tune_folds_number = 2,
    tune_testing_proportion = 0.3,
    tune_grid_proportion = 0.8,

    with_platt_scaling = TRUE,
    platt_proportion = 0.4,

    verbose = FALSE
  ))

  expect_deep_learning(
    model = model,
    x = to_matrix(x_bin),
    y = as.numeric(y_bin) - 1,
    hyperparams = hyperparams,
    responses = list(
      y = list(type = RESPONSE_TYPES$BINARY, levels = levels(y_bin))
    ),
    tune_grid_proportion = 0.8,
    with_platt_scaling = TRUE
  )
})
