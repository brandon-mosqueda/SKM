suppressMessages(library(e1071))

data(Iris)

expect_tuning <- function(tuner,
                          loss_function,
                          cv_class,
                          folds_number,
                          testing_proportion,
                          hyperparams,
                          grid_proportion = NULL,
                          combinations_number = NULL) {
  expect_class(tuner, "Tuner")
  expect_equal(tuner$loss_function, loss_function)

  expect_class(tuner$cross_validator, cv_class)
  expect_equal(tuner$cross_validator$folds_number, folds_number)
  expect_equal(tuner$cross_validator$testing_proportion, testing_proportion)

  if (is.null(combinations_number)) {
    combinations_number <- ceiling(
      nrow(expand.grid(hyperparams)) * grid_proportion
    )
  }
  expect_equal(tuner$combinations_number, combinations_number)

  expect_data_frame(
    tuner$all_combinations,
    any.missing = combinations_number == 1,
    nrows = combinations_number,
    ncols = length(hyperparams) + 1
  )

  expect_numeric(
    tuner$all_combinations$loss,
    any.missing = combinations_number == 1,
    len = combinations_number,
    sorted = TRUE
  )

  expect_equal(
    tuner$best_combination,
    as.list(tuner$all_combinations[1, ])
  )
}

clone_tuner <- function(tuner,
                        class,
                        x = NULL,
                        y = NULL,
                        responses = NULL,
                        is_multivariate = NULL,
                        training_function = NULL,
                        predict_function = NULL,
                        loss_function = NULL,
                        hyperparams = NULL,
                        fit_params = NULL,
                        cv_type = NULL,
                        folds_number = NULL,
                        testing_proportion = NULL,
                        grid_proportion = NULL,
                        ...) {
  return(class$new(
    x = nonull(x, tuner$x),
    y = nonull(y, tuner$y),
    responses = nonull(responses, tuner$responses),
    is_multivariate = nonull(is_multivariate, tuner$is_multivariate),
    training_function = nonull(training_function, tuner$training_function),
    predict_function = nonull(predict_function, tuner$predict_function),
    loss_function = nonull(loss_function, tuner$loss_function),
    hyperparams = nonull(hyperparams, tuner$hyperparams),
    fit_params = nonull(fit_params, tuner$fit_params),
    cv_type = nonull(cv_type, tuner$cv_type),
    folds_number = nonull(folds_number, tuner$folds_number),
    testing_proportion = nonull(testing_proportion, tuner$testing_proportion),
    grid_proportion = nonull(grid_proportion, tuner$grid_proportion),
    ...
  ))
}

temp_loss <- function(observed, predicted, responses = NULL) {
  x <- sum(observed == predicted)

  return(rnorm(1, x, 2))
}

test_that("GridTuner (univariate)", {
  # Numeric all combinations --------------------------------------------------

  fit_params <- list(
    kernel = "polynomial",
    scale = TRUE,
    class_weights = NULL,
    cache_size = 40,
    tolerance = 0.001,
    epsilon = 0.1,
    shrinking = TRUE,
    fitted = TRUE
  )

  hyperparams <- list(
    degree = c(2, 3),
    gamma = 1 / 5,
    coef0 = c(0, 2),
    cost = 1
  )
  single_hyperparams <- list(
    degree = 3,
    gamma = 1 / 5,
    coef0 = 0,
    cost = 1
  )

  tuner <- GridTuner$new(
    x = to_matrix(x_num),
    y = y_num,
    responses = list(y = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL)),
    is_multivariate = FALSE,
    training_function =
      SupportVectorMachineModel$private_methods$train_univariate,,
    predict_function =
      SupportVectorMachineModel$private_methods$predict_univariate,
    hyperparams = hyperparams,
    fit_params = fit_params,
    cv_type = "K_fold",
    folds_number = 5,
    testing_proportion = NULL,
    grid_proportion = 1
  )

  hush(tuner$tune())

  expect_tuning(
    tuner = tuner,
    loss_function = mse,
    cv_class = "KFoldCV",
    folds_number = 5,
    testing_proportion = NULL,
    hyperparams = hyperparams,
    grid_proportion = 1
  )

  # Numeric half combinations --------------------------------------------------

  tuner <- clone_tuner(
    tuner = tuner,
    class = GridTuner,
    grid_proportion = 0.5
  )

  hush(tuner$tune())

  expect_tuning(
    tuner = tuner,
    loss_function = mse,
    cv_class = "KFoldCV",
    folds_number = 5,
    testing_proportion = NULL,
    hyperparams = hyperparams,
    grid_proportion = 0.5
  )

  # Numeric one combination --------------------------------------------------

  tuner <- clone_tuner(
    tuner = tuner,
    class = GridTuner,
    hyperparams = single_hyperparams,
    loss_function = temp_loss,
    grid_proportion = 0.5
  )

  hush(tuner$tune())

  expect_tuning(
    tuner = tuner,
    loss_function = temp_loss,
    cv_class = "KFoldCV",
    folds_number = 5,
    testing_proportion = NULL,
    hyperparams = single_hyperparams,
    grid_proportion = 0.5
  )

  # Categorical all combinations -----------------------------------------------

  tuner <- clone_tuner(
    tuner = tuner,
    class = GridTuner,
    x = to_matrix(x_cat),
    y = y_cat,
    responses = list(
      y = list(
        type = RESPONSE_TYPES$CATEGORICAL,
        levels = levels(y_cat)
      )
    ),
    loss_function = pcic,
    hyperparams = hyperparams,
    fit_params = fit_params,
    grid_proportion = 1
  )

  hush(tuner$tune())

  expect_tuning(
    tuner = tuner,
    loss_function = pcic,
    cv_class = "KFoldCV",
    folds_number = 5,
    testing_proportion = NULL,
    hyperparams = hyperparams,
    grid_proportion = 1
  )
})

test_that("GridTuner (Multivariate)", {
  # All combinations --------------------------------------------------

  fit_params <- list(
    model_formula = formula("Multivar(y1, y2) ~ ."),
    importance = FALSE,
    splits_number = 10,
    na_action = "na.omit"
  )

  hyperparams <- list(
    trees_number = c(5, 10),
    sampled_x_vars_number = c(1, 2)
  )
  single_hyperparams <- list(
    trees_number = 5,
    sampled_x_vars_number = 2
  )

  tuner <- GridTuner$new(
    x = x_multi_cat,
    y = y_multi_cat,
    responses = list(
      y1 = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL),
      y2 = list(
        type = RESPONSE_TYPES$CATEGORICAL,
        levels = levels(y_multi_cat$y2)
      )
    ),
    is_multivariate = TRUE,
    training_function = train_random_forest,
    predict_function =
      RandomForestModel$private_methods$predict_multivariate,
    hyperparams = hyperparams,
    fit_params = fit_params,
    cv_type = "Random",
    folds_number = 3,
    testing_proportion = 0.2,
    grid_proportion = 1
  )

  hush(tuner$tune())

  expect_tuning(
    tuner = tuner,
    loss_function = multivariate_loss,
    cv_class = "RandomCV",
    folds_number = 3,
    testing_proportion = 0.2,
    hyperparams = hyperparams,
    grid_proportion = 1
  )

  # Half combinations --------------------------------------------------

  tuner <- clone_tuner(
    tuner = tuner,
    class = GridTuner,
    grid_proportion = 0.5
  )

  hush(tuner$tune())

  expect_tuning(
    tuner = tuner,
    loss_function = multivariate_loss,
    cv_class = "RandomCV",
    folds_number = 3,
    testing_proportion = 0.2,
    hyperparams = hyperparams,
    grid_proportion = 0.5
  )

  # One combination --------------------------------------------------

  tuner <- clone_tuner(
    tuner = tuner,
    class = GridTuner,
    hyperparams = single_hyperparams,
    loss_function = temp_loss,
    grid_proportion = 0.5
  )

  hush(tuner$tune())

  expect_tuning(
    tuner = tuner,
    loss_function = temp_loss,
    cv_class = "RandomCV",
    folds_number = 3,
    testing_proportion = 0.2,
    hyperparams = single_hyperparams,
    grid_proportion = 0.5
  )
})

test_that("Grid deep learning tuner (univariate)", {
  # All combinations --------------------------------------------------

  hyperparams <- list(
    neurons_number_1 = 10,
    activation_1 = "relu",
    lasso_penalty_1 = 0.2,
    ridge_penalty_1 = 0.2,
    dropout_1 = 0.1,
    learning_rate= c(0.1, 0.001),
    output_lasso_penalty = 0,
    output_ridge_penalty = 0,
    epochs_number = c(5, 8),
    batch_size = c(32)
  )
  single_hyperparams <- hyperparams
  single_hyperparams$learning_rate <- 0.1
  single_hyperparams$epochs_number <- 5

  responses <- list(
    y = list(
      type = RESPONSE_TYPES$CONTINUOUS,
      levels = NULL,
      last_layer_neurons = 1,
      last_layer_activation = "linear",
      loss_function = "mse",
      metric = "mse"
    )
  )

  tuner <- DeepLearningGridTuner$new(
    x = to_matrix(x_num),
    y = y_num,
    responses = responses,
    is_multivariate = FALSE,
    training_function = DeepLearningModel$private_methods$train_univariate,
    predict_function = invisible,
    loss_function = invisible,
    hyperparams = hyperparams,
    fit_params = list(
      hidden_layers_number = 1,
      shuffle = TRUE,
      responses = responses,
      early_stop = FALSE,
      early_stop_patience = 10,
      optimizer_function = get_keras_optimizer_function("adam")
    ),
    cv_type = "Random",
    folds_number = 2,
    testing_proportion = 0.3,
    grid_proportion = 1
  )

  hush(tuner$tune())

  expect_tuning(
    tuner = tuner,
    loss_function = invisible,
    cv_class = "RandomCV",
    folds_number = 2,
    testing_proportion = 0.3,
    hyperparams = hyperparams,
    grid_proportion = 1
  )

  # Half combinations --------------------------------------------------

  tuner <- clone_tuner(
    tuner = tuner,
    class = DeepLearningGridTuner,
    grid_proportion = 0.5
  )

  hush(tuner$tune())

  expect_tuning(
    tuner = tuner,
    loss_function = invisible,
    cv_class = "RandomCV",
    folds_number = 2,
    testing_proportion = 0.3,
    hyperparams = hyperparams,
    grid_proportion = 0.5
  )

  # One combinations --------------------------------------------------

  tuner <- clone_tuner(
    tuner = tuner,
    class = DeepLearningGridTuner,
    hyperparams = single_hyperparams,
    loss_function = temp_loss,
    grid_proportion = 0.5
  )

  hush(tuner$tune())

  expect_tuning(
    tuner = tuner,
    loss_function = temp_loss,
    cv_class = "RandomCV",
    folds_number = 2,
    testing_proportion = 0.3,
    hyperparams = single_hyperparams,
    grid_proportion = 0.5
  )
})

test_that("Grid Deep learning tuner (multivariate)", {
  # All combinations --------------------------------------------------

  hyperparams <- list(
    neurons_number_1 = 10,
    activation_1 = "relu",
    lasso_penalty_1 = 0.2,
    ridge_penalty_1 = 0.2,
    dropout_1 = 0.1,
    learning_rate= c(0.1, 0.001),
    output_lasso_penalty = 0,
    output_ridge_penalty = 0,
    epochs_number = c(5, 8),
    batch_size = c(32)
  )
  single_hyperparams <- hyperparams
  single_hyperparams$learning_rate <- 0.1
  single_hyperparams$epochs_number <- 5

  responses <- list(
    y1 = list(
      type = RESPONSE_TYPES$CONTINUOUS,
      levels = NULL,
      colnames = "y1",
      last_layer_neurons = 1,
      last_layer_activation = "linear",
      loss_function = "mse",
      metric = "mse"
    ),
    y2 = list(
      type = RESPONSE_TYPES$CATEGORICAL,
      levels = levels(y_multi_cat$y2),
      colnames = c("setosa", "versicolor", "virginica"),
      last_layer_neurons = 3,
      last_layer_activation = "softmax",
      loss_function = "categorical_crossentropy",
      metric = "accuracy"
    )
  )

  y <- y_multi_cat
  y <- cbind(y$y1, to_categorical(as.numeric(y$y2) - 1))
  colnames(y) <- c("y1", "setosa", "versicolor", "virginica")

  tuner <- DeepLearningGridTuner$new(
    x = to_matrix(x_multi_cat),
    y = y,
    responses = responses,
    is_multivariate = TRUE,
    training_function = DeepLearningModel$private_methods$train_multivariate,
    predict_function = invisible,
    loss_function = invisible,
    hyperparams = hyperparams,
    fit_params = list(
      hidden_layers_number = 1,
      shuffle = TRUE,
      responses = responses,
      early_stop = FALSE,
      early_stop_patience = 10,
      optimizer_function = get_keras_optimizer_function("adam")
    ),
    cv_type = "K_fold",
    folds_number = 3,
    testing_proportion = NULL,
    grid_proportion = 1
  )

  hush(tuner$tune())

  expect_tuning(
    tuner = tuner,
    loss_function = invisible,
    cv_class = "KFoldCV",
    folds_number = 3,
    testing_proportion = NULL,
    hyperparams = hyperparams,
    grid_proportion = 1
  )

  # Half combinations --------------------------------------------------

  tuner <- clone_tuner(
    tuner = tuner,
    class = DeepLearningGridTuner,
    grid_proportion = 0.5
  )

  hush(tuner$tune())

  expect_tuning(
    tuner = tuner,
    loss_function = invisible,
    cv_class = "KFoldCV",
    folds_number = 3,
    testing_proportion = NULL,
    hyperparams = hyperparams,
    grid_proportion = 0.5
  )

  # One combination --------------------------------------------------

  tuner <- clone_tuner(
    tuner = tuner,
    class = DeepLearningGridTuner,
    hyperparams = single_hyperparams,
    loss_function = temp_loss,
    grid_proportion = 0.5
  )

  hush(tuner$tune())

  expect_tuning(
    tuner = tuner,
    loss_function = temp_loss,
    cv_class = "KFoldCV",
    folds_number = 3,
    testing_proportion = NULL,
    hyperparams = single_hyperparams,
    grid_proportion = 0.5
  )
})

test_that("BayesianTuner (univariate)", {
  # Numeric all combinations --------------------------------------------------

  fit_params <- list(
    degree = 2,
    kernel = "polynomial",
    scale = TRUE,
    class_weights = NULL,
    cache_size = 40,
    tolerance = 0.001,
    epsilon = 0.1,
    shrinking = TRUE,
    fitted = TRUE
  )

  hyperparams <- list(
    gamma = list(min = 1 / 5, max = 1),
    coef0 = list(min = 0, 2),
    cost = list(min = 0.001, max = 0.5)
  )

  tuner <- BayesianTuner$new(
    x = to_matrix(x_num),
    y = y_num,
    responses = list(y = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL)),
    is_multivariate = FALSE,
    training_function =
      SupportVectorMachineModel$private_methods$train_univariate,,
    predict_function =
      SupportVectorMachineModel$private_methods$predict_univariate,
    hyperparams = hyperparams,
    fit_params = fit_params,
    cv_type = "K_fold",
    folds_number = 5,
    testing_proportion = NULL,
    samples_number = 5,
    iterations_number = 2
  )

  hush(tuner$tune())

  expect_tuning(
    tuner = tuner,
    loss_function = mse,
    cv_class = "KFoldCV",
    folds_number = 5,
    testing_proportion = NULL,
    hyperparams = hyperparams,
    combinations_number = 7
  )

  # Categorical all combinations -----------------------------------------------

  tuner <- clone_tuner(
    tuner = tuner,
    class = BayesianTuner,
    x = to_matrix(x_cat),
    y = y_cat,
    responses = list(
      y = list(
        type = RESPONSE_TYPES$CATEGORICAL,
        levels = levels(y_cat)
      )
    ),
    loss_function = pcic,
    hyperparams = hyperparams,
    fit_params = fit_params,
    grid_proportion = 1,
    samples_number = 5,
    iterations_number = 2
  )

  hush(tuner$tune())

  expect_tuning(
    tuner = tuner,
    loss_function = pcic,
    cv_class = "KFoldCV",
    folds_number = 5,
    testing_proportion = NULL,
    hyperparams = hyperparams,
    grid_proportion = 1,
    combinations_number = 7
  )
})
