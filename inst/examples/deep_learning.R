# Use all default hyperparameters (no tuning) -------------------------------
x <- to_matrix(iris[, -5])
y <- iris$Species
model <- deep_learning(x, y)

# Predict using the fitted model
predictions <- predict(model, x)
# Obtain the predicted values
predictions$predicted
# Obtain the predicted probabilities
predictions$probabilities

# Tune with grid search -----------------------------------------------------
x <- to_matrix(iris[, -1])
y <- iris$Sepal.Length
model <- deep_learning(
  x,
  y,
  epochs_number = c(10, 20),
  learning_rate = c(0.001, 0.01),
  layers = list(
    # First hidden layer
    list(neurons_number = c(10, 20)),
    # Second hidden layer
    list(neurons_number = c(10))
  ),
  tune_type = "grid_search",
  tune_cv_type = "k_fold",
  tune_folds_number = 5
)

# Obtain the whole grid with the loss values
model$hyperparams_grid
# Obtain the hyperparameters combination with the best loss value
model$best_hyperparams

# Predict using the fitted model
predictions <- predict(model, x)
# Obtain the predicted values
predictions$predicted

# Tune with Bayesian optimization -------------------------------------------
x <- to_matrix(iris[, -1])
y <- iris$Sepal.Length
model <- deep_learning(
  x,
  y,
  epochs_number = list(min = 10, max = 50),
  learning_rate = list(min = 0.001, max = 0.5),
  layers = list(
    list(
      neurons_number = list(min = 10, max = 20),
      dropout = list(min = 0, max = 1),
      activation_layer = "sigmoid"
    )
  ),
  tune_type = "bayesian_optimization",
  tune_bayes_samples_number = 5,
  tune_bayes_iterations_number = 5,
  tune_cv_type = "random",
  tune_folds_number = 2
)

# Obtain the whole grid with the loss values
model$hyperparams_grid
# Obtain the hyperparameters combination with the best loss value
model$best_hyperparams

# Predict using the fitted model
predictions <- predict(model, x)
# Obtain the predicted values
predictions$predicted

# Obtain the execution time taken to tune and fit the model
model$execution_time

# Multivariate analysis -----------------------------------------------------
x <- to_matrix(iris[, -c(1, 5)])
y <- iris[, c(1, 5)]
model <- deep_learning(
  x,
  y,
  epochs_number = 10,
  layers = list(
    list(
      neurons_number = 50,
      dropout = 0.5,
      activation = "relu",
      ridge_penalty = 0.5,
      lasso_penalty = 0.5
    )
  ),
  optimizer = "adadelta"
)

# Predict using the fitted model
predictions <- predict(model, x)
# Obtain the predicted values of the first response
predictions$Sepal.Length$predicted
# Obtain the predicted values and probabilities of the second response
predictions$Species$predicted
predictions$Species$probabilities

# Obtain the predictions in a data.frame not in a list
predictions <- predict(model, x, format = "data.frame")
head(predictions)

# With Platt scaling --------------------------------------------------------
x <- to_matrix(iris[, -1])
y <- iris$Sepal.Length
model <- deep_learning(
  x,
  y,
  with_platt_scaling = TRUE,
  platt_proportion = 0.25
)

# Predict using the fitted model
predictions <- predict(model, x)
# Obtain the predicted values
predictions$predicted
