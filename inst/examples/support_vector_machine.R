# Use all default hyperparameters (no tuning) ------------------------------
x <- to_matrix(iris[, -5])
y <- iris$Species
model <- support_vector_machine(x, y)

# Predict using the fitted model
predictions <- predict(model, x)
# Obtain the predicted values
predictions$predicted
# Obtain the predicted probabilities
predictions$probabilities

# Tune with grid search -----------------------------------------------------
x <- to_matrix(iris[, -1])
y <- iris$Sepal.Length
model <- support_vector_machine(
  x,
  y,
  kernel = "polynomial",
  degree = c(1, 2),
  gamma = c(0.1, 0.3),
  coef0 = c(0, 1),
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
model <- support_vector_machine(
  x,
  y,
  kernel = "sigmoid",
  gamma = list(min = 0.01, max = 0.9),
  coef0 = list(min = 0, max = 5),
  tune_type = "bayesian_optimization",
  tune_bayes_samples_number = 5,
  tune_bayes_iterations_number = 5,
  tune_cv_type = "random",
  tune_folds_number = 4
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
