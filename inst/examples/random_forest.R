# Use all default hyperparameters (no tuning) -------------------------------
x <- to_matrix(iris[, -5])
y <- iris$Species
model <- random_forest(x, y)

# Obtain the variables importance
coef(model)

# Predict using the fitted model
predictions <- predict(model, x)
# Obtain the predicted values
predictions$predicted
# Obtain the predicted probabilities
predictions$probabilities

# Tune with grid search -----------------------------------------------------
x <- to_matrix(iris[, -1])
y <- iris$Sepal.Length
model <- random_forest(
  x,
  y,
  trees_number = c(100, 200, 300),
  node_size = c(1, 2),
  node_depth = c(10, 15),
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
model <- random_forest(
  x,
  y,
  trees_number = list(min = 100, max = 500),
  node_size = list(min = 1, max = 10),
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

# Obtain the variables importance
coef(model)

# Obtain the execution time taken to tune and fit the model
model$execution_time

# Multivariate analysis -----------------------------------------------------
x <- to_matrix(iris[, -c(1, 5)])
y <- iris[, c(1, 5)]
model <- random_forest(x, y, trees_number = 100)

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
