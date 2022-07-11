# Use all default hyperparameters (no tuning) -------------------------------
x <- to_matrix(iris[, -5])
y <- iris$Species
model <- generalized_linear_model(x, y)

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
model <- generalized_linear_model(
  x,
  y,
  alpha = c(0, 0.3, 0.6, 1),
  tune_type = "grid_search",
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
model <- generalized_linear_model(
  x,
  y,
  alpha = list(min = 0, max = 1),
  tune_type = "bayesian_optimization",
  tune_bayes_samples_number = 5,
  tune_bayes_iterations_number = 5
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
x <- to_matrix(iris[, -c(1, 2)])
y <- iris[, c(1, 2)]
model <- generalized_linear_model(x, y, alpha = 1)

# Predict using the fitted model
predictions <- predict(model, x)
# Obtain the predicted values of the first response variable
predictions$Sepal.Length$predicted
# Obtain the predicted values of the second response variable
predictions$Sepal.Width$predicted

# Obtain the predictions in a data.frame not in a list
predictions <- predict(model, x, format = "data.frame")
head(predictions)
