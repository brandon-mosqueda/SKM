# Use all default hyperparameters ----------------------------------------------
x <- list(list(x = to_matrix(iris[, -5]), model = "BRR"))
y <- iris$Species
model <- bayesian_model(x, y, testing_indices = c(1:5, 51:55, 101:105))

# Obtain the model's coefficients
coef(model)

# Predict using the fitted model (of the specified testing indices)
predictions <- predict(model)
# Obtain the predicted values
predictions$predicted
# Obtain the predicted probabilities
predictions$probabilities

# Obtain the predict values of custom individuals ------------------------------
x <- list(
  list(x = to_matrix(iris$Species), model = "fixed"),
  list(x = to_matrix(iris[, c(3, 4)]), model = "bayes_a")
)
y <- iris$Sepal.Length
y[c(5, 10, 15, 60, 80, 120, 130)] <- NA
model <- bayesian_model(
  x,
  y,
  iterations_number = 2000,
  burn_in = 500
)

# Predict using the fitted model
predictions <- predict(model)
# Obtain the predicted values
predictions$predicted

# Obtain the Predicted values of custom individuals using the fitted model
predictions <- predict(model, indices = 1:100)
# Obtain the predicted values
predictions$predicted

# Multivariate analysis --------------------------------------------------------
x <- list(list(x = to_matrix(iris[, -c(1, 2)]), model = "fixed"))
y <- iris[, c(1, 2)]
model <- bayesian_model(x, y, iterations_number = 2000)

# Predict using the fitted model
predictions <- predict(model, indices = 1:50)
# Obtain the predicted values of the first response variable
predictions$Sepal.Length$predicted
# Obtain the predicted values of the second response variable
predictions$Sepal.Width$predicted

# Obtain the predictions in a data.frame not in a list
predictions <- predict(model, indices = 1:50, format = "data.frame")
head(predictions)
