# Use all default hyperparameters -------------------------------------------
x <- to_matrix(iris[, -1])
y <- iris$Sepal.Length
model <- partial_least_squares(x, y)

# Obtain the optimal number of components to use with predict
model$optimal_components_num

# Obtain the model's coefficients
coef(model)

# Predict using the fitted model
predictions <- predict(model, x)
# Obtain the predicted values
predictions$predicted

# Predict with a non optimal number of components ---------------------------
x <- to_matrix(iris[, -1])
y <- iris$Sepal.Length
model <- partial_least_squares(x, y, method = "orthogonal")

# Obtain the optimal number of components to use with predict
model$optimal_components_num

# Predict using the fitted model with the optimal number of components
predictions <- predict(model, x)
# Obtain the predicted values
predictions$predicted

# Predict using the fitted model without the optimal number of components
predictions <- predict(model, x, components_num = 2)
# Obtain the predicted values
predictions$predicted

# Obtain the model's coefficients
coef(model)

# Obtain the execution time taken to tune and fit the model
model$execution_time

# Multivariate analysis -----------------------------------------------------
x <- to_matrix(iris[, -c(1, 2)])
y <- iris[, c(1, 2)]
model <- partial_least_squares(x, y, method = "wide_kernel")

# Predict using the fitted model
predictions <- predict(model, x)
# Obtain the predicted values of the first response variable
predictions$Sepal.Length$predicted
# Obtain the predicted values of the second response variable
predictions$Sepal.Width$predicted

# Obtain the predictions in a data.frame not in a list
predictions <- predict(model, x, format = "data.frame")
head(predictions)
