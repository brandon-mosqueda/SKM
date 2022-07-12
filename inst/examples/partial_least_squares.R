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

# Genomic selection ------------------------------------------------------------
data(Wheat)

# Data preparation of G
Line <- model.matrix(~ 0 + Line, data = Wheat$Pheno)
# Compute cholesky without the first column (Line)
Geno <- cholesky(Wheat$Geno[, -1])
# G matrix
X <- Line %*% Geno
y <- Wheat$Pheno$Y

# Set seed for reproducible results
set.seed(2022)
folds <- cv_kfold(records_number = nrow(X), k = 3)

Predictions <- data.frame()

# Model training and predictions
for (i in seq_along(folds)) {
  cat("*** Fold:", i, "***\n")
  fold <- folds[[i]]

  # Identify the training and testing sets
  X_training <- X[fold$training, ]
  X_testing <- X[fold$testing, ]
  y_training <- y[fold$training]
  y_testing <- y[fold$testing]

  # Model training
  model <- partial_least_squares(
    x = X_training,
    y = y_training,

    scale = TRUE,
    method = "kernel"
  )

  # Prediction of testing set
  predictions <- predict(model, X_testing)

  # Predictions for the i-th fold
  FoldPredictions <- data.frame(
    Fold = i,
    Line = Wheat$Pheno$Line[fold$testing],
    Env = Wheat$Pheno$Env[fold$testing],
    Observed = y_testing,
    Predicted = predictions$predicted
  )
  Predictions <- rbind(Predictions, FoldPredictions)
}

head(Predictions)
# Compute the summary of all predictions
summaries <- gs_summaries(Predictions)

# Summaries by Line
head(summaries$line)

# Summaries by Environment
summaries$env

# Summaries by Fold
summaries$fold
