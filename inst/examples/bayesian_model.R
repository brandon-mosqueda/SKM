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

# Genomic selection ------------------------------------------------------------
data(Maize)

# Data preparation of G
Line <- model.matrix(~ 0 + Line, data = Maize$Pheno)
Env <- model.matrix(~ 0 + Env, data = Maize$Pheno)
# Compute cholesky
Geno <- cholesky(Maize$Geno)
# G matrix
LineGeno <- Line %*% Geno

# Identify the model
X <- list(
  Env = list(x = Env, model = "FIXED"),
  LinexGeno = list(x = LineGeno, model = "BRR")
)
y <- Maize$Pheno$Y

# Set seed for reproducible results
set.seed(2022)
folds <- cv_kfold(records_number = nrow(LineGeno), k = 5)

Predictions <- data.frame()

# Model training and predictions
for (i in seq_along(folds)) {
  cat("*** Fold:", i, "***\n")
  fold <- folds[[i]]

  # Model training
  model <- bayesian_model(
    x = X,
    y = y,
    testing_indices = fold$testing,
    iterations_number = 1000,
    burn_in = 500
  )

  # Prediction of testing set
  predictions <- predict(model)

  # Predictions for the i-th fold
  FoldPredictions <- data.frame(
    Fold = i,
    Line = Maize$Pheno$Line[fold$testing],
    Env = Maize$Pheno$Env[fold$testing],
    Observed = y[fold$testing],
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
