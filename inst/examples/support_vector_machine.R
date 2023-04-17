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

# Genomic selection ------------------------------------------------------------
data(Wheat)

# Data preparation of G
Line <- model.matrix(~ 0 + Line, data = Wheat$Pheno)
# Compute cholesky
Geno <- cholesky(Wheat$Geno)
# G matrix
X <- Line %*% Geno
y <- Wheat$Pheno$Y

# Set seed for reproducible results
set.seed(2022)
folds <- cv_random(
  records_number = nrow(X),
  folds_number = 5,
  testing_proportion = 0.3
)

Predictions <- data.frame()
Hyperparams <- data.frame()

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
  model <- support_vector_machine(
    x = X_training,
    y = y_training,

    # Specify the hyperparameters for tunning
    coef0 = list(min = 0.01, max = 2),
    gamma = list(min = 0.0001, max = 1),
    kernel = "polynomial",

    tune_type = "bayesian_optimization",
    tune_bayes_iterations_number = 5,
    tune_bayes_samples_number = 3
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

  # Hyperparams
  HyperparamsFold <- model$hyperparams_grid %>%
    mutate(Fold = i)
  Hyperparams <- rbind(Hyperparams, HyperparamsFold)

  # Best hyperparams of the model
  cat("*** Optimal hyperparameters: ***\n")
  print(model$best_hyperparams)
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

# First rows of Hyperparams
head(Hyperparams)
# Last rows of Hyperparams
tail(Hyperparams)
