setwd("~/data_science/SKM")

roxygen2::roxygenise()

data(Maize)

# Data preparation of G
Line <- model.matrix(~ 0 + Line, data = Maize$Pheno)
LineGeno <- Line %*% Maize$Geno %*% t(Line)
Env <- model.matrix(~ 0 + Env, data = Maize$Pheno)
KEnv <- Env %*% t(Env) / ncol(Env)

# Identify the model
X <- list(
  Env = list(x = KEnv),
  LinexGeno = list(x = LineGeno)
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
  model <- mixed_model(
    x = X,
    y = y,
    testing_indices = fold$testing
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
