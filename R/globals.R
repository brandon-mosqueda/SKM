# Kernels --------------------------------------------------

CONVENTIONAL_KERNELS <- c(
  "Linear",
  "Polynomial",
  "Exponential",
  "Sigmoid",
  "Gaussian",
  "Arc_cosine"
)

SPARSE_KERNELS <- c(
  "Sparse_Linear",
  "Sparse_Polynomial",
  "Sparse_Exponential",
  "Sparse_Sigmoid",
  "Sparse_Gaussian",
  "Sparse_Arc_cosine"
)

ARC_COSINE_KERNELS <- c("Arc_cosine", "Sparse_Arc_cosine")

KERNELS_WITH_DEGREE <- c("Polynomial", "Sparse_Polynomial")
KERNELS_WITH_GAMMA <- c(
  "Polynomial", "Sparse_Polynomial",
  "Sigmoid", "Sparse_Sigmoid",
  "Gaussian", "Sparse_Gaussian", "radial",
  "Exponential", "Sparse_Exponential"
)
KERNELS_WITH_COEF0 <- c(
  "Polynomial", "Sparse_Polynomial",
  "Sigmoid", "Sparse_Sigmoid"
)

# For models --------------------------------------------------

SVM_KERNELS <- c("linear", "polynomial", "radial", "sigmoid")
SVM_CLASS_WEIGHTS <- c("inverse")

RANDOM_FOREST_SPLIT_RULES <- c("mse", "gini", "auc", "entropy")

RANDOM_FOREST_NA_ACTIONS <- c("omit", "impute")

VALID_ACTIVATION_FUNCTIONS <- c(
  "linear",
  "relu",
  "elu",
  "selu",
  "hard_sigmoid",
  "linear",
  "sigmoid",
  "softmax",
  "softplus",
  "softsign",
  "tanh",
  "exponential"
)

VALID_OPTIMIZERS <- c(
  "adadelta",
  "adagrad",
  "adamax",
  "adam",
  "nadam",
  "rmsprop",
  "sgd"
)

VALID_DEEP_LEARNING_LOSS_FUNCTIONS <- c(
  "binary_crossentropy",
  "categorical_crossentropy",
  "sparse_categorical_crossentropy",
  "poisson",
  "kl_divergence",

  "mean_squared_error",
  "mean_absolute_error",
  "mean_absolute_percentage_error",
  "mean_squared_logarithmic_error",
  "cosine_similarity",
  "huber",
  "log_cosh",

  "hinge",
  "squared_hinge",
  "categorical_hinge"
)

DEFAULT_LAYER_NEURONS <- 50
DEFAULT_LAYER_ACTIVATION <- "relu"
DEFAULT_LAYER_DROPOUT <- 0
DEFAULT_RIDGE_PENALTY <- 0
DEFAULT_LASSO_PENALTY <- 0

NEURONS_PROPORTION_MAX_VALUE <- 10

BAYESIAN_MODELS <- c(
  "FIXED",
  "BGBLUP",
  "BRR",
  "Bayes_Lasso",
  "Bayes_A",
  "Bayes_B",
  "Bayes_C"
)

MULTIVARIATE_BAYESIAN_MODELS <- c("FIXED", "BGBLUP", "BRR")

BAYESIAN_TRASH_DIR <- ".skm_bayesian_temporal_files"

BAYESIAN_COVARIANCE_STRUCTURE_TYPES <- c(
  "Unstructured",
  "Diagonal",
  "Factor_analytic",
  "Recursive"
)

# Tuning --------------------------------------------------

TUNE_CV_TYPES <- c("K_fold", "Random")

GLM_CV_TYPES <- c("K_fold")

TUNE_TYPES <- c("Grid_search", "Bayesian_optimization")

TUNE_NUMERIC_LOSS_FUNCTIONS <- c("mse", "maape", "mae", "nrmse", "rmse")

TUNE_BINARY_LOSS_FUNCTIONS <- c("f1_score", "roc_auc")

TUNE_CATEGORICAL_LOSS_FUNCTIONS <- c("accuracy", "kappa_coeff")

NEED_INVERT_LOSS <- c(
  "f1_score",
  "roc_auc",
  "accuracy",
  "kappa_coeff"
)

# Others --------------------------------------------------

RESPONSE_TYPES <- list(
  CONTINUOUS = "continuous",
  DISCRETE = "discrete",
  BINARY = "binary",
  CATEGORICAL = "categorical"
)
