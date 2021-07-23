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
  "Gaussian", "Sparse_Gaussian",
  "Exponential", "Sparse_Exponential"
)
KERNELS_WITH_COEF0 <- c(
  "Polynomial", "Sparse_Polynomial",
  "Sigmoid", "Sparse_Sigmoid"
)

# For models --------------------------------------------------

SVM_KERNELS <- c("linear", "polynomial", "radial", "sigmoid")

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

DEFAULT_LAYER_NEURONS <- 0.25
DEFAULT_LAYER_ACTIVATION <- "relu"
DEFAULT_LAYER_DROPOUT <- 0
DEFAULT_RIDGE_PENALTY <- 0
DEFAULT_LASSO_PENALTY <- 0

NEURONS_PROPORTION_MAX_VALUE <- 10

BAYESIAN_MODELS <- c(
  "FIXED",
  "RKHS",
  "BGBLUP",
  "BRR",
  "Bayes_Lasso",
  "Bayes_A",
  "Bayes_B",
  "Bayes_C"
)

MULTIVARIATE_BAYESIAN_MODELS <- c("FIXED", "RKHS", "BGBLUP", "BRR")

BAYESIAN_TRASH_DIR <- ".skm_bayesian_temporal_files"

BAYESIAN_COVARIANCE_STRUCTURE_TYPES <- c(
  "Unstructured",
  "Diagonal",
  "Factor_analytic",
  "Recursive"
)

# Cross validation --------------------------------------------------

CV_TYPES <- c("K_fold", "Random")

TUNE_CV_TYPES <- c("K_fold", "Random")

# Others --------------------------------------------------

RESPONSE_TYPES <- list(
  CONTINUOUS = "continuous",
  DISCRETE = "discrete",
  BINARY = "binary",
  CATEGORICAL = "categorical"
)
