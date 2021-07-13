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

SVM_KERNELS <- c("linear", "polynomial", "radial", "sigmoid")

RESPONSE_TYPES <- list(
  CONTINUOUS = 1,
  DISCRETE = 2,
  BINARY = 3,
  CATEGORICAL = 4
)
