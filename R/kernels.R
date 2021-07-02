#' @include utils.R
#' @include globals.R

# Helpers --------------------------------------------------
l2norm <- function(x) sqrt(sum(x^2))

diagAK_f <- function(dAK1) {
  AKAK <- dAK1^2
  costheta <- dAK1 * AKAK^(-1 / 2)
  costheta[which(costheta > 1, arr.ind = TRUE)] <- 1
  theta <- acos(costheta)
  AKl <- (1 / pi) * (AKAK^(1 / 2)) * (sin(theta) + (pi - theta) * cos(theta))
  AKl <- AKl / median(AKl)

  return(AKl)
}

is_sparse_kernel <- function(kernel) {
  return(tolower(kernel) %in% tolower(SPARSE_KERNELS))
}

is_conventional_kernel <- function(kernel) {
  return(tolower(kernel) %in% tolower(CONVENTIONAL_KERNELS))
}

# Conventional kernels --------------------------------------------------
linear_kernel <- function(x, divisor = 1) {
  return(tcrossprod(x) / divisor)
}

radial_kernel <- function(x1, x2 = x1, gamma = 1) {
  return(exp(-gamma *
    outer(
      1:nrow(x1 <- as.matrix(x1)),
      1:ncol(x2 <- t(x2)),
      Vectorize(function(i, j) l2norm(x1[i, ] - x2[, j])^2)
    )
  ))
}

polynomial_kernel <- function(x1, x2 = x1, gamma = 1, coef0 = 0, degree = 3) {
  return((gamma * (as.matrix(x1) %*% t(x2)) + coef0)^degree)
}

sigmoid_kernel <- function(x1, x2 = x1, gamma = 1, coef0 = 0) {
  return(tanh(gamma * (as.matrix(x1) %*% t(x2)) + coef0))
}

exponential_kernel <- function(x1, x2 = x1, gamma = 1) {
  return(exp(-gamma *
    outer(
      1:nrow(x1 <- as.matrix(x1)),
      1:ncol(x2 <- t(x2)),
      Vectorize(function(i, j) l2norm(x1[i, ] - x2[, j]))
    )
  ))
}

arc_cosine_kernel <- function(x1, x2) {
  n1 <- nrow(x1)
  n2 <- nrow(x2)
  x1tx2 <- x1 %*% t(x2)

  norm1 <- sqrt(apply(x1, 1, function(x) crossprod(x)))
  norm2 <- sqrt(apply(x2, 1, function(x) crossprod(x)))

  costheta <- diag(1 / norm1) %*% x1tx2 %*% diag(1 / norm2)
  costheta[which(abs(costheta) > 1, arr.ind = TRUE)] <- 1
  theta <- acos(costheta)

  normx1x2 <- norm1 %*% t(norm2)

  J <- (sin(theta) + (pi - theta) * cos(theta))
  AK1 <- 1 / pi * normx1x2 * J
  AK1 <- AK1 / median(AK1)

  colnames(AK1) <- rownames(x2)
  rownames(AK1) <- rownames(x1)

  return(AK1)
}

arc_cosine_layers <- function(AK1, dAK1, nl) {
  n1 <- nrow(AK1)
  n2 <- ncol(AK1)
  AKl1 <- AK1

  for (l in 1:nl) {
    AKAK <- tcrossprod(dAK1, diag(AKl1))

    costheta <- AKl1 * (AKAK^(-1 / 2))
    costheta[which(costheta > 1, arr.ind = TRUE)] <- 1
    theta <- acos(costheta)

    AKl <- (1 / pi) * (AKAK^(1 / 2)) * (sin(theta) + (pi - theta) * cos(theta))
    dAKl <- diagAK_f(dAK1)
    AKl1 <- AKl
    dAK1 <- dAKl
  }

  AKl <- AKl / median(AKl)

  rownames(AKl) <- rownames(AK1)
  colnames(AKl) <- colnames(AK1)

  return(AKl)
}

# Wrapper kernels functions --------------------------------------------------
conventional_kernel <- function(x,
                                kernel = "linear",
                                arc_cosine_deep = 1,
                                gamma = 1,
                                coef0 = 0,
                                degree = 3) {
  kernel <- tolower(kernel)

  if (kernel == "linear") {
    x <- linear_kernel(x)
  } else if (kernel == "polynomial") {
    x <- polynomial_kernel(
      x1 = x,
      x2 = x,
      gamma = gamma,
      coef0 = coef0,
      degree = degree
    )
  } else if (kernel == "sigmoid") {
    x <- sigmoid_kernel(x1 = x, x2 = x, gamma = gamma, coef0 = coef0)
  } else if (kernel == "gaussian") {
    x <- radial_kernel(x1 = x, x2 = x, gamma = gamma)
  } else if (kernel == "exponential") {
    x <- exponential_kernel(x1 = x, x2 = x, gamma = gamma)
  } else if (kernel == "arc_cosine") {
    x <- arc_cosine_kernel(x1 = x, x2 = x)

    if (arc_cosine_deep > 1) {
      x <- arc_cosine_layers(AK1 = x, dAK1 = diag(x), nl = arc_cosine_deep)
    }
  } else {
    stop(kernel, " is not a valid kernel")
  }

  return(x)
}

sparse_kernel <- function(x,
                          rows_proportion = 1,
                          kernel = "linear",
                          arc_cosine_deep = 1,
                          gamma = 1 / ncol(x),
                          coef0 = 0,
                          degree = 3) {
  kernel <- tolower(kernel)
  kernel <- gsub("sparse_", "", kernel)
  final_rows <- sample(1:nrow(x), nrow(x) * rows_proportion)

  # Step 1 compute K_m
  x_m <- x[final_rows, ]

  if (kernel == "linear") {
    K_m <- x_m %*% t(x_m) / gamma
    K_n_m <- x %*% t(x_m) / gamma
  } else if (kernel == "polynomial") {
    K_m <- polynomial_kernel(
      x1 = x_m,
      x2 = x_m,
      gamma = gamma,
      coef0 = coef0,
      degree = degree
    )
    K_n_m <- polynomial_kernel(
      x1 = x,
      x2 = x_m,
      gamma = gamma,
      coef0 = coef0,
      degree = degree
    )
  } else if (kernel == "sigmoid") {
    K_m <- sigmoid_kernel(x1 = x_m, x2 = x_m, gamma = gamma, coef0 = coef0)
    K_n_m <- sigmoid_kernel(x1 = x, x2 = x_m, gamma = gamma, coef0 = coef0)
  } else if (kernel == "gaussian") {
    K_m <- radial_kernel(x1 = x_m, x2 = x_m, gamma = gamma)
    K_n_m <- radial_kernel(x1 = x, x2 = x_m, gamma = gamma)
  } else if (kernel == "exponential") {
    K_m <- exponential_kernel(x1 = x_m, x2 = x_m, gamma = gamma)
    K_n_m <- exponential_kernel(x1 = x, x2 = x_m, gamma = gamma)
  } else if (kernel == "arc_cosine") {
    if (arc_cosine_deep == 1) {
      K_m <- arc_cosine_kernel(x1 = x_m, x2 = x_m)
      K_n_m <- arc_cosine_kernel(x1 = x, x2 = x_m)
    } else {
      K_m <- arc_cosine_kernel(x1 = x_m, x2 = x_m)
      K_nm1 <- arc_cosine_kernel(x1 = x, x2 = x_m)
      K_all <- arc_cosine_kernel(x1 = x, x2 = x)
      K_n_m <- arc_cosine_layers(
        AK1 = K_nm1,
        dAK1 = diag(K_all),
        nl = arc_cosine_deep
      )
    }
  } else {
    stop(kernel, " is not a valid kernel")
  }

  EVD_K_m <- eigen(K_m)
  U <- EVD_K_m$vectors
  S <- EVD_K_m$values
  S[EVD_K_m$values < 0] <- 0
  S_0.5_Inv <- sqrt(1 / S)
  S_mat_Inv <- diag(S_0.5_Inv)

  P <- K_n_m %*% U %*% S_mat_Inv
  rownames(P) <- rownames(x)
  colnames(P) <- colnames(x)[final_rows]

  P <- remove_no_variance_cols(P)

  return(P)
}

apply_kernel <- function(x,
                         kernel,
                         rows_proportion = 0.8,
                         arc_cosine_deep = 1,
                         gamma = 1 / ncol(x),
                         coef0 = 0,
                         degree = 3) {
  kernel <- tolower(kernel)

  if (is_sparse_kernel(kernel)) {
    x <- sparse_kernel(
      x,
      rows_proportion = rows_proportion,
      kernel = kernel,
      arc_cosine_deep = arc_cosine_deep,
      gamma = gamma,
      coef0 = coef0,
      degree = degree
    )
  } else if (is_conventional_kernel(kernel)) {
    x <- conventional_kernel(
      x,
      arc_cosine_deep = arc_cosine_deep,
      gamma = gamma,
      coef0 = coef0,
      degree = degree
    )
  } else {
    stop(kernel, " is not a valid kernel")
  }

  return(x)
}