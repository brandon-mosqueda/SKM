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

# Conventional kernels --------------------------------------------------
linear_kernel <- function(x) {
  return(tcrossprod(x) / ncol(x))
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

polynomial_kernel <- function(x1, x2 = x1, gamma = 1, b = 0, p = 3) {
  return((gamma * (as.matrix(x1) %*% t(x2)) + b)^p)
}

sigmoid_kernel <- function(x1, x2 = x1, gamma = 1, b = 0) {
  return(tanh(gamma * (as.matrix(x1) %*% t(x2)) + b))
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

# Sparse kernels --------------------------------------------------
sparse_kernel <- function(x,
                          sample_proportion = 1,
                          kernel = "linear",
                          kernel_deep = 1) {
  name <- tolower(kernel)
  name <- gsub("sparse_", "", name)
  p <- ncol(x)
  pos_m <- sample(1:nrow(x), nrow(x) * rows_proportion)

  # Step 1 compute K_m
  x_m <- x[pos_m, ]

  if (name == "linear") {
    K_m <- x_m %*% t(x_m) / p
    K_n_m <- x %*% t(x_m) / p
  } else if (name == "polynomial") {
    K_m <- polynomial_kernel(x1 = x_m, x2 = x_m, gamma = 1 / p)
    K_n_m <- polynomial_kernel(x1 = x, x2 = x_m, gamma = 1 / p)
  } else if (name == "sigmoid") {
    K_m <- sigmoid_kernel(x1 = x_m, x2 = x_m, gamma = 1 / p)
    K_n_m <- sigmoid_kernel(x1 = x, x2 = x_m, gamma = 1 / p)
  } else if (name == "gaussian") {
    K_m <- radial_kernel(x1 = x_m, x2 = x_m, gamma = 1 / p)
    K_n_m <- radial_kernel(x1 = x, x2 = x_m, gamma = 1 / p)
  } else if (name == "exponential") {
    K_m <- exponential_kernel(x1 = x_m, x2 = x_m, gamma = 1 / p)
    K_n_m <- exponential_kernel(x1 = x, x2 = x_m, gamma = 1 / p)
  } else if (is_arc_cosine_kernel(name)) {
    if (name == "arc_cosine_1") {
      K_m <- arc_cosine_kernel(x1 = x_m, x2 = x_m)
      K_n_m <- arc_cosine_kernel(x1 = x, x2 = x_m)
    } else {
      K_m <- arc_cosine_kernel(x1 = x_m, x2 = x_m)
      K_nm1 <- arc_cosine_kernel(x1 = x, x2 = x_m)
      K_all <- arc_cosine_kernel(x1 = x, x2 = x)
      K_n_m <- arc_cosine_layers(
        AK1 = K_nm1,
        dAK1 = diag(K_all),
        nl = kernel_deep
      )
    }
  }

  EVD_K_m <- eigen(K_m)
  U <- EVD_K_m$vectors
  S <- EVD_K_m$values
  S[EVD_K_m$values < 0] <- 0
  S_0.5_Inv <- sqrt(1 / S)
  S_mat_Inv <- diag(S_0.5_Inv)
  P <- K_n_m %*% U %*% S_mat_Inv
  rownames(P) <- rownames(x)
  colnames(P) <- colnames(x)[pos_m]

  P <- remove_no_variance_cols(P)

  return(P)
}
