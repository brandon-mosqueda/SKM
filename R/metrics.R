# For categorical data --------------------------------------------------

#' @title Class agreement
#' Taken from e1071 original source code
#' @export
classAgreement <- function(tab, match.names = FALSE) {
  n <- sum(tab)
  ni <- apply(tab, 1, sum)
  nj <- apply(tab, 2, sum)
  if (match.names && !is.null(dimnames(tab))) {
    lev <- intersect(colnames(tab), rownames(tab))
    p0 <- sum(diag(tab[lev, lev])) / n
    pc <- sum(ni[lev] * nj[lev]) / n^2
  } else {
    m <- min(length(ni), length(nj))
    p0 <- sum(diag(tab[1:m, 1:m])) / n
    pc <- sum((ni[1:m] / n) * (nj[1:m] / n))
  }
  n2 <- choose(n, 2)
  rand <- 1 + (sum(tab^2) - (sum(ni^2) + sum(nj^2)) / 2) / n2
  nis2 <- sum(choose(ni[ni > 1], 2))
  njs2 <- sum(choose(nj[nj > 1], 2))
  crand <- (sum(choose(tab[tab > 1], 2)) - (nis2 * njs2) / n2) / ((nis2 +
    njs2) / 2 - (nis2 * njs2) / n2)

  return(list(
    diag = p0,
    kappa = (p0 - pc) / (1 - pc),
    rand = rand,
    crand = crand
  ))
}

#' @title Kappa coefficient
kappa_coeff <- function(actual, predicted) {
  return(classAgreement(table(actual, predicted))$kappa)
}

#' @title Proportion of correctly classified cases
pccc <- function(actual, predicted, na.rm = TRUE) {
  return(mean(actual == predicted, na.rm = na.rm))
}

#' @title Brier score
brier <- function(actual, probabilities) {
  if (all(is.na(actual))) {
    return(NA)
  }
  actual <- factor(actual, levels = colnames(probabilities))
  ActualDummy <- model.matrix(~ 0 + actual)

  return(mean(rowSums((probabilities - ActualDummy)^2)))
}

# For continuous data --------------------------------------------------

#' @title Mean Square Error
mse <- function(actual, predicted, na.rm = TRUE) {
  return(mean((actual - predicted)^2, na.rm = na.rm))
}

#' @title Root Mean Square Error
rmse <- function(actual, predicted, na.rm = TRUE) {
  return(sqrt(mse(actual, predicted, na.rm = na.rm)))
}

#' @title Mean Absolute Error
mae <- function(actual, predicted, na.rm = TRUE) {
  return(mean(abs(actual - predicted), na.rm = na.rm))
}

#' @title Mean Arctangent Absolute Percentage Error
maape <- function(actual, predicted, na.rm = TRUE) {
  return(mean(atan(abs(actual - predicted) / abs(actual)), na.rm = na.rm))
}

#' @title Normalized Root Mean Square Error
nrmse <- function(actual, predicted, na.rm = TRUE) {
  return(rmse(actual, predicted, na.rm) / mean(actual, na.rm = na.rm))
}

#' @title Range Normalized Root Mean Square Error
rnrmse <- function(observed, predicted, na.rm = TRUE) {
  return(
    rmse(observed, predicted, na.rm) /
    diff(range(observed, na.rm = na.rm))
  )
}

spearman <- function(x, y) {
  return(cor(x, y, method = "spearman", use = "na.or.complete"))
}

pearson <- function(x, y) {
  return(cor(x, y, method = "pearson", use = "na.or.complete"))
}
