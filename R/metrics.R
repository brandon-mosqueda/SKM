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

#' @export
confusion_matrix <- function(observed, predicted, all_levels = NULL) {
  observed <- as.factor(observed)
  predicted <- as.factor(predicted)

  if (is.null(all_levels)) {
    all_levels <- union(levels(observed), levels(predicted))
  }

  levels(observed) <- all_levels
  levels(predicted) <- all_levels

  return(table(observed, predicted))
}

#' @title Kappa coefficient
#' @export
kappa_coeff <- function(observed, predicted, all_levels = NULL) {
  return(classAgreement(confusion_matrix(observed, predicted, all_levels))$kappa)
}

#' @title Proportion of correctly classified cases
#' @export
pccc <- function(observed, predicted, na.rm = TRUE) {
  if (length(observed) != length(predicted)) {
    stop("observed and predicted must have the same length")
  }

  return(mean(observed == predicted, na.rm = na.rm))
}

#' @title Proportion of cases incorrectly classified
#' @export
pcic <- function(observed, predicted, na.rm = TRUE) {
  if (length(observed) != length(predicted)) {
    stop("observed and predicted must have the same length")
  }

  return(mean(observed != predicted, na.rm = na.rm))
}

#' @title Brier score
#' @export
brier_score <- function(observed, probabilities) {
  if (length(observed) != nrow(probabilities)) {
    stop("observed and probabilities must have the same number of records")
  } else if (is.null(ncol(probabilities)) || ncol(probabilities) < 2) {
    stop("probabilities must have at least two columns (classes)")
  } else if (is.null(colnames(probabilities))) {
    stop("probabilities must have the classes' names as columns names")
  }

  if (all(is.na(observed))) {
    return(NaN)
  }
  observed <- factor(observed, levels = colnames(probabilities))
  observed_dummy <- model.matrix(~ 0 + observed)

  return(mean(rowSums((probabilities - observed_dummy)^2)))
}

# For continuous data --------------------------------------------------

#' @title Mean Squared Error
#' @export
mse <- function(observed, predicted, na.rm = TRUE) {
  if (length(observed) != length(predicted)) {
    stop("observed and predicted must have the same length")
  }

  return(mean((as.numeric(observed) - as.numeric(predicted))^2, na.rm = na.rm))
}

#' @title Root Mean Squared Error
#' @export
rmse <- function(observed, predicted, na.rm = TRUE) {
  return(sqrt(mse(observed, predicted, na.rm = na.rm)))
}

#' @title Normalize Root Mean Squared Error
#' @export
nrmse <-  function(observed, predicted, type = "sd", na.rm = TRUE) {
  rmse_value <- rmse(observed, predicted)
  if (is.nan(rmse_value) || is.na(rmse_value)) {
    return(rmse_value)
  }

  type <- tolower(type)

  divisor <- NULL

  if (type == "sd") {
    divisor <- sd(observed, na.rm = na.rm)
  } else if (type == "mean") {
    divisor <- mean(observed, na.rm = na.rm)
  } else if (type == "maxmin" || type == "range") {
    divisor <- diff(range(observed, na.rm = na.rm))
  } else if (type == "iq") {
    divisor <- diff(quantile(observed, c(0.25, 0.75), na.rm = na.rm))
  } else {
    stop(sprintf(
      "{%s} is not a valid type of normalization",
      set_collapse(type)
    ))
  }

  result <- rmse_value / divisor

  if (is.infinite(result)) {
    result <- NaN
  }
  result <- as.numeric(result)

  return(result)
}

#' @title Mean Absolute Error
#' @export
mae <- function(observed, predicted, na.rm = TRUE) {
  if (length(observed) != length(predicted)) {
    stop("observed and predicted must have the same length")
  }

  return(mean(abs(observed - predicted), na.rm = na.rm))
}

#' @title Mean Arctangent Absolute Percentage Error
#' @export
maape <- function(observed, predicted, na.rm = TRUE) {
  if (length(observed) != length(predicted)) {
    stop("observed and predicted must have the same length")
  } else if (is.null(observed) && is.null(predicted)) {
    return(NaN)
  }

  return(mean(atan(abs(observed - predicted) / abs(observed)), na.rm = na.rm))
}

spearman <- function(x, y) {
  if (length(x) != length(y)) {
    stop("x and y must have the same length")
  } else if (is.null(x) && is.null(y)) {
    return(NaN)
  }

  return(cor(x, y, method = "spearman", use = "na.or.complete"))
}

pearson <- function(x, y) {
  if (length(x) != length(y)) {
    stop("x and y must have the same length")
  } else if (is.null(x) && is.null(y)) {
    return(NaN)
  }

  return(cor(x, y, method = "pearson", use = "na.or.complete"))
}
