#' @import checkmate

#' @include validator.R

# For categorical data --------------------------------------------------

#' @title Confusion matrix
#'
#' @templateVar coercibleTo factor
#'
#' @description
#' Given the observed and predicted values of categorical data (of any number of
#' classes) computes the confusion matrix.
#'
#' @template base-metrics-params
#' @param all_levels (`character`) The whole levels (categories) that `observed`
#'   and `predicted` have. Useful when `observed` and `predicted` are not
#'   factors or they both have no all levels. If it is `NULL` the union of
#'   `observed` levels and `predicted` levels is used. `NULL` by default.
#'
#' @return
#' An object of class `table` with the confusion matrix for all levels.
#'
#' @family categorical_metrics
#'
#' @examples
#' \dontrun{
#' confusion_matrix("a", "a")
#' confusion_matrix("a", "b")
#' confusion_matrix(c("a", "b"), c("c", "d"))
#' confusion_matrix(c("a", "a"), c("a", "a"), all_levels = c("a", "b"))
#' confusion_matrix(iris$Species, iris$Species)
#' }
#'
#' @export
confusion_matrix <- function(observed,
                             predicted,
                             all_levels = NULL,
                             na.rm = TRUE) {
  assert_same_length(observed, predicted)

  if (is.null(all_levels)) {
    all_levels <- get_all_levels(observed, predicted)
  }

  observed <- factor(observed, levels = all_levels)
  predicted <- factor(predicted, levels = all_levels)

  useNA <- if (na.rm) "no" else "always"
  return(table(observed, predicted, useNA = useNA))
}

#' @title Cohen's Kappa coefficient
#'
#' @description
#' Given the observed and predicted values of categorical data (of any number of
#' classes) computes the Cohen's Kappa coefficient.
#'
#' @inheritParams confusion_matrix
#'
#' @return
#' A single numeric value with the Cohen's Kappa coefficient.
#'
#' @examples
#' \dontrun{
#' kappa_coeff(c("a", "b"), c("a", "b"))
#' kappa_coeff(c("a", "b"), c("b", "a"))
#' kappa_coeff(c("a", "b", "a"), c("b", "a", "c"))
#' }
#'
#' @family categorical_metrics
#'
#' @export
kappa_coeff <- function(observed, predicted, all_levels = NULL, na.rm = TRUE) {
  conf_matrix <- confusion_matrix(
    observed,
    predicted,
    all_levels = all_levels,
    na.rm = na.rm
  )

  diagonal_counts <- diag(conf_matrix)
  N <- sum(conf_matrix)
  row_marginal_props <- rowSums(conf_matrix) / N
  col_marginal_props <- colSums(conf_matrix) / N

  Po <- sum(diagonal_counts) / N
  Pe <- sum(row_marginal_props * col_marginal_props)

  return((Po - Pe) / (1 - Pe))
}

#' @title Matthews Correlation Coefficient (MCC)
#'
#' @description
#' Given the observed and predicted values of binary data computes the Matthews
#' Correlation Coefficient (MCC) also known as Phi Coefficient or Mean Square
#' Contingency Coefficient.
#'
#' @inheritParams confusion_matrix
#'
#' @return
#' A single numeric value with the Matthews Correlation Coefficient.
#'
#' @family categorical_metrics
#'
#' @examples
#' \dontrun{
#' mcc(c("a", "b"), c("a", "b"))
#' mcc(c("a", "b"), c("b", "a"))
#' mcc(c("a", "b"), c("b", "b"))
#' mcc(c(TRUE, FALSE), c(FALSE, TRUE))
#' }
#'
#' @export
mcc <- function(observed, predicted, na.rm = TRUE) {
  assert_same_length(observed, predicted)

  all_levels <- get_all_levels(observed, predicted)

  if (length(all_levels) == 1) {
    all_levels <- c(all_levels, "OtherClass")
  } else if (length(all_levels) > 2) {
    stop("Matthews correlation coefficient (MCC) is only for binary variables")
  }

  conf_matrix <- confusion_matrix(
    observed,
    predicted,
    all_levels = all_levels,
    na.rm = na.rm
  )

  tp <- conf_matrix[1, 1]
  tn <- conf_matrix[2, 2]
  fp <- conf_matrix[1, 2]
  fn <- conf_matrix[2, 1]

  return(
    (tp * tn - fp * fn) /
    sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
  )
}

#' @title Sensitivity
#'
#' @description
#' Given the observed and predicted values of categorical data (of any number of
#' classes) computes the sensitivity (also known as recall), the metric that
#' evaluates a models ability to predict true positives of each available
#' category.
#'
#' @inheritParams confusion_matrix
#'
#' @return
#' For binary data a single value is returned, for more than 2 categories a
#' vector of sensitivities is returned, one per each category.
#'
#' @family categorical_metrics
#'
#' @examples
#' \dontrun{
#' sensitivity(c("a", "b"), c("a", "b"))
#' sensitivity(c("a", "b"), c("b", "a"))
#' sensitivity(c("a", "b"), c("b", "b"))
#' sensitivity(c(TRUE, FALSE), c(FALSE, TRUE))
#' sensitivity(c("a", "b", "a"), c("b", "a", "c"))
#' }
#'
#' @export
sensitivity <- function(observed, predicted, all_levels = NULL, na.rm = TRUE) {
  all_levels <- get_all_levels(observed, predicted)

  if (length(all_levels) == 1) {
    all_levels <- c(all_levels, "OtherClass")
  }

  conf_matrix <- confusion_matrix(
    observed,
    predicted,
    all_levels = all_levels,
    na.rm = na.rm
  )

  if (is_empty(conf_matrix)) {
    return(NaN)
  }

  all_levels <- colnames(conf_matrix)

  if (length(all_levels) == 2) {
    tp <- conf_matrix[1, 1]
    tn <- conf_matrix[2, 2]
    fp <- conf_matrix[1, 2]
    fn <- conf_matrix[2, 1]

    return(tp / (tp + fn))
  }

  sensitivities <- vector("numeric", length(all_levels))
  names(sensitivities) <- all_levels

  for (level in all_levels) {
    sensitivities[level] <- conf_matrix[level, level] /
      sum(conf_matrix[, level])
  }

  return(sensitivities)
}

#' @title Specificity
#'
#' @description
#' Given the observed and predicted values of categorical data (of any number of
#' classes) computes the specificity, the metric that evaluates a model's
#' ability to predict true negatives of each available category.
#'
#' @inheritParams confusion_matrix
#'
#' @return
#' For binary data a single value is returned, for more than 2 categories a
#' vector of sensitivities is returned, one per each category.
#'
#' @family categorical_metrics
#'
#' @examples
#' \dontrun{
#' specificity(c("a", "b"), c("a", "b"))
#' specificity(c("a", "b"), c("b", "a"))
#' specificity(c("a", "b"), c("b", "b"))
#' specificity(c(TRUE, FALSE), c(FALSE, TRUE))
#' specificity(c("a", "b", "a"), c("b", "a", "c"))
#' }
#'
#' @export
specificity <- function(observed, predicted, all_levels = NULL, na.rm = TRUE) {
  all_levels <- get_all_levels(observed, predicted)

  if (length(all_levels) == 1) {
    all_levels <- c(all_levels, "OtherClass")
  }

  conf_matrix <- confusion_matrix(
    observed,
    predicted,
    all_levels = all_levels,
    na.rm = na.rm
  )

  if (is_empty(conf_matrix)) {
    return(NaN)
  }

  all_levels <- colnames(conf_matrix)

  if (length(all_levels) == 2) {
    tp <- conf_matrix[1, 1]
    tn <- conf_matrix[2, 2]
    fp <- conf_matrix[1, 2]
    fn <- conf_matrix[2, 1]

    return(tn / (tn + fp))
  }

  specificities <- vector("numeric", length(all_levels))
  names(specificities) <- all_levels

  for (i in seq(all_levels)) {
    level <- all_levels[i]

    level_matrix <- conf_matrix[, -i, drop = FALSE]
    specificities[level] <- sum(level_matrix[level, ]) / sum(level_matrix)
  }

  return(specificities)
}

#' @title Recall
#'
#' @description
#' Given the observed and predicted values of categorical data (of any number of
#' classes) computes the recall (also known as sensitibity), the metric that
#' evaluates a models ability to predict true positives of each available
#' category.
#'
#' @inheritParams confusion_matrix
#'
#' @return
#' For binary data a single value is returned, for more than 2 categories a
#' vector of recalls is returned, one per each category.
#'
#' @family categorical_metrics
#'
#' @examples
#' \dontrun{
#' recall(c("a", "b"), c("a", "b"))
#' recall(c("a", "b"), c("b", "a"))
#' recall(c("a", "b"), c("b", "b"))
#' recall(c(TRUE, FALSE), c(FALSE, TRUE))
#' recall(c("a", "b", "a"), c("b", "a", "c"))
#' }
#'
#' @export
recall <- function(observed, predicted, all_levels = NULL, na.rm = TRUE) {
  return(sensitivity(
    observed,
    predicted,
    all_levels = all_levels,
    na.rm = na.rm
  ))
}

#' @title Precision
#'
#' @description
#' Given the observed and predicted values of categorical data (of any number of
#' classes) computes the precision, that represents the ratio of true positives
#' to total predicted positives.
#'
#' @inheritParams confusion_matrix
#'
#' @return
#' For binary data a single value is returned, for more than 2 categories a
#' vector of precisions is returned, one per each category.
#'
#' @family categorical_metrics
#'
#' @examples
#' \dontrun{
#' precision(c("a", "b"), c("a", "b"))
#' precision(c("a", "b"), c("b", "a"))
#' precision(c("a", "b"), c("b", "b"))
#' precision(c(TRUE, FALSE), c(FALSE, TRUE))
#' precision(c("a", "b", "a"), c("b", "a", "c"))
#' }
#'
#' @export
precision <- function(observed, predicted, all_levels = NULL, na.rm = TRUE) {
  all_levels <- get_all_levels(observed, predicted)

  if (length(all_levels) == 1) {
    all_levels <- c(all_levels, "OtherClass")
  }

  conf_matrix <- confusion_matrix(
    observed,
    predicted,
    all_levels = all_levels,
    na.rm = na.rm
  )

  if (is_empty(conf_matrix)) {
    return(NaN)
  }

  all_levels <- colnames(conf_matrix)

  if (length(all_levels) == 2) {
    tp <- conf_matrix[1, 1]
    tn <- conf_matrix[2, 2]
    fp <- conf_matrix[1, 2]
    fn <- conf_matrix[2, 1]

    return(tp / (tp + fp))
  }

  precisions <- vector("numeric", length(all_levels))
  names(precisions) <- all_levels

  diag_sum <- sum(diag(conf_matrix))

  for (level in all_levels) {
    precisions[level] <- conf_matrix[level, level] / diag_sum
  }

  return(precisions)
}

#' @title Proportion of Correctly Classified Cases (accuracy)
#'
#' @description
#' Given the observed and predicted values of categorical data (of any number of
#' classes) computes the Proportion of Correctly Classified Cases (also known as
#' accuracy).
#'
#' @inheritParams confusion_matrix
#'
#' @return
#' A single numeric value with the Proportion of Correctly Classified Cases.
#'
#' @family categorical_metrics
#'
#' @examples
#' \dontrun{
#' pccc(c("a", "b"), c("a", "b"))
#' pccc(c("a", "b"), c("b", "a"))
#' pccc(c("a", "b"), c("b", "b"))
#' pccc(c("a", "b", "a"), c("b", "a", "c"))
#' }
#'
#' @export
pccc <- function(observed, predicted, na.rm = TRUE) {
    assert_same_length(observed, predicted)

  return(mean(as.character(observed) == as.character(predicted), na.rm = na.rm))
}

#' @title Proportion of Incorrectly Classified Cases
#'
#' @description
#' Given the observed and predicted values of categorical data (of any number of
#' classes) computes the Proportion of Incorrectly Classified Cases (1 -
#' [pccc()], the same as: 1 - accuracy).
#'
#' @inheritParams confusion_matrix
#'
#' @return
#' A single numeric value with the Proportion of Incorrectly Classified Cases.
#'
#' @family categorical_metrics
#'
#' @examples
#' \dontrun{
#' pccc(c("a", "b"), c("a", "b"))
#' pccc(c("a", "b"), c("b", "a"))
#' pccc(c("a", "b"), c("b", "b"))
#' pccc(c("a", "b", "a"), c("b", "a", "c"))
#' }
#'
#' @export
pcic <- function(observed, predicted, na.rm = TRUE) {
  assert_same_length(observed, predicted)

  return(mean(as.character(observed) != as.character(predicted), na.rm = na.rm))
}

#' @title Brier Score
#'
#' @description
#' Given the observed values and the predicted probabilites of categorical data
#' (of at least two classes) computes the Brier Score.
#'
#' @inheritParams confusion_matrix
#' @param observed (`vector`) The observed values. It has to be coercible to
#'   `<%= coercibleTo %>` and with as many records as rows in `probabilities`.
#' @param probabilities (`matrix` | `data.frame`) The probability of each class
#'   for each individual. It is required the columns names of `probabilities`
#'   corresponds to all levels (or classes) in `observed` and that
#'   `probabilities` has as many rows as records `observed`.
#'
#' @return
#' A single numeric value with the Brier Score.
#'
#' @family categorical_metrics
#'
#' @examples
#' \dontrun{
#' probs <- matrix(c(0.7, 0.3, 0.2, 0.8), 2, 2, byrow = TRUE)
#' colnames(probs) <- c("0", "1")
#' brier_score(c(0, 1), probs)
#'
#' probs <- matrix(
#'   c(0.2, 0.3, 0.5, 0.8, 0.1, 0.1, 0.3, 0.3, 0.4),
#'   3,
#'   3,
#'   byrow = TRUE
#' )
#' colnames(probs) <- c("a", "b", "c")
#'
#' brier_score(c("a", "a", "c"), probs)
#'
#'
#' probs <- matrix(c(0, 1), 1, 2)
#' colnames(probs) <- c("a", "b")
#' brier_score("a", probs)
#' }
#'
#' @export
brier_score <- function(observed, probabilities, na.rm = TRUE) {
  if (!na.rm && (anyNA(observed) || anyNA(probabilities))) {
    return(NaN)
  } else if (length(observed) != nrow(probabilities)) {
    stop("observed and probabilities must have the same number of records")
  } else if (is.null(ncol(probabilities)) || ncol(probabilities) < 2) {
    stop("probabilities must have at least two columns (classes)")
  } else if (is.null(colnames(probabilities))) {
    stop("probabilities must have the classes' names as columns names")
  }

  if (na.rm) {
    remove_indices <- nas_indices(observed, probabilities)

    if (!is_empty(remove_indices)) {
      observed <- get_records(observed, -remove_indices)
      probabilities <- get_records(probabilities, -remove_indices)
    }
  }

  assert_subset(
    as.character(unique(observed)),
    colnames(probabilities),
    empty.ok = FALSE,
    .var.name = "observed"
  )

  observed <- factor(observed, levels = colnames(probabilities))
  observed_dummy <- model.matrix(~0 + observed)
  probabilities <- data.matrix(probabilities)

  return(mean(rowSums((probabilities - observed_dummy)^2)))
}

# For continuous data --------------------------------------------------

#' @title Mean Squared Error
#'
#' @templateVar coercibleTo numeric
#'
#' @description
#' Given the observed and predicted values of numeric data computes the Mean
#' Squared Error.
#'
#' @template base-metrics-params
#'
#' @return
#' A single numeric value with the Mean Squared Error.
#'
#' @examples
#' \dontrun{
#' mse(1, 1)
#' mse(1, 0)
#' mse(c(1, 2, NA, 4), c(1, NA, 3, 4))
#'
#' set.seed(1)
#' x <- rnorm(100)
#' mse(x, x)
#' mse(x, x - 1)
#' mse(x, x + 10)
#' }
#'
#' @family numeric_metrics
#'
#' @export
mse <- function(observed, predicted, na.rm = TRUE) {
  assert_same_length(observed, predicted)

  return(mean((as.numeric(observed) - as.numeric(predicted))^2, na.rm = na.rm))
}

#' @title Root Mean Squared Error
#'
#' @description
#' Given the observed and predicted values of numeric data computes the Root
#' Mean Squared Error.
#'
#' @inheritParams mse
#'
#' @return
#' A single numeric value with the Root Mean Squared Error.
#'
#' @examples
#' \dontrun{
#' rmse(1, 1)
#' rmse(1, 0)
#' rmse(c(1, 2, NA, 4), c(1, NA, 3, 4))
#'
#' set.seed(1)
#' x <- rnorm(100)
#' rmse(x, x)
#' rmse(x, x - 1)
#' rmse(x, x + 10)
#' }
#'
#' @family numeric_metrics
#'
#' @export
rmse <- function(observed, predicted, na.rm = TRUE) {
  return(sqrt(mse(observed, predicted, na.rm = na.rm)))
}

#' @title Normalized Root Mean Squared Error
#'
#' @description
#' Given the observed and predicted values of numeric data computes the
#' Normalized Root Mean Squared Error.
#'
#' @inheritParams mse
#' @param type (`character(1)`) (case not sensitive) The normalization type to
#'   use. The options are `"sd"`, `"mean"`, `"maxmin"` (or `"range"`) and `"iqr"`
#'   (for more information, see Details section below). `"sd"` by default.
#'
#' @details
#' The formula is the same as [rmse()] (Root Mean Square Error) but divided by a
#' normalization term specified in `type`:
#'
#' * `"sd"`: Standard deviatione.
#' * `"mean"`: Mean.
#' * `"maxmin"` or `"range"`: The range, maximun minus minimum.
#' * `"iqr"`: Interquantile range (Q3 - Q1).
#'
#' @return
#' A single numeric value with the Normalized Root Mean Squared Error.
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' x <- rnorm(100)
#' nrmse(x, x)
#' nrmse(x, x - 1, type = "mean")
#' nrmse(x, x + 10, type = "iqr")
#' nrmse(x, x + 10, type = "range")
#' nrmse(x, x + 10, type = "maxmin")
#' }
#'
#' @family numeric_metrics
#'
#' @export
nrmse <-  function(observed, predicted, type = "sd", na.rm = TRUE) {
  rmse_value <- rmse(observed, predicted)
  if (is.nan(rmse_value) || is.na(rmse_value)) {
    return(rmse_value)
  }

  lower_type <- tolower(type)

  divisor <- NULL

  if (lower_type == "sd") {
    divisor <- sd(observed, na.rm = na.rm)
  } else if (lower_type == "mean") {
    divisor <- mean(observed, na.rm = na.rm)
  } else if (lower_type == "maxmin" || lower_type == "range") {
    divisor <- diff(range(observed, na.rm = na.rm))
  } else if (lower_type == "iqr") {
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

#' @title Mean Abosolute Error
#'
#' @description
#' Given the observed and predicted values of numeric data computes the
#' Mean Abosolute Error.
#'
#' @inheritParams mse
#'
#' @return
#' A single numeric value with the Mean Abosolute Error.
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' x <- rnorm(100)
#' mae(x, x)
#' mae(x, x - 1)
#' mae(x, x + 10)
#' mae(x, x + 10)
#' mae(x, x + 10)
#' }
#'
#' @family numeric_metrics
#'
#' @export
mae <- function(observed, predicted, na.rm = TRUE) {
  assert_same_length(observed, predicted)

  return(mean(abs(observed - predicted), na.rm = na.rm))
}

#' @title Mean Arctangent Absolute Percentage Error
#'
#' @description
#' Given the observed and predicted values of numeric data computes the
#' Mean Arctangent Absolute Percentage Error.
#'
#' @inheritParams mse
#'
#' @return
#' A single numeric value with the Mean Arctangent Absolute Percentage Error.
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' x <- rnorm(100)
#' maape(x, x)
#' maape(x, x - 1)
#' maape(x, x + 10)
#' maape(x, x + 10)
#' maape(x, x + 10)
#' }
#'
#' @family numeric_metrics
#'
#' @export
maape <- function(observed, predicted, na.rm = TRUE) {
  assert_same_length(observed, predicted)

  if (is.null(observed) && is.null(predicted)) {
    return(NaN)
  }

  return(mean(atan(abs(observed - predicted) / abs(observed)), na.rm = na.rm))
}

#' @title Spearman's correlation
#'
#' @description
#' Computes the Spearman's correlation.
#'
#' @param x (`numeric` | `matrix`) The values to calculate the correlation.
#' @param y (`numeric` | `matrix`) The values to calculate the correlation with.
#'   the same values as `x` by default.
#' @inheritParams mse
#'
#' @return
#' A single numeric value with the Spearman's correlation.
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' x <- rnorm(100)
#' spearman(x, x)
#' spearman(x, x + rnorm(100, 2))
#' spearman(x, x - 1)
#' spearman(x, x + 10)
#' }
#'
#' @family numeric_metrics
#'
#' @export
spearman <- function(x, y = x, na.rm = TRUE) {
  if (na.rm) {
    remove_indices <- nas_indices(x, y)

    if (!is_empty(remove_indices)) {
      x <- get_records(x, -remove_indices)
      y <- get_records(y, -remove_indices)
    }
  }

  assert_same_length(x, y)

  if (is.null(x) && is.null(y)) {
    return(NaN)
  }

  return(cor(x, y, method = "spearman", use = "everything"))
}

#' @title Pearson's correlation
#'
#' @description
#' Computes the Pearson's correlation.
#'
#' @param x (`numeric` | `matrix`) The values to calculate the correlation.
#' @param y (`numeric` | `matrix`) The values to calculate the correlation with.
#'   the same values as `x` by default.
#' @inheritParams mse
#'
#' @return
#' A single numeric value with the Pearson's correlation.
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' x <- rnorm(100)
#' pearson(x, x)
#' pearson(x, x + rnorm(100, 2))
#' pearson(x, x - 1)
#' pearson(x, x + 10)
#' }
#'
#' @family numeric_metrics
#'
#' @export
pearson <- function(x, y = x, na.rm = TRUE) {
  if (na.rm) {
    remove_indices <- nas_indices(x, y)

    if (!is_empty(remove_indices)) {
      x <- get_records(x, -remove_indices)
      y <- get_records(y, -remove_indices)
    }
  }

  assert_same_length(x, y)

  if (is.null(x) && is.null(y)) {
    return(NaN)
  }

  return(cor(x, y, method = "pearson", use = "everything"))
}

# Helpers --------------------------------------------------

multivariate_loss <- function(observed, predicted, responses) {
  all_metrics <- c()

  for (response_name in names(responses)) {
    response_type <- responses[[response_name]]$type

    loss_function <- pcic
    if (is_numeric_response(response_type)) {
      loss_function <- maape
    }
    current_value <- loss_function(
      observed[, response_name],
      predicted[[response_name]]$predicted
    )
    all_metrics <- c(all_metrics, current_value)
  }

  return(mean(all_metrics, na.rm = TRUE))
}

get_loss_function <- function(responses, is_multivariate) {
  if (is_multivariate) {
    return(multivariate_loss)
  } else if (is_class_response(responses[[1]]$type)) {
    return(pcic)
  } else if (is_numeric_response(responses[[1]]$type)) {
    return(mse)
  } else {
    stop(sprintf(
      "{%s} is not recognized as a type of response variable",
      set_collapse(responses[[1]]$type)
    ))
  }
}
