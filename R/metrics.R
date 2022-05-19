#' @import checkmate
#' @importFrom MLmetrics PRAUC

#' @include validator.R

# For categorical data ---------------------------------------------------------

#' @title Mathematical Mode
#'
#' @description
#' Obtain the most frecuent values (mode) with the frequency from a vector. By
#' default all values with the greatest frequency are returned but you can
#' limit the result to the first one, in lexicographical order.
#'
#' @param x (`factor`) A vector of values to compute the mode.
#' @param remove_na (`logical(1)`) Should `NA` values be removed and not
#'   include them in the frequencies counting? `TRUE` by default.
#' @param allow_multimodal (`logical(1)`) If there is more than one mode, should
#'   all the most frequent values be returned? `TRUE` by default.
#'
#' @return
#' A `character` vector with the most frequent values if `allow_multimodal` is
#' `TRUE` or the first most frequent value (after ordering lexicographically) if
#' `allow_multimodal` is `FALSE`.
#'
#' The returned vector contains the attribute `"frequency"` with an integer of
#' the greatest frequency.
#'
#' @family categorical_metrics
#'
#' @examples
#' \dontrun{
#' # Multimodal
#' x <- math_mode(c("C", "A", "C", "A"))
#' # Obtain the greatest frequency
#' attr(x, "frequency")
#'
#' # Single mode
#' x <- math_mode(c("C", "A", "C", "A"), allow_multimodal = FALSE)
#'
#' # Multimodal
#' x <- math_mode(iris$Species)
#' attr(x, "frequency")
#'
#' values <- c("A", "B", NA, "C", "A", NA, "B")
#' # Multimodal without NAs
#' x <- math_mode(values)
#' # Multimodal with NAs
#' x <- math_mode(values, remove_na = FALSE)
#' }
#'
#' @export
math_mode <- function(x, remove_na = TRUE, allow_multimodal = TRUE) {
  assert_factor(x, min.len = 1)

  use_na <- "ifany"
  if (remove_na) use_na <- "no"

  frequencies <- sort(table(x, useNA = use_na), decreasing = TRUE)
  if (is_empty(frequencies)) return(NULL)

  if (allow_multimodal) {
    frequencies <- frequencies[frequencies == frequencies[1]]
  } else {
    frequencies <- frequencies[1]
  }

  mode <- names(frequencies)
  attr(mode, "frequency") <- as.integer(frequencies[1])

  return(mode)
}

#' @title Confusion matrix
#'
#' @description
#' Given the observed and predicted values of categorical data (of any number of
#' classes) computes the confusion matrix.
#'
#' @param observed (`factor`) The observed values. It has to have the same
#'   length as `predicted`.
#' @param predicted (`factor`) The observed values. It has to have the same
#'   length as `observed`.
#' @param remove_na (`logical(1)`) Should `NA` values be removed?. `TRUE` by
#'   default.
#'
#' @return
#' An object of class `table` with the confusion matrix.
#'
#' @family categorical_metrics
#'
#' @examples
#' \dontrun{
#' confusion_matrix(factor("a"), factor("a"))
#' confusion_matrix(factor("a"), factor("b"))
#' confusion_matrix(factor(c("a", "b")), factor(c("c", "d")))
#' confusion_matrix(factor(c("a", "a")), factor(c("a", "a")))
#' confusion_matrix(iris$Species, iris$Species)
#' }
#'
#' @export
confusion_matrix <- function(observed, predicted, remove_na = TRUE) {
  assert_categorical_obs_pred(observed, predicted)

  all_classes <- union(levels(observed), levels(predicted))
  observed <- factor(observed, levels = all_classes)
  predicted <- factor(predicted, levels = all_classes)

  useNA <- if (remove_na) "no" else "always"
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
#' @details
#' Given a confusion matrix P, Cohen's Kappa coefficient is be computed by:
#'
#' ![](kappa_coefficient.png "kappa(x) = (P_o - P_e) / (1 - P_e)")
#'
#' P_o is the sum of all diagonal values and P_e is the sum of all the products
#' of row i times col i.
#'
#' @return
#' A single numeric value with the Cohen's Kappa coefficient.
#'
#' @examples
#' \dontrun{
#' kappa_coeff(factor(c("a", "b")), factor(c("a", "b")))
#' kappa_coeff(factor(c("a", "b")), factor(c("b", "a")))
#' kappa_coeff(factor(c("a", "b", "a")), factor(c("b", "a", "c")))
#' }
#'
#' @family categorical_metrics
#'
#' @export
kappa_coeff <- function(observed, predicted, remove_na = TRUE) {
  conf_matrix <- confusion_matrix(observed, predicted, remove_na = remove_na)

  diagonal_counts <- diag(conf_matrix)
  N <- sum(conf_matrix)
  row_marginal_props <- rowSums(conf_matrix) / N
  col_marginal_props <- colSums(conf_matrix) / N

  Po <- as.numeric(sum(diagonal_counts) / N)
  Pe <- sum(as.numeric(row_marginal_props * col_marginal_props))

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
#' @details
#' Given the following binary confusion matrix:
#'
#' ![](binary_confusion_matrix.png "Binary confusion matrix")
#'
#' Matthews Correlation Coefficient (MCC) is computed as:
#'
#' ![](matthews_coefficient.png "(TN x TP - FN x FP) / (sqrt((TP + FP)(TP + FN)(TN + FP)(TN + FN)))")
#'
#' @return
#' A single numeric value with the Matthews Correlation Coefficient.
#'
#' @family categorical_metrics
#'
#' @examples
#' \dontrun{
#' matthews_coeff(factor(c("a", "b")), factor(c("a", "b")))
#' matthews_coeff(factor(c("a", "b")), factor(c("b", "a")))
#' matthews_coeff(factor(c("a", "b")), factor(c("b", "b")))
#' matthews_coeff(factor(c(TRUE, FALSE)), factor(c(FALSE, TRUE)))
#' }
#'
#' @export
matthews_coeff <- function(observed, predicted, remove_na = TRUE) {
  conf_matrix <- confusion_matrix(observed, predicted, remove_na = remove_na)

  if (ncol(conf_matrix) != 2) {
    stop("Matthews correlation coefficient (MCC) is only for binary variables")
  }

  rates <- as_tf_rates(conf_matrix)

  return(
    (rates$tp * rates$tn - rates$fp * rates$fn) /
    sqrt(
      (rates$tp + rates$fp) * (rates$tp + rates$fn) *
      (rates$tn + rates$fp) * (rates$tn + rates$fn)
    )
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
#' @param positive_class (`character(1)`) The name of the class (level) to be
#'   taken as reference as the positive class. This parameter is only used for
#'   binary variables. `NULL` by default which uses the second class in the
#'   union of the classes (levels) in `observed` and `predicted`.
#'
#' @details
#' Given the following binary confusion matrix:
#'
#' ![](binary_confusion_matrix.png "Binary confusion matrix")
#'
#' Sensitivity is computed as:
#'
#' ![](sensitivity.png "(TP) / (TP + FN)")
#'
#' @return
#' For binary data a single value is returned, for more than 2 categories a
#' vector of sensitivities is returned, one per each category.
#'
#' @family categorical_metrics
#'
#' @examples
#' \dontrun{
#' sensitivity(factor(c("a", "b")), factor(c("a", "b")))
#' sensitivity(factor(c("a", "b")), factor(c("b", "a")))
#' sensitivity(factor(c("a", "b")), factor(c("b", "b")))
#' sensitivity(factor(c(TRUE, FALSE)), factor(c(FALSE, TRUE)))
#' sensitivity(factor(c("a", "b", "a")), factor(c("b", "a", "c")))
#' }
#'
#' @export
sensitivity <- function(observed,
                        predicted,
                        positive_class = NULL,
                        remove_na = TRUE) {
  conf_matrix <- confusion_matrix(observed, predicted, remove_na = remove_na)
  assert_confusion_matrix(conf_matrix)
  assert_positive_class(positive_class, colnames(conf_matrix))

  all_classes <- colnames(conf_matrix)

  if (length(all_classes) == 2) {
    if (!is.null(positive_class)) {
      all_classes <- c(setdiff(all_classes, positive_class), positive_class)
      conf_matrix <- conf_matrix[all_classes, all_classes]
    }
    rates <- as_tf_rates(conf_matrix)

    return(rates$tp / (rates$tp + rates$fn))
  }

  sensitivities <- vector("numeric", length(all_classes))
  names(sensitivities) <- all_classes

  for (level in all_classes) {
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
#' @inheritParams sensitivity
#'
#' @details
#' Given the following binary confusion matrix:
#'
#' ![](binary_confusion_matrix.png "Binary confusion matrix")
#'
#' Specificity is computed as:
#'
#' ![](specificity.png "(TN) / (TN + FP)")
#'
#' @return
#' For binary data a single value is returned, for more than 2 categories a
#' vector of sensitivities is returned, one per each category.
#'
#' @family categorical_metrics
#'
#' @examples
#' \dontrun{
#' specificity(factor(c("a", "b")), factor(c("a", "b")))
#' specificity(factor(c("a", "b")), factor(c("b", "a")))
#' specificity(factor(c("a", "b")), factor(c("b", "b")))
#' specificity(factor(c(TRUE, FALSE)), factor(c(FALSE, TRUE)))
#' specificity(factor(c("a", "b", "a")), factor(c("b", "a", "c")))
#' }
#'
#' @export
specificity <- function(observed,
                        predicted,
                        positive_class = NULL,
                        remove_na = TRUE) {
  conf_matrix <- confusion_matrix(observed, predicted, remove_na = remove_na)
  assert_confusion_matrix(conf_matrix)
  assert_positive_class(positive_class, colnames(conf_matrix))

  all_classes <- colnames(conf_matrix)

  if (length(all_classes) == 2) {
    if (!is.null(positive_class)) {
      all_classes <- c(setdiff(all_classes, positive_class), positive_class)
      conf_matrix <- conf_matrix[all_classes, all_classes]
    }
    rates <- as_tf_rates(conf_matrix)

    return(as.numeric(rates$tn / (rates$tn + rates$fp)))
  }

  specificities <- vector("numeric", length(all_classes))
  names(specificities) <- all_classes

  for (i in seq(all_classes)) {
    level <- all_classes[i]

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
#' @inheritParams sensitivity
#'
#' @details
#' Given the following binary confusion matrix:
#'
#' ![](binary_confusion_matrix.png "Binary confusion matrix")
#'
#' Recall is computed as:
#'
#' ![](recall.png "(TP) / (TP + FN)")
#'
#' @return
#' For binary data a single value is returned, for more than 2 categories a
#' vector of recalls is returned, one per each category.
#'
#' @family categorical_metrics
#'
#' @examples
#' \dontrun{
#' recall(factor(c("a", "b")), factor(c("a", "b")))
#' recall(factor(c("a", "b")), factor(c("b", "a")))
#' recall(factor(c("a", "b")), factor(c("b", "b")))
#' recall(factor(c(TRUE, FALSE)), factor(c(FALSE, TRUE)))
#' recall(factor(c("a", "b", "a")), factor(c("b", "a", "c")))
#' }
#'
#' @export
recall <- function(observed,
                   predicted,
                   positive_class = NULL,
                   remove_na = TRUE) {
  return(sensitivity(
    observed,
    predicted,
    positive_class = positive_class,
    remove_na = remove_na
  ))
}

#' @title Precision
#'
#' @description
#' Given the observed and predicted values of categorical data (of any number of
#' classes) computes the precision, that represents the ratio of true positives
#' to total predicted positives.
#'
#' @inheritParams sensitivity
#'
#' @details
#' Given the following binary confusion matrix:
#'
#' ![](binary_confusion_matrix.png "Binary confusion matrix")
#'
#' Precision is computed as:
#'
#' ![](precision.png "(TP) / (TP + FP)")
#'
#' @return
#' For binary data a single value is returned, for more than 2 categories a
#' vector of precisions is returned, one per each category.
#'
#' @family categorical_metrics
#'
#' @examples
#' \dontrun{
#' precision(factor(c("a", "b")), factor(c("a", "b")))
#' precision(factor(c("a", "b")), factor(c("b", "a")))
#' precision(factor(c("a", "b")), factor(c("b", "b")))
#' precision(factor(c(TRUE, FALSE)), factor(c(FALSE, TRUE)))
#' precision(factor(c("a", "b", "a")), factor(c("b", "a", "c")))
#' }
#'
#' @export
precision <- function(observed,
                      predicted,
                      positive_class = NULL,
                      remove_na = TRUE) {
  conf_matrix <- confusion_matrix(observed, predicted, remove_na = remove_na)
  assert_confusion_matrix(conf_matrix)
  assert_positive_class(positive_class, colnames(conf_matrix))

  all_classes <- colnames(conf_matrix)

  if (length(all_classes) == 2) {
    if (!is.null(positive_class)) {
      all_classes <- c(setdiff(all_classes, positive_class), positive_class)
      conf_matrix <- conf_matrix[all_classes, all_classes]
    }

    rates <- as_tf_rates(conf_matrix)

    return(rates$tp / (rates$tp + rates$fp))
  }

  precisions <- vector("numeric", length(all_classes))
  names(precisions) <- all_classes

  diag_sum <- sum(diag(conf_matrix))

  for (level in all_classes) {
    precisions[level] <- as.numeric(conf_matrix[level, level]) / diag_sum
  }

  return(precisions)
}

#' @title ROC Area Under the Curver (ROC-AUC)
#'
#' @description
#' Given the observed values and predicted probabilities values of binary data
#' computes the ROC Area Under the Curve (ROC-AUC).
#'
#' @inheritParams sensitivity
#' @param observed (`factor`) The observed values. It has to have the same
#'   length as rows `probabilities`.
#' @param probabilities (`data.frame`) The probability of each class for each
#'   individual. It is required the columns names of `probabilities` corresponds
#'   to all classes (levels) in `observed` and that `probabilities` has as many
#'   rows as records `observed`.
#'
#' @return
#' A single numeric value with the ROC-AUC.
#'
#' @family categorical_metrics
#'
#' @examples
#' \dontrun{
#' roc_auc(factor(c("a", "b")), data.frame(a = c(0.2, 0.6), b = c(0.5, 0.4)))
#' roc_auc(factor(c("a", "b")), data.frame(a = c(0.8, 0.3), b = c(0.2, 0.7)))
#' roc_auc(
#'   factor(c("a", "b")),
#'   data.frame(a = c(0.2, 0.6), b = c(0.5, 0.4)),
#'   positive_class = "b"
#' )
#' roc_auc(
#'   factor(c(TRUE, FALSE)),
#'   data.frame(
#'     `TRUE` = c(0.3, 0.2),
#'     `FALSE` = c(0.7, 0.8),
#'     check.names = FALSE
#'   )
#' )
#' }
#'
#' @export
roc_auc <- function(observed,
                    probabilities,
                    positive_class = NULL,
                    remove_na = TRUE) {
  assert_observed_probabilities(observed, probabilities)

  all_classes <- colnames(probabilities)
  assert_positive_class(positive_class, all_classes)

  if (length(all_classes) != 2) {
    stop("ROC Area Under the Curve (ROC-AUC) is only for binary variables")
  }

  if (is.null(positive_class)) {
    positive_class <- all_classes[2]
  }

  observed <- factor(observed, levels = all_classes)
  observed <- observed == positive_class
  probabilities <- probabilities[[positive_class]]
  n1 <- sum(!observed)
  n2 <- sum(observed)
  U <- sum(rank(probabilities)[!observed]) - n1 * (n1 + 1) / 2

  return(1 - U / n1 / n2)
}

#' @title Precision-Recall Area Under the Curver (PR-AUC)
#'
#' @description
#' Given the observed values and predicted probabilities values of binary data
#' computes the Precision-Recall Area Under the Curve (PR-AUC).
#'
#' @inheritParams roc_auc
#'
#' @return
#' A single numeric value with the PR-AUC.
#'
#' @family categorical_metrics
#'
#' @examples
#' \dontrun{
#' pr_auc(factor(c("a", "b")), data.frame(a = c(0.2, 0.6), b = c(0.5, 0.4)))
#' pr_auc(factor(c("a", "b")), data.frame(a = c(0.8, 0.3), b = c(0.2, 0.7)))
#' pr_auc(
#'   factor(c("a", "b")),
#'   data.frame(a = c(0.2, 0.6), b = c(0.5, 0.4)),
#'   positive_class = "b"
#' )
#' pr_auc(
#'   factor(c(TRUE, FALSE)),
#'   data.frame(
#'     `TRUE` = c(0.3, 0.2),
#'     `FALSE` = c(0.7, 0.8),
#'     check.names = FALSE
#'   )
#' )
#' }
#'
#' @export
pr_auc <- function(observed,
                   probabilities,
                   positive_class = NULL,
                   remove_na = TRUE) {
  assert_observed_probabilities(observed, probabilities)

  all_classes <- colnames(probabilities)
  assert_positive_class(positive_class, all_classes)

  if (length(all_classes) != 2) {
    stop(
      "Precision-Recall Aure Under the Curve (PR-AUC) is only for binary, ",
      "variables."
    )
  }

  if (is.null(positive_class)) {
    positive_class <- all_classes[2]
  }

  result <- try(
    MLmetrics::PRAUC(
      probabilities[[positive_class]],
      as.integer(observed == positive_class)
    ),
    silent = TRUE
  )

  return(if(is.numeric(result)) result else NaN)
}

#' @title F1 score
#'
#' @description
#' Given the observed and predicted values of categorical data (of any number of
#' classes) computes the F1 score, that combines the [precision] and [recall],
#' and it is defined as the harmonic mean of the precision and recall.
#'
#' @inheritParams sensitivity
#'
#' @details
#' F1 score is computed as:
#'
#' ![](f1_score.png "2 * ((precision * recall) / (precision + recall))")
#'
#' See [precision()] and [recall()] for more information.
#'
#' @return
#' For binary data a single value is returned, for more than 2 categories a
#' vector of F1 scores is returned, one per each category.
#'
#' @family categorical_metrics
#'
#' @examples
#' \dontrun{
#' f1_score(factor(c("a", "b")), factor(c("a", "b")))
#' f1_score(factor(c("a", "b", "a", "b")), factor(c("a", "b", "b", "a")))
#' f1_score(factor(c("a", "b")), factor(c("b", "b")))
#' }
#'
#' @export
f1_score <- function(observed,
                     predicted,
                     positive_class = NULL,
                     remove_na = TRUE) {
  p <- precision(
    observed,
    predicted,
    positive_class = positive_class,
    remove_na = remove_na
  )
  r <- recall(
    observed,
    predicted,
    positive_class = positive_class,
    remove_na = remove_na
  )

  return(2 * ((p * r) / (p + r)))
}

#' @title Proportion of Correctly Classified Cases (PCCC)
#'
#' @description
#' Given the observed and predicted values of categorical data (of any number of
#' classes) computes the Proportion of Correctly Classified Cases (also known as
#' accuracy).
#'
#' @inheritParams confusion_matrix
#'
#' @details
#' PCCC can be computed as:
#'
#' ![](pccc.png "(1 / N) * (sum(diag(confusion_matrix)))")
#'
#' that is, the sum of the diagonal in the confusion matrix (correct
#' classifications) over the total number of values in the matrix (N). An
#' equivalent but more efficient method is used.
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
pccc <- function(observed, predicted, remove_na = TRUE) {
  assert_categorical_obs_pred(observed, predicted)

  return(mean(
    as.character(observed) == as.character(predicted),
    na.rm = remove_na
  ))
}

#' @title Accuracy
#'
#' @description
#' Given the observed and predicted values of categorical data (of any number of
#' classes) computes the accuracy (also known as proportion of correctly
#' classified cases).
#'
#' @inheritParams confusion_matrix
#'
#' @details
#' Accuracy can be computed as:
#'
#' ![](accuracy.png "(1 / N) * (sum(diag(confusion_matrix)))")
#'
#' that is, the sum of the diagonal in the confusion matrix (correct
#' classifications) over the total number of values in the matrix (N). An
#' equivalent but more efficient method is used.
#'
#' @return
#' A single numeric value with the accuracy.
#'
#' @family categorical_metrics
#'
#' @examples
#' \dontrun{
#' accuracy(c("a", "b"), c("a", "b"))
#' accuracy(c("a", "b"), c("b", "a"))
#' accuracy(c("a", "b"), c("b", "b"))
#' accuracy(c("a", "b", "a"), c("b", "a", "c"))
#' }
#'
#' @export
accuracy <- function(observed, predicted, remove_na = TRUE) {
  return(pccc(observed, predicted, remove_na = remove_na))
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
pcic <- function(observed, predicted, remove_na = TRUE) {
  assert_categorical_obs_pred(observed, predicted)


  return(mean(
    as.character(observed) != as.character(predicted),
    na.rm = remove_na
  ))
}

#' @title Brier Score
#'
#' @description
#' Given the observed values and the predicted probabilites of categorical data
#' (of at least two classes) computes the Brier Score.
#'
#' @inheritParams roc_auc
#'
#' @details
#' Brier score is computed as:
#'
#' ![](brier_score.png "(1 / N) *  sum(sum((f_{ti} - o_{ti})^2))")
#'
#' Where R is the number of possible classes in which the event can fall, and N
#' the overall number of instances of all classes. f_{ti} is the predicted
#' probability for class i. o_{ti} is 1 if it is i-th class in instant t, 0
#' otherwise.
#'
#' @return
#' A single numeric value with the Brier Score.
#'
#' @family categorical_metrics
#'
#' @examples
#' \dontrun{
#' probs <- data.frame(a = c(0.7, 0.2), b = c(0.3, 0.8))
#' brier_score(factor(c("a", "b")), probs)
#'
#' probs <- data.frame(
#'   a = c(0.2, 0.8, 0.3),
#'   b = c(0.3, 0.1, 0.3),
#'   c = c(0.5, 0.1, 0.4),
#' )
#'
#' brier_score(factor(c("a", "a", "c")), probs)
#'
#'
#' probs <- data.frame(a = 1, b = 0)
#' brier_score("a", probs)
#' }
#'
#' @export
brier_score <- function(observed, probabilities, remove_na = TRUE) {
  assert_observed_probabilities(observed, probabilities)

  if (!remove_na && (anyNA(observed) || anyNA(probabilities))) {
    return(NaN)
  }

  if (remove_na) {
    remove_indices <- nas_indices(observed, probabilities)

    if (!is_empty(remove_indices)) {
      observed <- get_records(observed, -remove_indices)
      probabilities <- get_records(probabilities, -remove_indices)
    }
  }

  observed <- factor(observed, levels = colnames(probabilities))
  observed_dummy <- model.matrix(~0 + observed)
  probabilities <- data.matrix(probabilities)

  return(mean(rowSums((probabilities - observed_dummy)^2)))
}

# For continuous data --------------------------------------------------

#' @title Mean Squared Error
#'
#' @description
#' Given the observed and predicted values of numeric data computes the Mean
#' Squared Error.
#'
#' @param observed (`numeric`) The observed values. It has to have the same
#'   length as `predicted`.
#' @param predicted (`numeric`) The observed values. It has to have the same
#'   length as `observed`.
#' @param remove_na (`logical(1)`) Should `NA` values be removed?. `TRUE` by
#'   default.
#'
#' @details
#' Mean Squared Error is computed as:
#'
#' ![](mse.png "(1 / N) * sum((observed - predicted)^2)")
#'
#' where y_i is the observed value of element i, hat{y_i} is the predicted
#' value of element i and N is the total number of elements.
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
mse <- function(observed, predicted, remove_na = TRUE) {
  assert_same_length(observed, predicted)

  return(mean((
    as.numeric(observed) - as.numeric(predicted))^2,
    na.rm = remove_na
  ))
}

#' @title Root Mean Squared Error
#'
#' @description
#' Given the observed and predicted values of numeric data computes the Root
#' Mean Squared Error.
#'
#' @inheritParams mse
#'
#' @details
#' Root Mean Squared Error is computed as:
#'
#' ![](rmse.png "sqrt((1 / N) * sum((observed - predicted)^2))")
#'
#' where y_i is the observed value of element i, hat{y_i} is the predicted
#' value of element i and N is the total number of elements.
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
rmse <- function(observed, predicted, remove_na = TRUE) {
  return(sqrt(mse(observed, predicted, remove_na = remove_na)))
}

#' @title Normalized Root Mean Squared Error
#'
#' @description
#' Given the observed and predicted values of numeric data computes the
#' Normalized Root Mean Squared Error.
#'
#' @inheritParams mse
#' @param type (`character(1)`) (case not sensitive) The normalization type to
#'   use. The options are `"sd"`, `"mean"`, `"maxmin"` (or `"range"`) and
#'   `"iqr"` (for more information, see Details section below). `"sd"` by
#'   default.
#'
#' @details
#' Normalized Root Mean Squared Error is computed as:
#'
#' ![](nrmse.png "sqrt((1 / N) * sum((observed - predicted)^2)) / norm")
#'
#' where y_i is the observed value of element i, hat{y_i} is the predicted
#' value of element i, N is the total number of elements and Y' is the
#' normalized observed values. You can specify one of the following types of
#' normalization with the `type` parameter:
#'
#' * `"sd"`: Standard deviation.
#' * `"mean"`: Mean.
#' * `"maxmin"` or `"range"`: Maximun minus minimum (range).
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
nrmse <-  function(observed, predicted, type = "sd", remove_na = TRUE) {
  rmse_value <- rmse(observed, predicted)
  if (is.nan(rmse_value) || is.na(rmse_value)) {
    return(rmse_value)
  }

  lower_type <- tolower(type)

  divisor <- NULL

  if (lower_type == "sd") {
    divisor <- sd(observed, na.rm = remove_na)
  } else if (lower_type == "mean") {
    divisor <- mean(observed, na.rm = remove_na)
  } else if (lower_type == "maxmin" || lower_type == "range") {
    divisor <- diff(range(observed, na.rm = remove_na))
  } else if (lower_type == "iqr") {
    divisor <- diff(quantile(observed, c(0.25, 0.75), na.rm = remove_na))
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
#' @details
#' Mean Absolute Error is computed as:
#'
#' ![](mae.png "mean(abs(observed - predicted))")
#'
#' where y_i is the observed value of element i, hat{y_i} is the predicted
#' value of element i and N is the total number of elements.
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
mae <- function(observed, predicted, remove_na = TRUE) {
  assert_same_length(observed, predicted)

  return(mean(abs(observed - predicted), na.rm = remove_na))
}

#' @title Mean Arctangent Absolute Percentage Error
#'
#' @description
#' Given the observed and predicted values of numeric data computes the
#' Mean Arctangent Absolute Percentage Error.
#'
#' @inheritParams mse
#'
#' @details
#' Mean Arctangent Absolute Percentage Error is computed as:
#'
#' ![](maape.png "mean(atan(abs(observed - predicted) / abs(observed)))")
#'
#' where y_i is the observed value of element i, hat{y_i} is the predicted
#' value of element i and N is the total number of elements.
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
maape <- function(observed, predicted, remove_na = TRUE) {
  assert_same_length(observed, predicted)

  if (is.null(observed) && is.null(predicted)) {
    return(NaN)
  }

  return(mean(
    atan(abs(observed - predicted) / abs(observed)),
    na.rm = remove_na
  ))
}

nmaape <- function(observed, predicted, remove_na = TRUE) {
  return(
    maape(observed, predicted, remove_na = remove_na) /
    # Normalization with respect of the max value
    (pi / 2)
  )
}

#' @title Spearman's correlation coefficient
#'
#' @description
#' Computes the Spearman's correlation.
#'
#' @param x (`numeric` | `matrix`) The values to calculate the correlation.
#' @param y (`numeric` | `matrix`) The values to calculate the correlation with.
#'   the same values as `x` by default.
#' @inheritParams mse
#'
#' @details
#' Spearman's correlation coefficient is computed as:
#'
#' ![](spearman.png "cov(rank(x), rank(y)) / (sd(rank(x)) * sd(rank(y)))")
#'
#' where R is the rank of a variable, cov the covariance of two variables and
#' sigma is the standard deviation.
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
spearman <- function(x, y = x, remove_na = TRUE) {
  if (remove_na) {
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

#' @title Pearson's correlation coefficient
#'
#' @description
#' Computes the Pearson's correlation.
#'
#' @param x (`numeric` | `matrix`) The values to calculate the correlation.
#' @param y (`numeric` | `matrix`) The values to calculate the correlation with.
#'   the same values as `x` by default.
#' @inheritParams mse
#'
#' @details
#' Pearson's correlation coefficient is computed as:
#'
#' ![](pearson.png "cov(x, y) / (sd(x) * sd(y))")
#'
#' where cov is the covariance of two variables and sigma is the standard
#' deviation.
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
pearson <- function(x, y = x, remove_na = TRUE) {
  if (remove_na) {
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
      loss_function <- nmaape
    }
    current_value <- loss_function(
      observed[, response_name],
      predicted[[response_name]]$predicted
    )
    all_metrics <- c(all_metrics, current_value)
  }

  return(mean(all_metrics, na.rm = TRUE))
}

get_loss_function <- function(loss_function, responses, is_multivariate) {
  if (is_multivariate) {
    if (!is.null(loss_function)) {
      warning("loss_function is ignored always in multivariate analysis.")
    }

    return("multivariate_loss")
  }

  if (is.null(loss_function)) {
    if (is_class_response(responses[[1]]$type)) {
      return("accuracy")
    } else if (is_numeric_response(responses[[1]]$type)) {
      return("mse")
    }
  }

  if (
    is_binary_response(responses[[1]]$type) &&
    !is_binary_loss(loss_function)
  ) {
    warning(
      loss_function,
      " is not a valid loss function for binary response variables, ",
      "accuracy set instead."
    )
    loss_function <- "accuracy"
  } else if (
    is_numeric_response(responses[[1]]$type) &&
    !is_numeric_loss(loss_function)
  ) {
    warning(
      loss_function,
      " is not a valid loss function for numeric response variables, ",
      "mse set instead."
    )
    loss_function <- "mse"
  } else if (
    is_categorical_response(responses[[1]]$type) &&
    !is_categorical_loss(loss_function)
  ) {
    warning(
      loss_function,
      " is not a valid loss function for categorical response variables, ",
      "accuracy set instead."
    )
    loss_function <- "accuracy"
  }

  return(loss_function)
}

wrapper_loss <- function(observed,
                         predictions,
                         tuner) {
  if (tuner$is_multivariate) {
    loss <- tuner$loss_function(
      observed = observed,
      predicted = predictions,
      responses = tuner$responses
    )
  } else {
    # For GBM that needs the response in 0, 1 format
    if (
      !tuner$is_multivariate &&
      is_binary_response(tuner$responses$y$type) &&
      !is.factor(observed)
    ) {
      observed <- tuner$responses$y$levels[observed + 1]
      observed <- factor(observed, levels = tuner$responses$y$levels)
    }

    if (
      tuner$loss_function_name == "roc_auc" ||
      tuner$loss_function_name == "pr_auc"
    ) {
      positive_class <- tuner$responses$y$levels[2]
      loss <- tuner$loss_function(
        observed = observed,
        probabilities = predictions$probabilities,
        positive_class = positive_class
      )
    } else {
      loss <- tuner$loss_function(
        observed = observed,
        predicted = predictions$predicted
      )
    }
  }

  if (need_invert_loss(tuner$loss_function_name)) {
    loss <- loss * -1
  }

  return(loss)
}

# Summary --------------------------------------------------

#' @title Categorical summary
#'
#' @description
#' Given the observed and predicted values of categorical data (of any number of
#' classes) computes the confusion matrix, Kappa's coefficient, Matthews'
#' correlation coefficient, accuracy, sensitivity, specificity, precision, F1
#' score and Brier's score.
#'
#' @inheritParams confusion_matrix
#' @inheritParams brier_score
#'
#' @return
#' A list with the confusion_matrix and all metrics. Matthews' correlation
#' coefficient is only returned for binary data and Brier's score when the
#' probabilities matrix is provided.
#'
#' @examples
#' \dontrun{
#' categorical_summary(c("a", "b"), c("a", "b"))
#' categorical_summary(c("a", "b"), c("b", "a"))
#' categorical_summary(c("a", "b", "a"), c("b", "a", "c"))
#'
#' example <- data.frame(
#'   observed = c("a", "a", "a", "a", "b", "b", "b", "b", "c", "c", "c", "c"),
#'   predicted = c("a", "a", "b", "c", "a", "b", "b", "c", "a", "b", "b", "c"),
#'   # Probabilities
#'   a = c(
#'     0.2377, 0.2924, 0.0406, 0.1893, 0.3978, 0.1965,
#'     0.0673, 0.2796, 0.1921, 0.2020, 0.1752, 0.3428
#'   ),
#'   b = c(
#'     0.0432, 0.1948, 0.0835, 0.3969, 0.0749, 0.0250,
#'     0.1507, 0.0752, 0.3952, 0.0807, 0.3097, 0.1282
#'   ),
#'   c = c(
#'     0.7190, 0.5126, 0.8757, 0.4136, 0.5272, 0.7783,
#'     0.7818, 0.6451, 0.4125, 0.7172, 0.5150, 0.5288
#'   )
#' )
#' categorical_summary(
#'   example$observed,
#'   example$predicted,
#'   probabilities = example[, c("a", "b", "c")]
#' )
#' }
#'
#' @family categorical_metrics
#'
#' @export
categorical_summary <- function(observed,
                                predicted,
                                probabilities = NULL,
                                positive_class = NULL,
                                remove_na = TRUE) {
  summary <- list(
    confusion_matrix = confusion_matrix(
      observed,
      predicted,
      remove_na = remove_na
    ),
    kappa_coeff = kappa_coeff(
      observed,
      predicted,
      remove_na = remove_na
    ),
    sensitivity = sensitivity(
      observed,
      predicted,
      positive_class = positive_class,
      remove_na = remove_na
    ),
    specificity = specificity(
      observed,
      predicted,
      positive_class = positive_class,
      remove_na = remove_na
    ),
    precision = precision(
      observed,
      predicted,
      positive_class = positive_class,
      remove_na = remove_na
    ),
    f1_score = f1_score(
      observed,
      predicted,
      positive_class = positive_class,
      remove_na = remove_na
    ),
    accuracy = accuracy(
      observed,
      predicted,
      remove_na = remove_na
    )
  )

  if (ncol(summary$confusion_matrix) == 2) {
    summary$matthews_coeff <- matthews_coeff(
      observed,
      predicted,
      remove_na = remove_na
    )

    if (!is.null(probabilities)) {
      summary$roc_auc <- roc_auc(
        observed,
        probabilities,
        positive_class = positive_class,
        remove_na = remove_na
      )

      summary$pr_auc <- pr_auc(
        observed,
        probabilities,
        positive_class = positive_class,
        remove_na = remove_na
      )
    }
  }

  if (!is.null(probabilities)) {
    summary$brier_score <- brier_score(
      observed,
      probabilities = probabilities,
      remove_na = remove_na
    )
  }

  class(summary) <- "CategoricalSummary"

  return(summary)
}

#' @export
print.CategoricalSummary <- function(summary, digits = 4) {
  cat("* Confusion matrix:\n")
  print(summary$confusion_matrix)
  cat(sprintf(
    "\n* Kappa coefficient: %s\n",
    round(summary$kappa_coeff, digits)
  ))
  cat(sprintf("* Accuracy: %s\n", round(summary$accuracy, digits)))

  if (!is.null(summary$matthews_coeff)) {
    cat(sprintf(
      "* Matthews correlation coefficient: %s\n",
      round(summary$matthews_coeff, digits)
    ))
  }

  if (!is.null(summary$roc_auc)) {
    cat(sprintf("* ROC-AUC: %s\n", round(summary$roc_auc, digits)))
  }

  if (!is.null(summary$pr_auc)) {
    cat(sprintf("* PR-AUC: %s\n", round(summary$pr_auc, digits)))
  }

  if (!is.null(summary$brier_score)) {
    cat(sprintf(
      "* Brier Score: %s\n",
      round(summary$brier_score, digits)
    ))
  }

  all_classes <- colnames(summary$confusion_matrix)

  if (length(all_classes) == 2) {
    cat(sprintf("* Sensitivity: %s\n", round(summary$sensitivity, digits)))
    cat(sprintf("* Specificity: %s\n", round(summary$specificity, digits)))
    cat(sprintf("* Precision: %s\n", round(summary$precision, digits)))
    cat(sprintf("* F1 score: %s\n", round(summary$f1_score, digits)))
  } else {
    table_summary <- matrix(
      c(
        summary$sensitivity, summary$specificity,
        summary$precision, summary$f1_score),
      nrow = 4,
      ncol = length(all_classes),
      byrow = TRUE
    )
    colnames(table_summary) <- all_classes
    rownames(table_summary) <- c(
      "* Sensitivity",
      "* Specificity",
      "* Precision",
      "* F1 score"
    )
    table_summary <- round(table_summary, digits)

    cat("\n")
    print(table_summary)
  }

  return(invisible(summary))
}

#' @title Numeric summary
#'
#' @description
#' Given the observed and predicted values of numeric data computes the Mean
#' Squared Error (MSE), Root Mean Squared Error (RMSE), Normalized Root Mean
#' Squared Error (NRMSE), Mean Arctangent Absolute Percentage Error (MAAPE),
#' Mean Absolute Error (MAE) and Person's correlation
#'
#' @inheritParams mse
#'
#' @return
#' A list with all metrics.
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' x <- rnorm(100)
#' numeric_summary(x, x)
#' numeric_summary(x, x - 1)
#' numeric_summary(x, x + 10)
#' }
#'
#' @family numeric_metrics
#'
#' @export
numeric_summary <- function(observed, predicted, remove_na = TRUE) {
  summary <- list(
    mse = mse(observed, predicted, remove_na = remove_na),
    rmse = rmse(observed, predicted, remove_na = remove_na),
    nrmse = nrmse(observed, predicted, remove_na = remove_na),
    maape = maape(observed, predicted, remove_na = remove_na),
    mae = mae(observed, predicted, remove_na = remove_na),
    pearson = pearson(observed, predicted, remove_na = remove_na)
  )

  class(summary) <- "NumericSummary"

  return(summary)
}

#' @export
print.NumericSummary <- function(summary, digits = 4) {
  cat(sprintf("* MSE: %s\n", round(summary$mse, digits)))
  cat(sprintf("* RMSE: %s\n", round(summary$rmse, digits)))
  cat(sprintf("* NRMSE: %s\n", round(summary$nrmse, digits)))
  cat(sprintf("* MAAPE: %s\n", round(summary$maape, digits)))
  cat(sprintf("* MAE: %s\n", round(summary$mae, digits)))
  cat(sprintf("* Pearson correlation: %s\n", round(summary$pearson, digits)))
}
