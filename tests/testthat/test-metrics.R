set.seed(1)

numeric_vars <- list(
  observed = round(rnorm(5, 10, 1), 2)
)
numeric_vars$predicted <- round(numeric_vars$observed + rnorm(5), 2)

categorical_vars <- list(
  observed = factor(c("A", "B", "C", "A", "B", "A", "C")),
  predicted = factor(c("A", "C", "C", "B", "B", "A", "C"))
)

test_that("confusion_matrix", {
  true_matrix <- matrix(c(2, 1, 0, 0, 1, 1, 0, 0, 2), nrow = 3, byrow = TRUE)
  colnames(true_matrix) <- c("A", "B", "C")
  rownames(true_matrix) <- colnames(true_matrix)

  expect_identical(
    all(
      confusion_matrix(
        categorical_vars$observed,
        categorical_vars$predicted
      ) == true_matrix
    ),
    TRUE
  )

  true_matrix <- matrix(c(0, 1, 0, 0), nrow = 2, byrow = TRUE)
  colnames(true_matrix) <- c("a", "b")
  rownames(true_matrix) <- colnames(true_matrix)

  expect_identical(
    all(confusion_matrix(factor("a"), factor("b")) == true_matrix),
    TRUE
  )

  temp <- factor(sample(c("versicolor", "setosa"), nrow(iris), replace = TRUE))
  conf <- confusion_matrix(iris$Species, temp)
  expect_matrix(conf, nrows = 3, ncols = 3)
  expect_names(rownames(conf), permutation.of = levels(iris$Species))
  expect_names(colnames(conf), permutation.of = levels(iris$Species))

  conf <- confusion_matrix(factor(c("A", "B")), factor(c("C", "D")))
  expect_matrix(conf, nrows = 4, ncols = 4)
  expect_names(rownames(conf), permutation.of = c("A", "B", "C", "D"))
  expect_names(colnames(conf), permutation.of = c("A", "B", "C", "D"))

  conf <- confusion_matrix(
    factor(c("A", "B")),
    factor(c("A", "B"), levels = c("A", "B", "C", "D"))
  )
  expect_matrix(conf, nrows = 4, ncols = 4)
  expect_names(rownames(conf), permutation.of = c("A", "B", "C", "D"))
  expect_names(colnames(conf), permutation.of = c("A", "B", "C", "D"))

  conf <- confusion_matrix(factor(c("a", "b")), factor(c("b", "b")))
  true_matrix <- matrix(c(0, 1, 0, 1), nrow = 2, byrow = TRUE)
  colnames(true_matrix) <- c("a", "b")
  rownames(true_matrix) <- colnames(true_matrix)

  expect_identical(all(conf == true_matrix), TRUE)
})

test_that("math_mode", {
  mode_result <- function(value, frequency) {
    result <- as.character(value)
    attr(result, "frequency") <- as.integer(frequency)

    return(result)
  }

  expect_equal(math_mode(factor(1)), mode_result(1, 1))
  expect_equal(math_mode(factor(c(1, 2))), mode_result(c(1, 2), 1))
  expect_equal(math_mode(factor(c(1, 2, 3, 1, 1))), mode_result(1, 3))
  expect_equal(math_mode(iris$Species), mode_result(levels(iris$Species), 50))
  expect_equal(math_mode(
    factor(c("C", "A", "C", "A"))),
    mode_result(c("A", "C"), 2)
  )

  expect_equal(math_mode(factor(NA)), NULL)
  expect_equal(math_mode(factor(NA), remove_na = FALSE), mode_result(NA, 1))
  x <- factor(c(rep("A", 100), rep(NA, 100)))
  expect_equal(math_mode(x), mode_result("A", 100))
  expect_equal(math_mode(x, remove_na = FALSE), mode_result(c("A", NA), 100))
  x <- factor(c("A", "B", "A", NA, "B", "A", "B", NA, NA, NA, "C"))
  expect_equal(math_mode(x), mode_result(c("A", "B"), 3))
  expect_equal(math_mode(x, remove_na = FALSE), mode_result(NA, 4))

  x <- factor(c("A", "B", "A", NA, "B", "A", "B", NA, NA, NA, "A", "B"))
  expect_equal(math_mode(x, allow_multimodal = FALSE), mode_result(c("A"), 4))
  expect_equal(
    math_mode(x, allow_multimodal = FALSE, remove_na = FALSE),
    mode_result(c("A"), 4)
  )
  expect_equal(
    math_mode(factor(rep("A", 10)), allow_multimodal = FALSE),
    mode_result(c("A"), 10)
  )
})

test_that("kappa_coef", {
  expect_identical(kappa_coeff(factor(c("a", "b")), factor(c("a", "b"))), 1)
  expect_identical(
    kappa_coeff(factor(rep(c("a", "b"), 20)), factor(rep(c("a", "b"), 20))),
    1
  )

  coeff <- kappa_coeff(categorical_vars$observed, categorical_vars$predicted)
  expect_equal(round(coeff, 4), 0.5758)

  expect_identical(kappa_coeff(factor(c("a", "b")), factor(c("b", "b"))), 0)

  expect_identical(kappa_coeff(factor(c("a", "b")), factor(c("b", "a"))), -1)
  expect_identical(kappa_coeff(factor(c("a", "b")), factor(c("b", "a"))), -1)
  expect_identical(
    kappa_coeff(factor(rep(c("a", "b"), 20)), factor(rep(c("b", "a"), 20))),
    -1
  )

  expect_identical(kappa_coeff(factor("a"), factor("a")), NaN)
  expect_identical(kappa_coeff(factor("a"), factor("b")), 0)
})

test_that("matthews_coeff", {
  expect_identical(matthews_coeff(factor(c("a", "b")), factor(c("a", "b"))), 1)
  expect_identical(
    matthews_coeff(factor(rep(c("a", "b"), 20)), factor(rep(c("a", "b"), 20))),
    1
  )

  coeff <- matthews_coeff(
    factor(c(1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0)),
    factor(c(0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1))
  )
  expect_equal(round(coeff, 4), 0.4781)

  expect_identical(
    matthews_coeff(factor(c("a", "b")), factor(c("b", "b"))),
    NaN
  )
  expect_identical(matthews_coeff(factor(c("a", "b")), factor(c("b", "a"))), -1)
  expect_identical(
    matthews_coeff(factor(rep(c("a", "b"), 20)), factor(rep(c("b", "a"), 20))),
    -1
  )

  expect_error(
    matthews_coeff(factor("a"), factor("a")),
    "Matthews correlation coefficient \\(MCC\\) is only for binary variables"
  )
  expect_identical(matthews_coeff(factor("a"), factor("b")), NaN)
})

test_that("pccc", {
  expect_identical(pccc(factor("a"), factor("a")), 1)
  expect_identical(pccc(factor(1), factor(1)), 1)
  expect_identical(pccc(factor(1), factor(2)), 0)
  expect_identical(pccc(factor("a"), factor("b")), 0)
  expect_identical(pccc(factor(c("a", "b")), factor(c("b", "b"))), 0.5)
  expect_identical(
    pccc(factor(c("a", NA, "b", "c")), factor(c("b", "b", "b", NA))),
    0.5
  )

  expect_identical(pccc(factor(rep("a", 20)), factor(rep("a", 20))), 1)
  expect_identical(pccc(factor(rep("a", 20)), factor(rep("b", 20))), 0)

  expect_identical(
    pccc(categorical_vars$observed, categorical_vars$predicted),
    5 / 7
  )

  expect_error(
    pccc(factor("a"), factor(c("a", "b"))),
    "observed and predicted must have the same length"
  )

  x <- factor(c("A", "A", "C", "A", "C", "C"), levels = c("A", "C"))
  y <- factor(c("A", "A", "C", "A", "C", "C"), levels = c("A", "B", "C"))
  expect_identical(pccc(x, y), 1)
  y[c(2, 4, 5)] <- "B"
  expect_identical(pccc(x, y), 0.5)
})

test_that("sensitivity", {
  expect_identical(sensitivity(factor(c("a", "b")), factor(c("b", "b"))), 1)

  result <- c(NaN, 0.5, NaN)
  names(result) <- c("a", "b", "c")
  expect_identical(
    sensitivity(factor(c("a", NA, "b", "c")), factor(c("b", "b", "b", NA))),
    result
  )

  expect_identical(sensitivity(factor(rep("a", 20)), factor(rep("b", 20))), NaN)

  sensitivities <- sensitivity(
    categorical_vars$observed,
    categorical_vars$predicted
  )
  result <- c(1, 0.5, 0.6667)
  names(result) <- c("A", "B", "C")
  expect_equal(round(sensitivities, 4), result)

  expect_error(
    sensitivity("a", c("a", "b")),
    "observed and predicted must have the same length"
  )
  expect_error(
    sensitivity("a", NULL),
    "observed and predicted must have the same length"
  )

  x <- factor(c("A", "A", "C", "A", "C", "C"), levels = c("A", "C"))
  y <- factor(c("A", "A", "C", "A", "C", "C"), levels = c("A", "B", "C"))
  result <- c(1, 1, NaN)
  names(result) <- c("A", "C", "B")
  expect_identical(sensitivity(x, y), result)
  y[c(2, 4, 5)] <- "B"
  result <- c(1, 1, 0)
  names(result) <- c("A", "C", "B")
  expect_identical(sensitivity(x, y), result)

  x <- factor(c(rep("a", 4), rep("b", 4), rep("c", 4)))
  y <- factor(c(
    "a", "a", "b", "c",
    "a", "b", "b", "c",
    "a", "b", "b", "c"
  ))
  sensitivities <- sensitivity(x, y)
  result <- c(0.5, 0.4, 0.3333)
  names(result) <- c("a", "b", "c")
  expect_equal(round(sensitivities, 4), result)
})

test_that("specificity", {
  expect_identical(specificity(factor(c("a", "b")), factor(c("b", "b"))), 0)

  result <- c(0.5, NaN, 0)
  names(result) <- c("a", "b", "c")
  expect_identical(
    specificity(factor(c("a", NA, "b", "c")), factor(c("b", "b", "b", NA))),
    result
  )

  expect_identical(specificity(factor(rep("a", 20)), factor(rep("b", 20))), 0)

  specificities <- specificity(
    categorical_vars$observed,
    categorical_vars$predicted
  )
  result <- c(0.2, 0.2, 0)
  names(result) <- c("A", "B", "C")
  expect_equal(round(specificities, 4), result)

  expect_error(
    specificity("a", c("a", "b")),
    "observed and predicted must have the same length"
  )
  expect_error(
    specificity("a", NULL),
    "observed and predicted must have the same length"
  )

  x <- factor(c("A", "A", "C", "A", "C", "C"), levels = c("A", "C"))
  y <- factor(c("A", "A", "C", "A", "C", "C"), levels = c("A", "B", "C"))
  result <- c(0, 0, 0)
  names(result) <- c("A", "C", "B")
  expect_identical(specificity(x, y), result)
  y[c(2, 4, 5)] <- "B"
  result <- c(0.4, 0.25, 0)
  names(result) <- c("A", "C", "B")
  expect_equal(round(specificity(x, y), 4), result)

  x <- factor(c(rep("a", 4), rep("b", 4), rep("c", 4)))
  y <- factor(c(
    "a", "a", "b", "c",
    "a", "b", "b", "c",
    "a", "b", "b", "c"
  ))
  sensitivities <- specificity(x, y)
  result <- c(0.25, 0.2857, 0.3333)
  names(result) <- c("a", "b", "c")
  expect_equal(round(sensitivities, 4), result)
})

test_that("precision", {
  expect_identical(precision(factor(c("a", "b")), factor(c("b", "b"))), 0.5)

  result <- c(0, 1, 0)
  names(result) <- c("a", "b", "c")
  expect_identical(
    precision(factor(c("a", NA, "b", "c")), factor(c("b", "b", "b", NA))),
    result
  )

  expect_identical(precision(factor(rep("a", 20)), factor(rep("b", 20))), 0)

  precisions <- precision(
    categorical_vars$observed,
    categorical_vars$predicted
  )
  result <- c(0.4, 0.2, 0.4)
  names(result) <- c("A", "B", "C")
  expect_equal(round(precisions, 4), result)

  expect_error(
    precision("a", c("a", "b")),
    "observed and predicted must have the same length"
  )
  expect_error(
    precision("a", NULL),
    "observed and predicted must have the same length"
  )

  x <- factor(c("A", "A", "C", "A", "C", "C"), levels = c("A", "C"))
  y <- factor(c("A", "A", "C", "A", "C", "C"), levels = c("A", "B", "C"))
  result <- c(0.5, 0.5, 0)
  names(result) <- c("A", "C", "B")
  expect_identical(precision(x, y), result)
  y[c(2, 4, 5)] <- "B"
  result <- c(0.3333, 0.6667, 0)
  names(result) <- c("A", "C", "B")
  expect_equal(round(precision(x, y), 4), result)

  x <- factor(c(rep("a", 4), rep("b", 4), rep("c", 4)))
  y <- factor(c(
    "a", "a", "b", "c",
    "a", "b", "b", "c",
    "a", "b", "b", "c"
  ))
  precisions <- precision(x, y)
  result <- c(0.4, 0.4, 0.2)
  names(result) <- c("a", "b", "c")
  expect_equal(round(precisions, 4), result)
})

test_that("roc_auc", {
  probs <- data.frame(a = c(0.6, 0.6), b = c(0.4, 0.4))
  expect_identical(roc_auc(factor(c("a", "a")), probs), NaN)
  probs <- data.frame(a = c(0.6, 0.4), b = c(0.4, 0.6))
  expect_identical(roc_auc(factor(c("a", "b")), probs), 1)
  probs <- data.frame(a = c(0.4, 0.6), b = c(0.6, 0.4))
  expect_identical(roc_auc(factor(c("a", "b")), probs), 0)

  probs <- data.frame(
    a = c(0.2, 0.5, 0.9),
    b = c(0.3, 0.2, 0.05),
    c = c(0.5, 0.3, 0.05)
  )
  expect_error(
    roc_auc(factor(c("a", "b", "c")), probs),
    "Area Under the Curve \\(ROC-AUC\\) is only for binary variables"
  )

  set.seed(42)
  probs <- data.frame(a = rnorm(100))
  probs$b <- 1 - probs$a
  observed <- factor(sample(c("a", "b"), 100, replace = TRUE))

  expect_equal(round(roc_auc(observed, probs), 4), 0.5269)
})

test_that("pr_auc", {
  probs <- data.frame(a = c(0.6, 0.6), b = c(0.4, 0.4))
  expect_identical(pr_auc(factor(c("a", "a"), c("a", "b")), probs), NaN)
  probs <- data.frame(a = c(0.6, 0.4), b = c(0.4, 0.6))
  expect_identical(pr_auc(factor(c("a", "b")), probs), 0)
  probs <- data.frame(a = c(0.4, 0.6), b = c(0.6, 0.4))
  expect_identical(pr_auc(factor(c("a", "b")), probs), 0.25)

  probs <- data.frame(
    a = c(0.2, 0.5, 0.9),
    b = c(0.3, 0.2, 0.05),
    c = c(0.5, 0.3, 0.05)
  )
  expect_error(
    pr_auc(factor(c("a", "b", "c")), probs),
    paste0(
      "Precision-Recall Aure Under the Curve \\(PR-AUC\\) is only for binary, ",
      "variables."
    )
  )

  set.seed(42)
  probs <- data.frame(a = rnorm(100))
  probs$b <- 1 - probs$a
  observed <- factor(sample(c("a", "b"), 100, replace = TRUE))

  expect_equal(round(pr_auc(observed, probs), 4), 0.4688)
})

test_that("f1_score", {
  expect_identical(f1_score(factor(1), factor(2)), NaN)
  expect_identical(f1_score(factor("a"), factor("b")), NaN)
  expect_identical(
    round(f1_score(factor(c("a", "b")), factor(c("b", "b"))), 4),
    0.6667
  )

  result <- c(NaN, 0.6667, NaN)
  names(result) <- c("a", "b", "c")
  expect_equal(
    round(f1_score(
      factor(c("a", NA, "b", "c")),
      factor(c("b", "b", "b", NA))
    ), 4),
    result
  )

  expect_identical(f1_score(factor(rep("a", 20)), factor(rep("b", 20))), NaN)

  f1_scores <- f1_score(
    categorical_vars$observed,
    categorical_vars$predicted
  )
  result <- c(0.5714, 0.2857, 0.5)
  names(result) <- c("A", "B", "C")
  expect_equal(round(f1_scores, 4), result)

  expect_error(
    f1_score("a", c("a", "b")),
    "observed and predicted must have the same length"
  )
  expect_error(
    f1_score("a", NULL),
    "observed and predicted must have the same length"
  )

  x <- factor(c("A", "A", "C", "A", "C", "C"), levels = c("A", "C"))
  y <- factor(c("A", "A", "C", "A", "C", "C"), levels = c("A", "B", "C"))
  result <- c(0.6667, 0.6667, NaN)
  names(result) <- c("A", "C", "B")
  expect_identical(round(f1_score(x, y), 4), result)
  y[c(2, 4, 5)] <- "B"
  result <- c(0.5, 0.8, NaN)
  names(result) <- c("A", "C", "B")
  expect_equal(round(f1_score(x, y), 4), result)

  x <- factor(c(rep("a", 4), rep("b", 4), rep("c", 4)))
  y <- factor(c(
    "a", "a", "b", "c",
    "a", "b", "b", "c",
    "a", "b", "b", "c"
  ))
  precisions <- f1_score(x, y)
  result <- c(0.4444, 0.4, 0.25)
  names(result) <- c("a", "b", "c")
  expect_equal(round(precisions, 4), result)
})

test_that("pcic", {
  expect_identical(pcic(factor("a"), factor("a")), 0)
  expect_identical(pcic(factor(1), factor(1)), 0)
  expect_identical(pcic(factor(1), factor(2)), 1)
  expect_identical(pcic(factor("a"), factor("b")), 1)
  expect_identical(pcic(factor(c("a", "b")), factor(c("b", "b"))), 0.5)
  expect_identical(
    pcic(factor(c("a", NA, "b", "c")), factor(c("b", "b", "b", NA))),
    0.5
  )

  expect_identical(pcic(factor(rep("a", 20)), factor(rep("a", 20))), 0)
  expect_identical(pcic(factor(rep("a", 20)), factor(rep("b", 20))), 1)

  expect_identical(
    pcic(categorical_vars$observed, categorical_vars$predicted),
    2 / 7
  )

  expect_error(
    pcic("a", c("a", "b")),
    "observed and predicted must have the same length"
  )
  expect_error(
    pcic("a", NULL),
    "observed and predicted must have the same length"
  )

  x <- factor(c("A", "A", "C", "A", "C", "C"), levels = c("A", "C"))
  y <- factor(c("A", "A", "C", "A", "C", "C"), levels = c("A", "B", "C"))
  expect_identical(pcic(x, y), 0)
  y[c(2, 4, 5)] <- "B"
  expect_identical(pcic(x, y), 0.5)
})

test_that("brier_score", {
  probs <- data.frame(a = 1)
  expect_error(
    brier_score(factor("a"), probs),
    "probabilities must have at least two columns \\(classes\\)"
  )
  probs <- as.data.frame(matrix(c(1, 0), 1, 2))
  colnames(probs) <- NULL
  expect_error(
    brier_score(factor("a"), probs),
    "probabilities must have the classes' names as columns names"
  )
  probs <- as.data.frame(matrix(c(1, 0), 1, 2))
  colnames(probs) <- c("a", "b")
  expect_error(
    brier_score(factor(c("a", "b")), probs),
    "observed and probabilities must have the same number of records"
  )
  probs <- as.data.frame(matrix(c(1, 0, 1, 0), 2, 2))
  colnames(probs) <- c("a", "b")
  expect_error(
    brier_score(factor("b"), probs),
    "observed and probabilities must have the same number of records"
  )
  probs <- as.data.frame(matrix(c(1, 0, 1, 0, 1, 0), 3, 2))
  colnames(probs) <- c("a", "b")
  expect_error(
    brier_score(factor(c("b", "c", "a")), probs),
    paste0(
      "Assertion on 'observed' failed: Must be a subset of ",
      "\\{'a','b'\\}, but has additional elements \\{'c'\\}."
    )
  )

  # Manual calculation
  # x <- c(
  #   (0.7 - 1)^2 + (0.3 - 0)^2,
  #   (0.2 - 0)^2 + (0.8 - 1)^2
  # )
  # mean(x)
  probs <- as.data.frame(matrix(c(0.7, 0.3, 0.2, 0.8), 2, 2, byrow = TRUE))
  colnames(probs) <- c("0", "1")
  expect_identical(brier_score(factor(c(0, 1)), probs), 0.13)

  # Manual calculation
  # x <- c(
  #   (0.2 - 1)^2 + (0.3 - 0)^2 + (0.5 - 0)^2,
  #   (0.8 - 1)^2 + (0.1 - 0)^2 + (0.1 - 0)^2,
  #   (0.3 - 0)^2 + (0.3 - 0)^2 + (0.4 - 1)^2
  # )
  # mean(x)
  probs <- as.data.frame(matrix(
    c(0.2, 0.3, 0.5, 0.8, 0.1, 0.1, 0.3, 0.3, 0.4),
    3,
    3,
    byrow = TRUE
  ))
  colnames(probs) <- c("a", "b", "c")
  expect_identical(
    round(brier_score(factor(c("a", "a", "c")), probs), 4),
    0.5267
  )

  probs <- as.data.frame(matrix(c(1, 0), 1, 2))
  colnames(probs) <- c("a", "b")
  expect_identical(brier_score(factor("a"), probs), 0)

  probs <- as.data.frame(matrix(c(0, 1), 1, 2))
  colnames(probs) <- c("a", "b")
  expect_identical(brier_score(factor("a"), probs), 2)
})

test_that("mse", {
  expect_identical(mse(1, 1), 0)
  expect_identical(mse(1, 0), 1)
  expect_identical(mse(c(1, 2, NA, 4), c(1, NA, 3, 4)), 0)
  expect_identical(mse(c(5, 2, NA, 4), c(1, NA, 3, 2)), 10)

  expect_identical(
    round(mse(numeric_vars$observed, numeric_vars$predicted), 4),
    0.3785
  )
  expect_identical(
    round(mse(
      c(34, 37, 44, 47, 48, 48, 46, 43, 32, 27, 26, 24),
      c(37, 40, 46, 44, 46, 50, 45, 44, 34, 30, 22, 23)
    ), 4),
    5.9167
  )

  expect_error(
    mse(c(1, 2), 0),
    "observed and predicted must have the same length"
  )
  expect_error(
    mse(0, c(1, 2)),
    "observed and predicted must have the same length"
  )

  x <- rnorm(100)
  expect_identical(mse(x, x), 0)
  expect_identical(mse(x, x - 1), 1)
  expect_identical(mse(x, x + 10), 100)

  expect_identical(mse(NA, 1), NaN)
  expect_identical(mse(1, NA), NaN)
  expect_identical(mse(NA, NA), NaN)
  expect_identical(mse(NULL, NULL), NaN)
})

test_that("rmse", {
  expect_identical(rmse(1, 1), 0)
  expect_identical(rmse(1, 0), 1)
  expect_identical(rmse(c(1, 2, NA, 4), c(1, NA, 3, 4)), 0)
  expect_identical(round(rmse(c(5, 2, NA, 4), c(1, NA, 3, 2)), 4), 3.1623)

  expect_identical(
    round(rmse(numeric_vars$observed, numeric_vars$predicted), 4),
    0.6152
  )
  expect_identical(
    round(rmse(
      c(34, 37, 44, 47, 48, 48, 46, 43, 32, 27, 26, 24),
      c(37, 40, 46, 44, 46, 50, 45, 44, 34, 30, 22, 23)
    ), 4),
    2.4324
  )

  expect_error(
    rmse(c(1, 2), 0),
    "observed and predicted must have the same length"
  )
  expect_error(
    rmse(0, c(1, 2)),
    "observed and predicted must have the same length"
  )

  x <- rnorm(100)
  expect_identical(rmse(x, x), 0)
  expect_identical(rmse(x, x - 1), 1)
  expect_identical(rmse(x, x + 10), 10)

  expect_identical(rmse(NA, 1), NaN)
  expect_identical(rmse(1, NA), NaN)
  expect_identical(rmse(NA, NA), NaN)
  expect_identical(rmse(NULL, NULL), NaN)
})

test_that("nrmse", {
  expect_identical(nrmse(1, 1), as.numeric(NA))
  expect_identical(nrmse(1, 0), as.numeric(NA))

  expect_identical(nrmse(1, 1, type = "mean"), 0)
  expect_identical(nrmse(1, 0, type = "mean"), 1)
  expect_identical(nrmse(1, 1, type = "maxmin"), NaN)
  expect_identical(nrmse(1, 0, type = "maxmin"), NaN)
  expect_identical(nrmse(1, 1, type = "range"), NaN)
  expect_identical(nrmse(1, 0, type = "range"), NaN)
  expect_identical(nrmse(1, 1, type = "iqr"), NaN)
  expect_identical(nrmse(1, 0, type = "iqr"), NaN)

  expect_error(
    nrmse(1, 1, type = "maen"),
    "\\{\\'maen\\'\\} is not a valid type of normalization"
  )
  expect_error(
    nrmse(1, 0, type = "sds"),
    "\\{'sds'\\} is not a valid type of normalization"
  )

  expect_identical(nrmse(c(1, 2, NA, 4), c(1, NA, 3, 4)), 0)
  expect_identical(round(nrmse(c(5, 2, NA, 4), c(1, NA, 3, 2)), 4), 2.0702)

  expect_identical(
    round(nrmse(numeric_vars$observed, numeric_vars$predicted), 4),
    0.6378
  )
  expect_identical(
    round(nrmse(
      c(34, 37, 44, 47, 48, 48, 46, 43, 32, 27, 26, 24),
      c(37, 40, 46, 44, 46, 50, 45, 44, 34, 30, 22, 23)
    ), 4),
    0.266
  )

  expect_error(
    nrmse(c(1, 2), 0),
    "observed and predicted must have the same length"
  )
  expect_error(
    nrmse(0, c(1, 2)),
    "observed and predicted must have the same length"
  )

  set.seed(2)
  x <- rnorm(100)
  expect_identical(nrmse(x, x), 0)
  expect_identical(round(nrmse(x, x - 1), 4), 0.8619)
  expect_identical(round(nrmse(x, x + 10), 4), 8.6193)

  expect_identical(nrmse(NA, 1), NaN)
  expect_identical(nrmse(1, NA), NaN)
  expect_identical(nrmse(NA, NA), NaN)
  expect_identical(suppressWarnings(nrmse(NULL, NULL)), NaN)

  observed <- c(5.1, 8.5, 3, 2.3, 4.4)
  predicted <- c(5.8, 7, 2.2, 3, 4.4)
  rmse_value <- sqrt(mean((observed - predicted)^2))

  expect_identical(
    round(nrmse(observed, predicted, type = "sd"), 4),
    round(rmse_value / sd(observed), 4)
  )

  expect_identical(
    round(nrmse(observed, predicted, type = "mean"), 4),
    round(rmse_value / mean(observed), 4)
  )

  expect_identical(
    round(nrmse(observed, predicted, type = "maxmin"), 4),
    round(rmse_value / (max(observed) - min(observed)), 4)
  )

  iq_diff <- as.numeric(quantile(observed, 0.75) - quantile(observed, 0.25))
  expect_identical(
    round(nrmse(observed, predicted, type = "iqr"), 4),
    round(rmse_value / iq_diff, 4)
  )
})

test_that("mae", {
  expect_identical(mae(1, 1), 0)
  expect_identical(mae(1, 0), 1)
  expect_identical(mae(c(1, 2, NA, 4), c(1, NA, 3, 4)), 0)
  expect_identical(mae(c(5, 2, NA, 4), c(1, NA, 3, 2)), 3)

  expect_identical(
    round(mae(numeric_vars$observed, numeric_vars$predicted), 4),
    0.588
  )
  expect_identical(
    round(mae(
      c(34, 37, 44, 47, 48, 48, 46, 43, 32, 27, 26, 24),
      c(37, 40, 46, 44, 46, 50, 45, 44, 34, 30, 22, 23)
    ), 4),
    2.25
  )

  expect_error(
    mae(c(1, 2), 0),
    "observed and predicted must have the same length"
  )
  expect_error(
    mae(0, c(1, 2)),
    "observed and predicted must have the same length"
  )

  x <- rnorm(100)
  expect_identical(mae(x, x), 0)
  expect_identical(mae(x, x - 1), 1)
  expect_identical(mae(x, x + 10), 10)

  expect_identical(mae(NA, 1), NaN)
  expect_identical(mae(1, NA), NaN)
  expect_identical(mae(NA, NA), NaN)
  expect_identical(mae(NULL, NULL), NaN)
})

test_that("maape", {
  expect_identical(maape(1, 1), 0)
  expect_identical(round(maape(1, 0), 4), 0.7854)
  expect_identical(maape(c(1, 2, NA, 4), c(1, NA, 3, 4)), 0)
  expect_identical(
    round(maape(c(5, 2, NA, 4), c(1, NA, 3, 2)), 4),
    0.5692
  )

  expect_identical(
    round(maape(numeric_vars$observed, numeric_vars$predicted), 4),
    0.0592
  )
  expect_identical(
    round(maape(
      c(34, 37, 44, 47, 48, 48, 46, 43, 32, 27, 26, 24),
      c(37, 40, 46, 44, 46, 50, 45, 44, 34, 30, 22, 23)
    ), 4),
    0.0645
  )

  expect_error(
    maape(c(1, 2), 0),
    "observed and predicted must have the same length"
  )
  expect_error(
    maape(0, c(1, 2)),
    "observed and predicted must have the same length"
  )

  set.seed(5)
  x <- rnorm(100)
  expect_identical(maape(x, x), 0)
  expect_identical(round(maape(x, x - 1), 4), 1.0073)
  expect_identical(round(maape(x, x + 10), 4), 1.496)

  expect_identical(maape(NA, 1), NaN)
  expect_identical(maape(1, NA), NaN)
  expect_identical(maape(NA, NA), NaN)
  expect_identical(maape(NULL, NULL), NaN)
})

test_that("spearman", {
  expect_identical(spearman(1, 1), as.numeric(NA))
  expect_identical(spearman(1, 0), as.numeric(NA))
  expect_identical(round(spearman(c(1, 2, NA, 4), c(1, NA, 3, 4)), 4), 1)
  expect_identical(round(spearman(c(5, 2, NA, 4), c(1, NA, 3, 2)), 4), -1)

  expect_identical(
    round(spearman(numeric_vars$observed, numeric_vars$predicted), 4),
    0.8
  )
  expect_identical(
    round(spearman(
      c(34, 37, 44, 47, 48, 48, 46, 43, 32, 27, 26, 24),
      c(37, 40, 46, 44, 46, 50, 45, 44, 34, 30, 22, 23)
    ), 4),
    0.9438
  )

  expect_error(
    spearman(c(1, 2), 0),
    "x and y must have the same length"
  )
  expect_error(
    spearman(0, c(1, 2)),
    "x and y must have the same length"
  )

  x <- rnorm(100)
  expect_identical(spearman(x, x), 1)
  expect_identical(spearman(x, x - 1), 1)
  expect_identical(spearman(x, x + 10), 1)

  expect_identical(spearman(NA, 1), NaN)
  expect_identical(spearman(1, NA), NaN)
  expect_identical(spearman(NA, NA), NaN)
  expect_identical(spearman(NULL, NULL), NaN)
})

test_that("pearson", {
  expect_identical(pearson(1, 1), as.numeric(NA))
  expect_identical(pearson(1, 0), as.numeric(NA))
  expect_identical(round(pearson(c(1, 2, NA, 4), c(1, NA, 3, 4)), 4), 1)
  expect_identical(round(pearson(c(5, 2, NA, 4), c(1, NA, 3, 2)), 4), -1)

  expect_identical(
    round(pearson(numeric_vars$observed, numeric_vars$predicted), 4),
    0.8727
  )
  expect_identical(
    round(pearson(
      c(34, 37, 44, 47, 48, 48, 46, 43, 32, 27, 26, 24),
      c(37, 40, 46, 44, 46, 50, 45, 44, 34, 30, 22, 23)
    ), 4),
    0.9635
  )

  expect_error(
    pearson(c(1, 2), 0),
    "x and y must have the same length"
  )
  expect_error(
    pearson(0, c(1, 2)),
    "x and y must have the same length"
  )

  x <- rnorm(100)
  expect_identical(pearson(x, x), 1)
  expect_identical(pearson(x, x - 1), 1)
  expect_identical(pearson(x, x + 10), 1)

  expect_identical(pearson(NA, 1), NaN)
  expect_identical(pearson(1, NA), NaN)
  expect_identical(pearson(NA, NA), NaN)
  expect_identical(pearson(NULL, NULL), NaN)
})