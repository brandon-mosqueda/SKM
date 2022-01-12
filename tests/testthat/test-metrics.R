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
    all(confusion_matrix("a", "b") == true_matrix),
    TRUE
  )

  expect_identical(all(confusion_matrix(NA, "a") == 0), TRUE)
  expect_identical(nrow(confusion_matrix(NA, "a")), 1L)
  expect_identical(ncol(confusion_matrix(NA, "a")), 1L)

  expect_identical(all(confusion_matrix("a", NA) == 0), TRUE)
  expect_identical(nrow(confusion_matrix("a", NA)), 1L)
  expect_identical(ncol(confusion_matrix("a", NA)), 1L)

  expect_identical(nrow(confusion_matrix(NA, NA)), 0L)
  expect_identical(ncol(confusion_matrix(NA, NA)), 0L)

  temp <- factor(sample(c("versicolor", "setosa"), nrow(iris), replace = TRUE))
  conf <- confusion_matrix(iris$Species, temp)
  expect_matrix(conf, nrows = 3, ncols = 3)
  expect_names(rownames(conf), permutation.of = levels(iris$Species))
  expect_names(colnames(conf), permutation.of = levels(iris$Species))

  conf <- confusion_matrix(c("A", "B"), c("C", "D"))
  expect_matrix(conf, nrows = 4, ncols = 4)
  expect_names(rownames(conf), permutation.of = c("A", "B", "C", "D"))
  expect_names(colnames(conf), permutation.of = c("A", "B", "C", "D"))

  conf <- confusion_matrix(
    c("A", "B"),
    c("A", "B"),
    all_levels = c("A", "B", "C", "D")
  )
  expect_matrix(conf, nrows = 4, ncols = 4)
  expect_names(rownames(conf), permutation.of = c("A", "B", "C", "D"))
  expect_names(colnames(conf), permutation.of = c("A", "B", "C", "D"))

  conf <- confusion_matrix(c("a", "b"), c("b", "b"))
  true_matrix <- matrix(c(0, 1, 0, 1), nrow = 2, byrow = TRUE)
  colnames(true_matrix) <- c("a", "b")
  rownames(true_matrix) <- colnames(true_matrix)

  expect_identical(all(conf == true_matrix), TRUE)
})

test_that("kappa_coef", {
  expect_identical(kappa_coeff(c("a", "b"), c("a", "b")), 1)
  expect_identical(kappa_coeff(rep(c("a", "b"), 20), rep(c("a", "b"), 20)), 1)

  coeff <- kappa_coeff(categorical_vars$observed, categorical_vars$predicted)
  expect_equal(round(coeff, 4), 0.5758)

  expect_identical(kappa_coeff(c("a", "b"), c("b", "b")), 0)

  expect_identical(kappa_coeff(c("a", "b"), c("b", "a")), -1)
  expect_identical(kappa_coeff(c("a", "b"), c("b", "a")), -1)
  expect_identical(kappa_coeff(rep(c("a", "b"), 20), rep(c("b", "a"), 20)), -1)

  expect_identical(kappa_coeff("a", "a"), NaN)
  expect_identical(kappa_coeff(NA, "a"), NaN)
  expect_identical(kappa_coeff("a", NA), NaN)
  expect_identical(kappa_coeff("a", "b"), 0)
})

test_that("matthews_coeff", {
  expect_identical(matthews_coeff(c("a", "b"), c("a", "b")), 1)
  expect_identical(
    matthews_coeff(rep(c("a", "b"), 20), rep(c("a", "b"), 20)),
    1
  )

  coeff <- matthews_coeff(
    c(1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0),
    c(0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1)
  )
  expect_equal(round(coeff, 4), 0.4781)

  expect_identical(matthews_coeff(c("a", "b"), c("b", "b")), NaN)

  expect_identical(matthews_coeff(c("a", "b"), c("b", "a")), -1)
  expect_identical(matthews_coeff(c("a", "b"), c("b", "a")), -1)
  expect_identical(
    matthews_coeff(rep(c("a", "b"), 20), rep(c("b", "a"), 20)),
    -1
  )

  expect_identical(matthews_coeff("a", "a"), NaN)
  expect_identical(matthews_coeff(NA, "a"), NaN)
  expect_identical(matthews_coeff("a", NA), NaN)
  expect_identical(matthews_coeff("a", "b"), NaN)
})

test_that("pccc", {
  expect_identical(pccc("a", "a"), 1)
  expect_identical(pccc(1, 1), 1)
  expect_identical(pccc(1, 2), 0)
  expect_identical(pccc("a", "b"), 0)
  expect_identical(pccc(c("a", "b"), c("b", "b")), 0.5)
  expect_identical(pccc(c("a", NA, "b", "c"), c("b", "b", "b", NA)), 0.5)

  expect_identical(pccc(rep("a", 20), rep("a", 20)), 1)
  expect_identical(pccc(rep("a", 20), rep("b", 20)), 0)

  expect_identical(
    pccc(categorical_vars$observed, categorical_vars$predicted),
    5 / 7
  )

  expect_error(
    pccc("a", c("a", "b")),
    "observed and predicted must have the same length"
  )
  expect_error(
    pccc("a", NULL),
    "observed and predicted must have the same length"
  )

  x <- factor(c("A", "A", "C", "A", "C", "C"), levels = c("A", "C"))
  y <- factor(c("A", "A", "C", "A", "C", "C"), levels = c("A", "B", "C"))
  expect_identical(pccc(x, y), 1)
  y[c(2, 4, 5)] <- "B"
  expect_identical(pccc(x, y), 0.5)

  expect_identical(pccc(NA, "a"), NaN)
  expect_identical(pccc("a", NA), NaN)
  expect_identical(pccc(NA, NA), NaN)
  expect_identical(pccc(NULL, NULL), NaN)
})

test_that("sensitivity", {
  expect_identical(sensitivity("a", "a"), 1)
  expect_identical(sensitivity(1, 1), 1)
  expect_identical(sensitivity(1, 2), NaN)
  expect_identical(sensitivity("a", "b"), NaN)
  expect_identical(sensitivity(c("a", "b"), c("b", "b")), NaN)

  result <- c(NaN, 0.5, NaN)
  names(result) <- c("a", "b", "c")
  expect_identical(
    sensitivity(c("a", NA, "b", "c"), c("b", "b", "b", NA)),
    result
  )

  expect_identical(sensitivity(rep("a", 20), rep("a", 20)), 1)
  expect_identical(sensitivity(rep("a", 20), rep("b", 20)), NaN)

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

  expect_identical(sensitivity(NA, "a"), NaN)
  expect_identical(sensitivity("a", NA), NaN)
  expect_identical(sensitivity(NA, NA), NaN)
  expect_identical(sensitivity(NULL, NULL), NaN)

  x <- c(rep("a", 4), rep("b", 4), rep("c", 4))
  y <- c(
    "a", "a", "b", "c",
    "a", "b", "b", "c",
    "a", "b", "b", "c"
  )
  sensitivities <- sensitivity(x, y)
  result <- c(0.5, 0.4, 0.3333)
  names(result) <- c("a", "b", "c")
  expect_equal(round(sensitivities, 4), result)
})

test_that("specificity", {
  expect_identical(specificity("a", "a"), NaN)
  expect_identical(specificity(1, 1), NaN)
  expect_identical(specificity(1, 2), 0)
  expect_identical(specificity("a", "b"), 0)
  expect_identical(specificity(c("a", "b"), c("b", "b")), 0.5)

  result <- c(0.5, NaN, 0)
  names(result) <- c("a", "b", "c")
  expect_identical(
    specificity(c("a", NA, "b", "c"), c("b", "b", "b", NA)),
    result
  )

  expect_identical(specificity(rep("a", 20), rep("a", 20)), NaN)
  expect_identical(specificity(rep("a", 20), rep("b", 20)), 0)

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

  expect_identical(specificity(NA, "a"), NaN)
  expect_identical(specificity("a", NA), NaN)
  expect_identical(specificity(NA, NA), NaN)
  expect_identical(specificity(NULL, NULL), NaN)

  x <- c(rep("a", 4), rep("b", 4), rep("c", 4))
  y <- c(
    "a", "a", "b", "c",
    "a", "b", "b", "c",
    "a", "b", "b", "c"
  )
  sensitivities <- specificity(x, y)
  result <- c(0.25, 0.2857, 0.3333)
  names(result) <- c("a", "b", "c")
  expect_equal(round(sensitivities, 4), result)
})

test_that("precision", {
  expect_identical(precision("a", "a"), 1)
  expect_identical(precision(1, 1), 1)
  expect_identical(precision(1, 2), 0)
  expect_identical(precision("a", "b"), 0)
  expect_identical(precision(c("a", "b"), c("b", "b")), 0)

  result <- c(0, 1, 0)
  names(result) <- c("a", "b", "c")
  expect_identical(
    precision(c("a", NA, "b", "c"), c("b", "b", "b", NA)),
    result
  )

  expect_identical(precision(rep("a", 20), rep("a", 20)), 1)
  expect_identical(precision(rep("a", 20), rep("b", 20)), 0)

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

  expect_identical(precision(NA, "a"), NaN)
  expect_identical(precision("a", NA), NaN)
  expect_identical(precision(NA, NA), NaN)
  expect_identical(precision(NULL, NULL), NaN)

  x <- c(rep("a", 4), rep("b", 4), rep("c", 4))
  y <- c(
    "a", "a", "b", "c",
    "a", "b", "b", "c",
    "a", "b", "b", "c"
  )
  precisions <- precision(x, y)
  result <- c(0.4, 0.4, 0.2)
  names(result) <- c("a", "b", "c")
  expect_equal(round(precisions, 4), result)
})

test_that("pcic", {
  expect_identical(pcic("a", "a"), 0)
  expect_identical(pcic(1, 1), 0)
  expect_identical(pcic(1, 2), 1)
  expect_identical(pcic("a", "b"), 1)
  expect_identical(pcic(c("a", "b"), c("b", "b")), 0.5)
  expect_identical(pcic(c("a", NA, "b", "c"), c("b", "b", "b", NA)), 0.5)

  expect_identical(pcic(rep("a", 20), rep("a", 20)), 0)
  expect_identical(pcic(rep("a", 20), rep("b", 20)), 1)

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

  expect_identical(pcic(NA, "a"), NaN)
  expect_identical(pcic("a", NA), NaN)
  expect_identical(pcic(NA, NA), NaN)
  expect_identical(pcic(NULL, NULL), NaN)
})

test_that("brier_score", {
  probs <- matrix(1)
  colnames(probs) <- c("a")
  expect_error(
    brier_score("a", probs),
    "probabilities must have at least two columns \\(classes\\)"
  )
  probs <- matrix(c(1, 0), 1, 2)
  expect_error(
    brier_score("a", probs),
    "probabilities must have the classes' names as columns names"
  )
  probs <- matrix(c(1, 0), 1, 2)
  colnames(probs) <- c("a", "b")
  expect_error(
    brier_score(c("a", "b"), probs),
    "observed and probabilities must have the same number of records"
  )
  probs <- matrix(c(1, 0, 1, 0), 2, 2)
  colnames(probs) <- c("a", "b")
  expect_error(
    brier_score("b", probs),
    "observed and probabilities must have the same number of records"
  )
  probs <- matrix(c(1, 0, 1, 0, 1, 0), 3, 2)
  colnames(probs) <- c("a", "b")
  expect_error(
    brier_score(c("b", "c", "a"), probs),
    paste0(
      "Assertion on 'observed' failed: Must be a subset of ",
      "\\{'a','b'\\}, but is \\{'b','c','a'\\}."
    )
  )
  probs <- matrix(c(0, 1), 1, 2)
  colnames(probs) <- c("a", "b")
  expect_error(
    brier_score(NA, probs),
    paste0(
      "Assertion on 'observed' failed: Must be a subset of ",
      "\\{'a','b'\\}, not empty"
    )
  )

  # Manual calculation
  # x <- c(
  #   (0.7 - 1)^2 + (0.3 - 0)^2,
  #   (0.2 - 0)^2 + (0.8 - 1)^2
  # )
  # mean(x)
  probs <- matrix(c(0.7, 0.3, 0.2, 0.8), 2, 2, byrow = TRUE)
  colnames(probs) <- c("0", "1")
  expect_identical(brier_score(c(0, 1), probs), 0.13)

  # Manual calculation
  # x <- c(
  #   (0.2 - 1)^2 + (0.3 - 0)^2 + (0.5 - 0)^2,
  #   (0.8 - 1)^2 + (0.1 - 0)^2 + (0.1 - 0)^2,
  #   (0.3 - 0)^2 + (0.3 - 0)^2 + (0.4 - 1)^2
  # )
  # mean(x)
  probs <- matrix(
    c(0.2, 0.3, 0.5, 0.8, 0.1, 0.1, 0.3, 0.3, 0.4),
    3,
    3,
    byrow = TRUE
  )
  colnames(probs) <- c("a", "b", "c")
  expect_identical(
    round(brier_score(c("a", "a", "c"), probs), 4),
    0.5267
  )

  probs <- matrix(c(1, 0), 1, 2)
  colnames(probs) <- c("a", "b")
  expect_identical(brier_score("a", probs), 0)

  probs <- matrix(c(0, 1), 1, 2)
  colnames(probs) <- c("a", "b")
  expect_identical(brier_score("a", probs), 2)
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