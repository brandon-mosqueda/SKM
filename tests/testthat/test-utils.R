set.seed(1)

test_that("to_matrix", {
  categories <- factor(sample(c("A", "B", "C"), 10, replace = TRUE))

  expect_matrix(to_matrix(1), nrows = 1, ncols = 1, any.missing = FALSE)
  expect_matrix(to_matrix(1, TRUE), nrows = 1, ncols = 2, any.missing = FALSE)

  expect_matrix(to_matrix(NA), nrows = 1, ncols = 1)
  expect_matrix(to_matrix(NA, TRUE), nrows = 1, ncols = 2)

  expect_matrix(to_matrix(1:10), nrows = 10, ncols = 1)
  expect_matrix(to_matrix(1:10, TRUE), nrows = 10, ncols = 2)

  expect_matrix(
    to_matrix(categories),
    nrows = 10,
    ncols = 2,
    any.missing = FALSE
  )
  expect_matrix(
    to_matrix(categories, TRUE),
    nrows = 10,
    ncols = 3,
    any.missing = FALSE
  )

  expect_matrix(
    to_matrix(as.character(categories)),
    nrows = 10,
    ncols = 2,
    any.missing = FALSE
  )
  expect_matrix(
    to_matrix(as.character(categories), TRUE),
    nrows = 10,
    ncols = 3,
    any.missing = FALSE
  )

  expect_matrix(
    to_matrix(matrix(1:10, 2, 5)),
    nrows = 2,
    ncols = 5,
    any.missing = FALSE
  )
  expect_matrix(
    to_matrix(matrix(1:10, 2, 5), TRUE),
    nrows = 2,
    ncols = 6,
    any.missing = FALSE
  )

  expect_matrix(
    to_matrix(iris),
    nrows = nrow(iris),
    ncols = 6,
    any.missing = FALSE
  )
  expect_matrix(
    to_matrix(iris, TRUE),
    nrows = nrow(iris),
    ncols = 7,
    any.missing = FALSE
  )

  diverse_data <- data.frame(
    a = rnorm(10),
    b = rpois(10, 5),
    c = as.character(categories),
    d = categories
  )
  expect_matrix(
    to_matrix(diverse_data),
    nrows = nrow(diverse_data),
    ncols = 6,
    any.missing = FALSE
  )
  expect_matrix(
    to_matrix(diverse_data, TRUE),
    nrows = nrow(diverse_data),
    ncols = 7,
    any.missing = FALSE
  )
})
