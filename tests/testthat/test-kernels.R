n_rows <- 50
n_cols <- 50

x <- matrix(rnorm(n_rows * n_cols, 100, 20), nrow = n_rows, ncol = n_cols)

test_that("Conventional", {
  expect_matrix(
    conventional_kernel(x, kernel = "linear"),
    any.missing = FALSE,
    nrows = n_rows,
    ncols = n_rows
  )

  expect_matrix(
    conventional_kernel(x, kernel = "polynomial"),
    any.missing = FALSE,
    nrows = n_rows,
    ncols = n_rows
  )

  expect_matrix(
    conventional_kernel(x, kernel = "sigmoid"),
    any.missing = FALSE,
    nrows = n_rows,
    ncols = n_rows
  )

  expect_matrix(
    conventional_kernel(x, kernel = "gaussian"),
    any.missing = FALSE,
    nrows = n_rows,
    ncols = n_rows
  )

  expect_matrix(
    conventional_kernel(x, kernel = "exponential"),
    any.missing = FALSE,
    nrows = n_rows,
    ncols = n_rows
  )

  expect_matrix(
    conventional_kernel(x, kernel = "arc_cosine"),
    any.missing = FALSE,
    nrows = n_rows,
    ncols = n_rows
  )

  expect_matrix(
    conventional_kernel(x, kernel = "arc_cosine", arc_cosine_deep = 4),
    any.missing = FALSE,
    nrows = n_rows,
    ncols = n_rows
  )
})

test_that("Sparse", {
  rows_proportion <- 0.5
  expected_cols <- n_rows * rows_proportion

  expect_matrix(
    sparse_kernel(x, kernel = "linear", rows_proportion = rows_proportion),
    any.missing = FALSE,
    nrows = n_rows,
    max.cols = expected_cols
  )

  expect_matrix(
    sparse_kernel(x, kernel = "polynomial", rows_proportion = rows_proportion),
    any.missing = FALSE,
    nrows = n_rows,
    max.cols = expected_cols
  )

  expect_matrix(
    sparse_kernel(
      scale(x),
      kernel = "sigmoid",
      rows_proportion = rows_proportion
    ),
    any.missing = FALSE,
    nrows = n_rows,
    max.cols = expected_cols
  )

  expect_matrix(
    sparse_kernel(x, kernel = "gaussian", rows_proportion = rows_proportion),
    any.missing = FALSE,
    nrows = n_rows,
    max.cols = expected_cols
  )

  expect_matrix(
    sparse_kernel(x, kernel = "exponential", rows_proportion = rows_proportion),
    any.missing = FALSE,
    nrows = n_rows,
    max.cols = expected_cols
  )

  expect_matrix(
    sparse_kernel(x, kernel = "arc_cosine", rows_proportion = rows_proportion),
    any.missing = FALSE,
    nrows = n_rows,
    max.cols = expected_cols
  )

  expect_matrix(
    sparse_kernel(
      x,
      kernel = "arc_cosine",
      arc_cosine_deep = 4,
      rows_proportion = rows_proportion
    ),
    any.missing = FALSE,
    nrows = n_rows,
    max.cols = expected_cols
  )
})