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

test_that("remove_no_variance_cols", {
  # Matrix --------------------------------------------------

  n_rows <- 10
  n_cols <- 10
  x <- matrix(rnorm(n_rows * n_cols), n_rows, n_cols)
  test_indices <- cbind(
    sample(n_rows, 5),
    sample(n_cols, 5)
  )

  expect_matrix(
    remove_no_variance_cols(x),
    any.missing = FALSE,
    nrows = n_rows,
    ncols = n_cols
  )

  temp <- cbind(3, x, 0.5)
  expect_matrix(
    remove_no_variance_cols(temp),
    any.missing = FALSE,
    nrows = n_rows,
    ncols = n_cols
  )

  temp <- cbind(3, 5, 0.5)
  expect_numeric(
    remove_no_variance_cols(temp),
    len = 0
  )

  temp <- x
  temp[test_indices] <- NA
  expect_matrix(
    remove_no_variance_cols(temp),
    any.missing = TRUE,
    nrows = n_rows,
    ncols = n_cols
  )

  temp <- cbind(x, NA)
  expect_matrix(
    remove_no_variance_cols(temp),
    any.missing = TRUE,
    nrows = n_rows,
    ncols = n_cols
  )

  # Data frame --------------------------------------------------

  n_rows <- nrow(iris)
  n_cols <- ncol(iris)

  expect_data_frame(
    remove_no_variance_cols(iris),
    any.missing = FALSE,
    nrows = n_rows,
    ncols = n_cols - 1
  )

  temp <- iris
  temp$char <- factor("hello")
  expect_data_frame(
    remove_no_variance_cols(temp),
    any.missing = FALSE,
    nrows = n_rows,
    ncols = n_cols - 1
  )

  temp <- iris
  temp[cbind(c(100, 58, 53, 2), c(1, 1, 3, 2))] <- NA
  expect_data_frame(
    remove_no_variance_cols(temp),
    nrows = n_rows,
    ncols = n_cols - 1
  )
})

test_that("mode", {
  x <- c(10, 1, 2, 2, 1, 3, 1, 5)

  expect_identical(mode(x), 1)

  temp <- x
  temp[c(2, 5, 1)] <- NA
  expect_identical(mode(temp), 2)
  expect_identical(mode(temp, na.rm = FALSE), as.numeric(NA))

  x <- c("A", "B", "C", "D", "B", "C", "A", "B", "C")
  expect_identical(mode(x), "B")

  temp <- x
  temp[c(2, 5, 8)] <- NA
  expect_identical(mode(temp), "C")
  expect_identical(mode(temp, na.rm = FALSE), "C")

  x <- factor(x)
  expect_identical(mode(x), "B")

  x <- c(TRUE, FALSE, TRUE, FALSE, NA, FALSE, TRUE)
  expect_identical(mode(x), FALSE)
})

test_that("mkdir and rmdir", {
  temp_dir <- file.path(tempdir(), as.numeric(Sys.time()))

  expect_identical(dir.exists(temp_dir), FALSE)
  mkdir(temp_dir)
  expect_identical(dir.exists(temp_dir), TRUE)
  mkdir(temp_dir)
  expect_identical(dir.exists(temp_dir), TRUE)
  rmdir(temp_dir)
  expect_identical(dir.exists(temp_dir), FALSE)

  temp_dir <- file.path(temp_dir, "foo", "bar", "child")
  expect_identical(dir.exists(temp_dir), FALSE)
  mkdir(temp_dir)
  expect_identical(dir.exists(temp_dir), TRUE)
  rmdir(temp_dir)
  expect_identical(dir.exists(temp_dir), FALSE)
})

test_that("is_empty_dir", {
  temp_dir <- file.path(tempdir(), as.numeric(Sys.time()))

  expect_identical(suppressWarnings(is_empty_dir(temp_dir)), TRUE)

  mkdir(temp_dir)
  expect_identical(is_empty_dir(temp_dir), TRUE)

  mkdir(file.path(temp_dir, "child_dir"))
  expect_identical(is_empty_dir(temp_dir), FALSE)

  temp_dir <- file.path(temp_dir, "child_dir")
  expect_identical(is_empty_dir(temp_dir), TRUE)

  file.create(file.path(temp_dir, "hola.txt"))
  expect_identical(is_empty_dir(temp_dir), FALSE)
})

test_that("lunique", {
  x <- c(rep(1, 5), rep(2, 5), rep(3, 5))
  expect_identical(lunique(x), 3L)

  x <- c(x, NA)
  expect_identical(lunique(x), 4L)

  x <- c("foo", "bar", "bar", "foo")
  expect_identical(lunique(x), 2L)
  x <- c(NA, x, NA)
  expect_identical(lunique(x), 3L)
})

test_that("has_dims", {
  expect_identical(has_dims(data.matrix(iris)), TRUE)
  expect_identical(has_dims(1:5), FALSE)
  expect_identical(has_dims(function(x) x^2), FALSE)
  expect_identical(has_dims(NA), FALSE)
  expect_identical(has_dims(NULL), FALSE)
})

test_that("shead and stail", {
  expect_data_frame(
    shead(iris),
    any.missing = FALSE,
    nrows = 5,
    ncols = 5
  )
  expect_data_frame(
    stail(iris),
    any.missing = FALSE,
    nrows = 5,
    ncols = 5
  )

  x <- to_matrix(iris)
  expect_matrix(
    shead(x),
    any.missing = FALSE,
    nrows = 5,
    ncols = 5
  )
  expect_matrix(
    stail(x),
    any.missing = FALSE,
    nrows = 5,
    ncols = 5
  )

  x <- x[1:2, 1:2]
  expect_matrix(
    shead(x),
    any.missing = FALSE,
    nrows = 2,
    ncols = 2
  )
  expect_matrix(
    stail(x),
    any.missing = FALSE,
    nrows = 2,
    ncols = 2
  )
})

test_that("get_length", {
  expect_identical(get_length(iris), nrow(iris))
  expect_identical(get_length(1:50), 50L)
  expect_identical(get_length(c(1:50, NA, NA)), 52L)
  expect_identical(get_length(NA), 1L)
  expect_identical(get_length(NULL), 0L)
})

test_that("is_square", {
  expect_identical(is_square(iris), FALSE)
  expect_identical(is_square(1:100), FALSE)
  expect_identical(is_square(1), FALSE)
  expect_identical(is_square(NA), FALSE)

  expect_identical(is_square(iris[1:5, 1:5]), TRUE)
  expect_identical(is_square(data.matrix(iris[1:5, 1:5])), TRUE)
  expect_identical(is_square(to_matrix(1)), TRUE)
  expect_identical(is_square(to_matrix(NA)), TRUE)
})

test_that("is_empty", {
  expect_identical(is_empty(iris), FALSE)
  expect_identical(is_empty(1:100), FALSE)
  expect_identical(is_empty(1), FALSE)
  expect_identical(is_empty(NA), FALSE)
  expect_identical(is_empty(mean), FALSE)

  expect_identical(is_empty(NULL), TRUE)
  expect_identical(is_empty(c()), TRUE)
  expect_identical(is_empty(matrix(0, 0, 0)), TRUE)
  expect_identical(is_empty(data.frame()), TRUE)
})

test_that("nonull", {
  expect_identical(nonull(NULL, 2, NULL), 2)
  expect_identical(nonull(NA, 2, NULL), as.logical(NA))
  expect_identical(nonull(iris), iris)
  expect_identical(nonull(NULL, iris, 2, NA), iris)
  expect_identical(nonull(FALSE, 5, NULL, NA, NULL), FALSE)
})

test_that("char_at", {
  expect_identical(char_at("abcdefg"), "a")
  expect_identical(char_at("abcdefg", 2), "b")
  expect_identical(char_at("abcdefg", 3), "c")
  expect_identical(char_at("abcdefg", -1), "g")
  expect_identical(char_at("abcdefg", -2), "f")
  expect_identical(char_at("abcdefg", 15), "")
  expect_identical(char_at("abcdefg", -15), "")
  expect_identical(char_at(NA), as.character(NA))
  expect_identical(char_at(NULL), character(0))
})