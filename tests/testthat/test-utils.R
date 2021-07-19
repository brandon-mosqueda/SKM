set.seed(1)

suppressWarnings(suppressMessages(library(dplyr)))

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

  temp <- iris[1:5, ]
  temp[1, 2] <- NA
  temp[3, 2] <- NA
  expect_matrix(
    to_matrix(temp),
    nrows = nrow(temp),
    ncols = 6,
    any.missing = TRUE
  )
  expect_matrix(
    to_matrix(temp, TRUE),
    nrows = nrow(temp),
    ncols = 7,
    any.missing = TRUE
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

  temp <- matrix(1:12, 4, 3)
  temp_intercept <- cbind(1, temp)
  colnames(temp) <- c(" df ", "135", "--.a")
  colnames(temp_intercept) <- c("(Intercept)", colnames(temp))
  expect_identical(to_matrix(temp), temp)
  expect_identical(to_matrix(temp, TRUE), temp_intercept)

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

test_that("to_data_frame", {
  set.seed(1)
  categories <- sample(c("A", "B", "C"), 10, replace = TRUE)
  binaries <- sample(c(TRUE, FALSE), 10, replace = TRUE)

  expect_equal(to_data_frame(1), data.frame(V1 = 1))

  expect_equal(to_data_frame(NA), data.frame(V1 = factor(NA)))

  expect_equal(to_data_frame(1:10), data.frame(V1 = 1:10))
  expect_equal(to_data_frame(binaries), data.frame(V1 = factor(binaries)))

  expect_equal(to_data_frame(categories), data.frame(x = factor(categories)))
  expect_equal(
    to_data_frame(factor(categories)),
    data.frame(x = factor(categories))
  )

  expect_equal(
    to_data_frame(matrix(1:10, 2, 5)),
    as.data.frame(matrix(1:10, 2, 5))
  )

  temp <- matrix(1:15, 5, 3)
  colnames(temp) <- c("V1", "V2", "V3")
  temp[1, 2] <- NA
  temp[3, 2] <- NA
  expect_equal(
    to_data_frame(temp),
    data.frame(temp)
  )

  expect_equal(to_data_frame(iris), iris)

  temp <- iris %>% mutate(Species = as.character(Species))
  expect_equal(to_data_frame(temp), iris)

  diverse_data <- data.frame(
    a = rnorm(10),
    b = rpois(10, 5),
    c = categories,
    d = factor(categories),
    e = binaries
  )
  temp <- diverse_data
  temp$c <- factor(temp$c)
  temp$e <- factor(temp$e)
  expect_equal(to_data_frame(diverse_data), temp)
})

test_that("remove_no_variance_cols", {
  # Matrix --------------------------------------------------

  set.seed(2)

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
  expect_equal(
    attr(remove_no_variance_cols(temp), "removed_cols"),
    c(1, ncol(x) + 2)
  )

  temp <- cbind(3, 5, 0.5)
  expect_numeric(
    remove_no_variance_cols(temp),
    len = 0
  )
  expect_equal(
    attr(remove_no_variance_cols(temp), "removed_cols"),
    1:3
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
    suppressWarnings(remove_no_variance_cols(iris)),
    any.missing = FALSE,
    nrows = n_rows,
    ncols = n_cols - 1
  )

  expect_equal(
    suppressWarnings(attr(remove_no_variance_cols(iris), "removed_col")),
    5
  )

  temp <- iris
  temp$char <- factor("hello")
  expect_data_frame(
    suppressWarnings(remove_no_variance_cols(temp)),
    any.missing = FALSE,
    nrows = n_rows,
    ncols = n_cols - 1
  )

  temp <- iris
  temp[cbind(c(100, 58, 53, 2), c(1, 1, 3, 2))] <- NA
  expect_data_frame(
    suppressWarnings(remove_no_variance_cols(temp)),
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

  file.create(file.path(temp_dir, "hello.txt"))
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

test_that("get_records", {
  expect_identical(get_records(iris, 10:20), iris[10:20, ])
  expect_identical(get_records(100:200, 1:10), 100:109)
  expect_identical(get_records(NULL, 10), NULL)
  expect_identical(get_records(NA, 10), NA)
  expect_identical(get_records(c("A", "B", "C"), 2), "B")
  expect_identical(get_records(mtcars, -1), mtcars[-1, ])
  expect_identical(get_records(c(TRUE, FALSE, TRUE), -3), c(TRUE, FALSE))
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

test_that("has", {
  expect_identical(has(NA), FALSE)
  expect_identical(has(NA, NA), FALSE)
  expect_identical(has(NA, NULL), FALSE)

  expect_identical(has(NULL), FALSE)
  expect_identical(has(NULL, NULL), FALSE)
  expect_identical(has(NULL, NA), FALSE)
  expect_identical(has(NA, NULL, iris), FALSE)

  expect_identical(has(1), TRUE)
  expect_identical(has(FALSE), TRUE)
  expect_identical(has(0), TRUE)
  expect_identical(has("A"), TRUE)
  expect_identical(has(iris, mtcars, 2, FALSE), TRUE)
})

test_that("nonull", {
  expect_identical(nonull(NULL, 2, NULL), 2)
  expect_identical(nonull(NA, 2, NULL), as.logical(NA))
  expect_identical(nonull(iris), iris)
  expect_identical(nonull(NULL, iris, 2, NA), iris)
  expect_identical(nonull(FALSE, 5, NULL, NA, NULL), FALSE)
})

test_that("echo", {
  expect_output(echo("hello"), "hello")
  expect_output(echo("hel%slo", "world"), "helworldlo")
  expect_output(echo("hel%ilo", 5), "hel5lo")
  expect_output(echo("hel%.2flo", pi), "3.14")
  expect_output(echo(NA), "NA")
  expect_output(echo(NULL), "NULL")
  expect_output(echo(" fold %s/%s ", 5, 10), " fold 5/10 ")
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
  expect_identical(char_at(334, 3), "4")
  expect_identical(char_at(FALSE, 2), "A")
})

test_that("str_join", {
  expect_identical(str_join("A", "B"), "AB")
  expect_identical(str_join(c("A", "B"), c("C", "D")), c("AC", "BD"))
  expect_identical(str_join(c("A", NA), c("C", "D")), c("AC", NA))
  expect_identical(str_join(c("A", "B"), c("C", NA)), c("AC", NA))
  expect_identical(str_join(c("A", NA), c("C", NA)), c("AC", NA))
  expect_identical(str_join(NA, NA), as.character(NA))
  expect_identical(str_join(NULL, NULL), character(0))
  expect_identical(str_join("A", NULL), "A")
  expect_identical(str_join(123, "AB"), "123AB")
  expect_identical(str_join(123, NA), as.character(NA))
})

test_that("get_tabs", {
  expect_identical(get_tabs(0), "")
  expect_identical(get_tabs(1), "\t")
  expect_identical(get_tabs(2), "\t\t")
  expect_identical(get_tabs(3), "\t\t\t")
  expect_identical(get_tabs(4), "\t\t\t\t")
})

test_that("get_response", {
  expect_identical(get_response(y ~ x), "y")
  expect_identical(get_response(Species ~ ., data = iris), "Species")
})

test_that("replace_by_regex", {
  expect_identical(replace_by_regex("hello", "AZ", "el"), "hAZlo")
  expect_identical(replace_by_regex(NA, "ha", "ha"), as.character(NA))
  expect_identical(replace_by_regex("hello", NA, "h"), as.character(NA))
  expect_identical(replace_by_regex("hello", "AZ", NA), as.character(NA))
  expect_identical(replace_by_regex(NULL, "AZ", "h"), character(0))
  expect_identical(
    replace_by_regex("he5llo 56 wo5rld", "X", "[0-9]"),
    "heXllo XX woXrld"
  )
})

test_that("regex_match", {
  expect_identical(regex_match("hello", "o"), "o")
  expect_identical(regex_match("hello 56 wo65rld", "[0-9]{2}"), "56")
  expect_identical(
    regex_match("hello 56 wo65rld", "[0-9]{3}"),
    as.character(NA)
  )

  expect_identical(
    regex_match("Jackson@oznocdo.zm", ".+(?=@)"),
    "Jackson"
  )

  expect_identical(
    regex_match("Jackson@oznocdo.zm", "(?<=@).+"),
    "oznocdo.zm"
  )

  expect_identical(regex_match(NULL, "."), as.character(NA))
  expect_identical(regex_match(NA, "."), as.character(NA))
  expect_identical(regex_match(659841, ".98."), "5984")
})

test_that("regex_contains", {
  expect_identical(regex_contains("hello", "el"), TRUE)
  expect_identical(regex_contains("hello", "a"), FALSE)
  expect_identical(regex_contains("jackson@gmail.com", ".+@.+"), TRUE)
  expect_identical(regex_contains(65487, "^65..."), TRUE)
  expect_identical(regex_contains("nadaa", "^nada$"), FALSE)
  expect_identical(regex_contains("nadaa", "^nadaa$"), TRUE)
  expect_identical(regex_contains(NA, "^nadaa$"), FALSE)
  expect_identical(regex_contains("nadaa", NA), FALSE)
  expect_identical(regex_contains(NA, NA), FALSE)
  expect_identical(regex_contains(NULL, "s"), FALSE)
  expect_identical(regex_contains("NA", NULL), FALSE)
})

test_that("has_str", {
  expect_identical(has_str("hola", "ol"), TRUE)
  expect_identical(has_str("hola", ".l."), FALSE)
  expect_identical(has_str(NA, "l"), FALSE)
  expect_identical(has_str("hola", NA), FALSE)
  expect_identical(has_str(NA, NA), FALSE)

  expect_identical(has_str(NULL, "l"), FALSE)
  expect_identical(has_str("hola", NULL), FALSE)
  expect_identical(has_str(NULL, NULL), FALSE)
})

test_that("set_collapse", {
  expect_identical(set_collapse("hola"), "'hola'")
  expect_identical(set_collapse(FALSE), "'FALSE'")
  expect_identical(set_collapse(23), "'23'")
  expect_identical(set_collapse(NA), "'NA'")
  expect_identical(set_collapse(NULL), "")

  expect_identical(set_collapse(c("hello", "world")), "'hello', 'world'")
  expect_identical(set_collapse(c("NA", "world")), "'NA', 'world'")
})

test_that("is_number", {
  expect_identical(is_number(1), TRUE)
  expect_identical(is_number(-3), TRUE)
  expect_identical(is_number(Inf), TRUE)
  expect_identical(is_number(1e-10), TRUE)
  expect_identical(is_number(FALSE), TRUE)
  expect_identical(is_number(TRUE), TRUE)
  expect_identical(is_number("565.566"), TRUE)
  expect_identical(all(is_number(c("565.566", "3"))), TRUE)

  expect_identical(is_number("hello"), FALSE)
  expect_identical(is_number("565.566.5"), FALSE)
  expect_identical(is_number(iris), FALSE)
  expect_identical(is_number(iris$Species), FALSE)
  expect_identical(is_number(NA), FALSE)
  expect_identical(is_number(NULL), FALSE)
})

test_that("is_int", {
  expect_identical(is_int(56), TRUE)
  expect_identical(all(is_int(c(56, 0))), TRUE)
  expect_identical(is_int(56.0), TRUE)

  expect_identical(is_int(c(56, 53.2)), c(TRUE, FALSE))

  expect_identical(is_int("56.0"), FALSE)
  expect_identical(is_int(c("56", "25")), c(FALSE, FALSE))
  expect_identical(is_int(Inf), FALSE)
  expect_identical(is_int(56.3), FALSE)
})

test_that("is_discrete", {
  expect_identical(is_discrete(2), TRUE)
  expect_identical(is_discrete(2.0), TRUE)
  expect_identical(is_discrete(mtcars$cyl), TRUE)

  expect_identical(is_discrete("2.6"), FALSE)
  expect_identical(is_discrete(NA), FALSE)
  expect_identical(is_discrete(FALSE), FALSE)
  expect_identical(is_discrete(iris$Species), FALSE)
  expect_identical(is_discrete(NULL), FALSE)
})

test_that("get_response_type", {
  expect_identical(get_response_type(1), RESPONSE_TYPES$DISCRETE)
  expect_identical(get_response_type(1.6), RESPONSE_TYPES$CONTINUOUS)
  expect_identical(
    get_response_type(c(1, 2, 3, 4.01)),
    RESPONSE_TYPES$CONTINUOUS
  )
  expect_identical(
    get_response_type(c(0, 1, 2, 3, 4)),
    RESPONSE_TYPES$DISCRETE
  )
  expect_identical(
    get_response_type(c(0, 1, 2, 3, 4, -1)),
    RESPONSE_TYPES$CONTINUOUS
  )
  expect_identical(get_response_type(FALSE), RESPONSE_TYPES$CATEGORICAL)
  expect_identical(get_response_type("F"), RESPONSE_TYPES$CATEGORICAL)
  expect_identical(get_response_type(iris$Species), RESPONSE_TYPES$CATEGORICAL)

  expect_identical(
    get_response_type(c(FALSE, FALSE, TRUE)),
    RESPONSE_TYPES$BINARY
  )
  expect_identical(
    get_response_type(c("A", "B")),
    RESPONSE_TYPES$BINARY
  )

  expect_identical(get_response_type(NA), RESPONSE_TYPES$CATEGORICAL)
  expect_identical(get_response_type(as.numeric(NA)), RESPONSE_TYPES$CONTINUOUS)

  expect_error(get_response_type(NULL))
})

test_that("is response type functions", {
  expect_identical(
    is_continuous_response(RESPONSE_TYPES$CONTINUOUS),
    TRUE
  )
  expect_identical(
    is_discrete_response(RESPONSE_TYPES$DISCRETE),
    TRUE
  )
  expect_identical(
    is_categorical_response(RESPONSE_TYPES$CATEGORICAL),
    TRUE
  )
  expect_identical(
    is_binary_response(RESPONSE_TYPES$BINARY),
    TRUE
  )

  expect_identical(
    is_numeric_response(RESPONSE_TYPES$DISCRETE),
    TRUE
  )
  expect_identical(
    is_numeric_response(RESPONSE_TYPES$CONTINUOUS),
    TRUE
  )

  expect_identical(
    is_class_response(RESPONSE_TYPES$BINARY),
    TRUE
  )
  expect_identical(
    is_class_response(RESPONSE_TYPES$CATEGORICAL),
    TRUE
  )

  expect_identical(
    is_class_response(RESPONSE_TYPES$CONTINUOUS),
    FALSE
  )
  expect_identical(
    is_class_response(RESPONSE_TYPES$DISCRETE),
    FALSE
  )

  expect_identical(
    is_numeric_response(RESPONSE_TYPES$BINARY),
    FALSE
  )
  expect_identical(
    is_numeric_response(RESPONSE_TYPES$CATEGORICAL),
    FALSE
  )
})
