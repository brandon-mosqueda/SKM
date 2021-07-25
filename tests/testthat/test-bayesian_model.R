data(Iris)

test_that("Univariate numeric", {
  x <- list(list(x = x_num, model = "BRR"))

  model <- bayesian_model(
    x = x,
    y = y_num,
    testing_indices = 10,
    seed = 1,
    verbose = FALSE
  )

  y <- y_num
  y[10] <- NA
  x <- list(x_1 = list(x = to_matrix(x_num), model = "BRR"))

  expect_bayesian_model(
    model = model,
    x = x,
    y = y,
    responses = list(
      y = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL)
    ),
    bglr_response_type = "gaussian",
    testing_indices = 10
  )
})

test_that("Univariate binary", {
  x <- list(list(x = x_bin, model = "BRR"), list(x = x_bin, model = "BRR"))
  y <- y_bin
  testing_indices <- c(5, 10, 15, 20)
  y[testing_indices] <- NA

  model <- suppressWarnings(bayesian_model(
    x,
    y,
    seed = 1,
    verbose = FALSE
  ))

  x <- list(
    x_1 = list(x = to_matrix(x_bin), model = "BRR"),
    x_2 = list(x = to_matrix(x_bin), model = "BRR")
  )

  expect_bayesian_model(
    model = model,
    x = x,
    y = y,
    responses = list(
      y = list(type = RESPONSE_TYPES$BINARY, levels = levels(y_bin))
    ),
    bglr_response_type = "ordinal",
    testing_indices = testing_indices
  )
})

test_that("Univariate categorical", {
  x <- list(list(x = x_cat, model = "bayes_a"))
  testing_indices <- c(1, 2, 3, 4, 148, 149, 150)

  model <- suppressWarnings(bayesian_model(
    x,
    y_cat,

    iterations_number = 100,
    burn_in = 50,

    testing_indices = testing_indices,
    seed = 1,
    verbose = FALSE
  ))

  x <- list(x_1 = list(x = to_matrix(x_cat), model = "bayes_a"))

  y <- y_cat
  y[testing_indices] <- NA

  expect_bayesian_model(
    model = model,
    x = x,
    y = y,
    responses = list(
      y = list(type = RESPONSE_TYPES$CATEGORICAL, levels = levels(y_cat))
    ),
    bglr_response_type = "ordinal",
    testing_indices = testing_indices
  )
})

test_that("Univariate numeric (NA)", {
  x <- x_num
  y <- y_num
  x[2, 3] <- NA
  x[56, 2] <- NA
  x[144, 1] <- NA
  y[100] <- NA
  y[20] <- NA

  testing_indices <- c(20, 2, 15, 18)

  x <- list(hola = list(x = x, model = "bayes_LASsO"))

  model <- suppressWarnings(bayesian_model(
    x,
    y,
    iterations_number = 100,
    burn_in = 5,
    thinning = 1,
    testing_indices = testing_indices,
    seed = 1,
    verbose = FALSE
  ))

  x <- list(hola = list(x = to_matrix(x$hola$x), model = "bayes_LASsO"))
  y[c(testing_indices, 100)] <- NA

  expect_bayesian_model(
    model = model,
    x = x,
    y = y,
    responses = list(
      y = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL)
    ),
    removed_rows = c(2, 56, 144),
    bglr_response_type = "gaussian",
    # In each one is substracted the number of lower removed rows
    testing_indices = c(14, 17, 98, 19)
  )
})

test_that("Multivariate numeric", {
  x <- list(
    list(x = x_multi, model = "BRR"),
    bar = list(x = x_multi, model = "BRR")
  )

  model <- bayesian_model(
    x,
    y_multi,

    testing_indices = c(5, 13),
    seed = 1,
    verbose = FALSE
  )

  x <- list(
    x_1 = list(x = to_matrix(x_multi), model = "BRR"),
    bar = list(x = to_matrix(x_multi), model = "BRR")
  )

  y <- to_matrix(y_multi)
  rownames(y) <- NULL
  y[c(5, 13), ] <- NA

  expect_bayesian_model(
    model = model,
    x = x,
    y = y,
    responses = list(
      y1 = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL),
      y2 = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL)
    ),
    is_multivariate = TRUE,
    bglr_response_type = NULL,
    testing_indices = c(5, 13)
  )
})

test_that("Multivariate combined", {
  x <- list(list(x = x_multi_cat, model = "BRR"))
  testing_indices <- 100:110

  model <- suppressWarnings(bayesian_model(
    x,
    y_multi_cat,

    testing_indices = testing_indices,

    seed = 1,
    verbose = FALSE
  ))

  x <- list(x_1 = list(x = to_matrix(x_multi_cat), model = "BRR"))
  y <- data.matrix(y_multi_cat)
  rownames(y) <- NULL
  y[testing_indices, ] <- NA

  expect_bayesian_model(
    model = model,
    x = x,
    y = y,
    responses = list(
      y1 = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL),
      y2 = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL)
    ),
    is_multivariate = TRUE,
    bglr_response_type = NULL,
    testing_indices = testing_indices
  )
})

test_that("Multivariate numeric (NA)", {
  x <- x_multi
  y <- data.matrix(y_multi)
  rownames(y) <- NULL

  x[5, 1] <- NA
  x[10, 2] <- NA
  x[50, 1] <- NA
  x[53, 3] <- NA

  y[5, 1] <- NA
  y[22, 2] <- NA
  y[40, 2] <- NA
  y[44, 1] <- NA

  x <- list(foo = list(x = x, model = "BRR"))

  model <- suppressWarnings(bayesian_model(
    x,
    y,
    seed = 1,
    verbose = FALSE
  ))

  x <- list(foo = list(x = to_matrix(x$foo$x), model = "BRR"))
  y[c(5, 22, 40, 44), ] <- NA

  expect_bayesian_model(
    model = model,
    x = x,
    y = y,
    responses = list(
      y1 = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL),
      y2 = list(type = RESPONSE_TYPES$CONTINUOUS, levels = NULL)
    ),
    is_multivariate = TRUE,
    bglr_response_type = NULL,
    removed_rows = c(5, 10, 50, 53),
    testing_indices = c(20, 38, 42)
  )
})
