#' @import dplyr

#' @include utils.R

prepare_univariate_y <- function() {
  if (is.data.frame(self$y)) {
    self$y <- unlist(self$y, use.names = FALSE)
  } else if (is.matrix(self$y)) {
    self$y <- c(self$y)
  }

  if (is.character(self$y)) {
    self$y <- factor(self$y)
  } else if (is.factor(self$y)) {
    self$y <- droplevels(self$y)
  }

  self$responses[["y"]] <- list(
    type = get_response_type(self$y),
    levels = levels(self$y)
  )
}

prepare_multivariate_y <- function() {
  if (is.matrix(self$y)) {
    self$y <- as.data.frame(self$y)
  }

  self$y <- self$y %>%
    mutate_if(is.character, factor) %>%
    mutate_if(is.logical, factor)

  self$y <- droplevels(self$y)

  for (col_name in colnames(self$y)) {
    self$responses[[col_name]] <- list(
      type = get_response_type(select(self$y, col_name)),
      levels = levels(select(self$y, col_name))
    )
  }
}

prepare_x <- function(x,
                      kernel,
                      rows_proportion,
                      arc_cosine_deep,
                      degree,
                      gamma,
                      coef0) {
  x <- to_matrix(x)
  x <- remove_no_variance_cols(x)

  if (!is.null(kernel)) {
    x <- apply_kernel(
      x = x,
      kernel = kernel,
      rows_proportion = rows_proportion,
      arc_cosine_deep = arc_cosine_deep,
      gamma = gamma,
      coef0 = coef0,
      degree = degree
    )
  }

  return(x)
}

prepare_degree <- function(kernel, degree) {
  if (is.null(kernel) || !(tolower(kernel) %in% tolower(KERNELS_WITH_DEGREE))) {
    return(NULL)
  }

  return(degree)
}

prepare_gamma <- function(kernel, gamma) {
  if (is.null(kernel) || !(tolower(kernel) %in% tolower(KERNELS_WITH_GAMMA))) {
    return(NULL)
  }

  return(gamma)
}

prepare_coef0 <- function(kernel, coef0) {
  if (is.null(kernel) || !(tolower(kernel) %in% tolower(KERNELS_WITH_COEF0))) {
    return(NULL)
  }

  return(coef0)
}

get_cross_validator <- function(type,
                                records_number,
                                folds_number,
                                testing_proportion) {
  type <- tolower(type)

  validator <- NULL
  if (type == "k_fold") {
    validator <- KFoldCV
  } else if (type == "random") {
    validator <- RandomCV
  } else {
    stop(echo(
      "{%s} is not a valid type of cross validation",
      set_collapse(type)
    ))
  }

  instance <- validator$new(
    folds_number = folds_number,
    records_number = records_number,
    testing_proportion = testing_proportion
  )

  return(instance)
}