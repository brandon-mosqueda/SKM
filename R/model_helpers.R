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

  for (col_name in colnames(self$y)) {
    self$responses[[col_name]] <- list(
      type = get_response_type(select(self$y, col_name)),
      levels = levels(select(self$y, col_name))
    )
  }
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

proportion_to <- function(proportion, to, lower = 0, upper = 1) {
  if (is.null(proportion)) {
    return(NULL)
  }

  return(ifelse(
    proportion >= lower & proportion <= upper,
    ceiling(proportion * to),
    proportion
  ))
}
