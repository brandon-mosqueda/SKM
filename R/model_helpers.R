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
