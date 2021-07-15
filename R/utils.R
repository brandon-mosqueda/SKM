#' @import dplyr

#' @include globals.R

# Computations --------------------------------------------------

lm_intercept <- function(x, y) {
  if (all(is.na(x)) || all(is.na(y))) {
    return(NaN)
  }

  return(lm(y ~ x, data = data.frame(x = x, y = y))$coefficients[1])
}

lm_slope <- function(x, y) {
  if (all(is.na(x)) || all(is.na(y))) {
    return(NaN)
  }

  return(lm(y ~ x, data = data.frame(x = x, y = y))$coefficients[2])
}

#' @export
to_matrix <- function(x, with_intercept = FALSE) {
  if (is.vector(x)) {
    if (is.character(x)) {
      x <- factor(x)
    } else {
      x <- t(t(x))
    }
  }

  if (is.factor(x)) {
    x <- as.data.frame(x)
  }

  if (is.matrix(x)) {
    if (is.null(colnames(x))) {
      colnames(x) <- paste0("x", 1:ncol(x))
    }

    if (with_intercept) {
      x <- cbind(1, x)
      colnames(x)[1] <- "Intercept"
    }
  } else if (is.data.frame(x)) {
    x <- model.matrix( ~ ., x)

    if (!with_intercept) {
      x <- x[, -1]
    }
  }

  return(x)
}

#' @export
to_data_frame <- function(x) {
  if (is.vector(x)) {
    if (is.character(x)) {
      x <- factor(x)
    } else {
      x <- t(t(x))
    }
  }

  x <- as.data.frame(x)

  x <- mutate_if(x, function(x) is.character(x) || is.logical(x), factor)

  return(x)
}

#' @export
remove_no_variance_cols <- function(x) {
  if (is.data.frame(x)) {
    x <- select_if(x, is.numeric)
  }

  cols_variances <- apply(x, 2, function(x) var(x, na.rm = TRUE))
  no_zero_variances_cols <- which(cols_variances > 0)

  return(x[, no_zero_variances_cols])
}

#' @title Cholesky
#'
#' @description Compute the Cholesky factorization of a non-real symmetric
#'              positive-definite square matrix. (Taken from BMTME R package at
#'              https://github.com/frahik/BMTME/blob/master/R/cholesky.R).
#'
#' @param G (\code{numeric - matrix}) an object to apply this method, it could
#'        be non positive-definite matrices.
#' @param tolerance (\code{double}) Tolerance level, by default is 1e-10.
cholesky_no_definite <- function(G, tolerance = 1e-10) {
  G <- (G + t(G)) / 2
  EigenA <- eigen(G)
  d_A    <- EigenA$values
  V_A    <- EigenA$vectors
  d_A[which(d_A < tolerance)] <- tolerance*100L
  pos_A1 <- which(d_A > tolerance)
  if (identical(pos_A1, integer(0))) {
    pos_A <- 1L
  } else {
    pos_A <- pos_A1
  }
  d_A_Star <- d_A[pos_A]
  V_A_Star <- V_A[, pos_A]

  if (length(pos_A) == 1L) {
    d_A_Star <- 1L / d_A_Star
    LG <- d_A_Star * sqrt(V_A_Star)
  } else {
    d_A_Star <- diag(d_A_Star)
    LG <- (V_A_Star %*% sqrt(d_A_Star))
  }
  return(LG)
}

#' @title Compute Cholesky
#'
#' @description Compute the Choleski factorization of a real symmetric
#'              positive-definite square matrix. If it fails, compute the
#'              Cholesky factorization of a non-real symmetric
#'              positive-definite square matrix
#'
#' @param x (\code{matrix}) The x
#'
#' @return The cholesky matrix.
#'
#' @export
cholesky <- function(x) {
  data_names <- colnames(x)

  tryCatch({
    result <- t(chol(x))
  }, error = function(error_condition) {
      result <<- cholesky_no_definite(x)
  })

  rownames(result) <- data_names
  colnames(result) <- data_names

  return(result)
}

#' Get the value that appears more times. If there are more than 1
#' value that appears more times (multimodal), return the lowest value.
#' @export
mode <- function(x, na.rm = TRUE) {
  use_na <- if (na.rm) "no" else "always"
  ocurrences <- sort(table(x, useNA = use_na), decreasing = TRUE)
  mode <- names(ocurrences)[1]

  if (is.numeric(x)) {
    mode <- as.numeric(mode)
  } else if (is.logical(x)) {
    mode <- as.logical(mode)
  }

  return(mode)
}

# Sysmtem --------------------------------------------------

#' @export
is_windows_os <- function() {
  return(.Platform$OS.type == "windows")
}

#' @export
is_unix_os <- function() {
  return(.Platform$OS.type == "unix")
}

#' Create a directory if does not exist, always recursively
#' @export
mkdir <- function(directory) {
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
}

#' @export
rmdir <- function(directory) {
  unlink(directory, recursive = TRUE)
}

#' @export
is_empty_dir <- function(directory) {
  if (!dir.exists(directory)) {
    warning("Directory does not exists")
    return(TRUE)
  }

  return(
    is_empty(list.files(directory, recursive = TRUE)) &&
    length(list.dirs(directory, recursive = TRUE)) <= 1
  )
}

# Utilities --------------------------------------------------

not_implemented_function <- function() {
  stop("Not implemented function")
}

#' @export
lunique <- function(x) {
  return(length(unique(x)))
}

#' @export
has_dims <- function(x) {
  return(!is.null(dim(x)))
}

close_all_devices <- function() {
  invisible(sapply(dev.list(), dev.off))
}

#' @export
shead <- function(x, n = 5) {
  return(x[1:min(n, nrow(x)), 1:min(n, ncol(x))])
}

#' @export
stail <- function(x, n = 5) {
  n_rows <- nrow(x)
  n_cols <- ncol(x)

  return(x[max(1, n_rows - n + 1):n_rows, max(1, n_cols - n + 1):n_cols])
}

#' @export
get_length <- function(x) {
  return(if (is.null(dim(x))) length(x) else nrow(x))
}

#' @export
get_records <- function(x, indices) {
  if (has_dims(x)) {
    return(x[indices, , drop = FALSE])
  } else {
    return(x[indices])
  }
}

#' Hide code output
#'
#' @param code (\code{function}) The code to be evaluated
#' @param all (\code{logical}) If \code{TRUE}, suppress also warnings and the
#'        result of the code. \code{FALSE} by default.
#'
#' @return If \emph{all} is \code{FALSE}, return whatever the evaluated code
#'         returns, nothing otherwise.
#'
#' @export
hush <- function(code, all = FALSE) {
  file <- ifelse(is_unix_os(), "/dev/null", "NUL")

  if (all) {
    capture.output(
      suppressWarnings(suppressMessages(temp <- code)),
      file = file,
      append = TRUE
    )
  } else {
    capture.output(
      suppressMessages(temp <- code),
      file = file,
      append = TRUE
    )
  }

  if (all) {
    return(invisible(temp))
  } else {
    return(temp)
  }
}

get_verbose_function <- function(verbose) {
  return(if (verbose) invisible else hush)
}

#' @export
is_square <- function(x) {
  result <- nrow(x) == ncol(x)
  if (identical(result, logical(0))) {
    result <- FALSE
  }

  return(result)
}

#' @export
is_empty <- function(x) {
  return(length(x) == 0)
}

#' @export
has <- function(...) {
  x <- list(...)

  return(!(anyNA(x) || any(sapply(x, is.null)) || anyNaN(x)))
}

#' @title nonull
#' @description Get the first value that is not NULL.
#' @export
nonull <- function(...) {
  params <- list(...)

  for (param in params) {
    if (!is.null(param)) {
      return(param)
    }
  }

  return(NULL)
}

# Text manipulation --------------------------------------------------

#' @export
echo <- function(format, ..., end = "\n") {
  if (is.null(format)) {
    format <- "NULL"
  } else if (is.na(format)) {
    format <- "NA"
  }

  invisible(cat(sprintf(format, ...), end))
}

print_model_time_execution <- function(execution_time) {
  echo(
    "*** Model evaluation completed in %s %s ***",
    round(execution_time, 4),
    attr(execution_time, "units")
  )
}

#' @export
char_at <- function(string, index = 1) {
  if (index < 0) {
    index <- nchar(string) + index + 1
  }

  return(substr(string, index, index))
}

#' @export
str_join <- function(string1, string2) {
  joined_strings <- paste0(string1, string2)

  nas_strings <- which(is.na(string1) | is.na(string2))

  joined_strings[nas_strings] <- NA

  return(joined_strings)
}

get_tabs <- function(num = 1) {
  return(paste0(rep("\t", num), collapse = ""))
}

# Given a formula, get the name of the response varibles
get_response <- function(formula, data = NULL) {
  tt <- terms(formula, data = data)
  ## [1] is the list call
  vars <- as.character(attr(tt, "variables"))[-1]
  # index of response var
  response <- attr(tt, "response")
  response <- vars[response]

  return(trimws(strsplit(response, "\\+")[[1]]))
}

#' @export
replace_by_regex <- function(original, str_to_replace, regex) {
  return(gsub(regex, str_to_replace, original))
}

#' @export
regex_match <- function(text, regex) {
  if (!has(text, regex)) {
    return(as.character(NA))
  }

  # Accepts looking behind and forward
  match <- regmatches(text, regexec(regex, text, perl = TRUE))
  match <- sapply(
    match,
    function(x) ifelse(identical(x, character(0)), as.character(NA), x)
  )

  return(match)
}

#' @export
regex_contains <- function(text, regex) {
  return(!is.na(regex_match(text, regex)))
}

#' @export
has_str <- function(base_str, substring) {
  if (!has(base_str, substring)) {
    return(FALSE)
  }

  return(grepl(substring, base_str, fixed = TRUE))
}

#' @export
set_collapse <- function(values) {
  return(paste0(shQuote(values), collapse = ", "))
}

# Type checks --------------------------------------------------

#' @export
is_number <- function(x) {
  if (!is.vector(x) || !has(x)) {
    return(FALSE)
  }

  suppressWarnings(x <- as.numeric(x))

  return(!is.na(x))
}

#' @export
is_int <- function(x) {
  if (!is.numeric(x)) {
    return(rep(FALSE, length(x)))
  }

  return(is.finite(x) & (x %% 1 == 0))
}

#' @export
is_discrete <- function(number) {
  if (all(is.na(number)) || !all(is_int(number), na.rm = TRUE)) {
    return(FALSE)
  }

  return(all(number >= 0, na.rm = TRUE))
}

get_response_type <- function(y) {
  if (is.character(y) || is.logical(y)) {
    y <- factor(y)
  }

  if (is.factor(y)) {
    type <- RESPONSE_TYPES$CATEGORICAL

    if (length(levels(y)) == 2) {
      type <- RESPONSE_TYPES$BINARY
    }
  } else if (is.vector(y) && is.numeric(y)) {
    type <- RESPONSE_TYPES$CONTINUOUS

    if (all(is_discrete(y))) {
      type <- RESPONSE_TYPES$DISCRETE
    }
  } else {
    stop("Invalid response variable, y must be a factor or a vector")
  }

  return(type)
}

is_continuous_response <- function(response_type) {
  if (!has(response_type)) {
    return(FALSE)
  }

  return(response_type == RESPONSE_TYPES$CONTINUOUS)
}

is_discrete_response <- function(response_type) {
  if (!has(response_type)) {
    return(FALSE)
  }

  return(response_type == RESPONSE_TYPES$DISCRETE)
}

is_numeric_response <- function(response_type) {
  if (!has(response_type)) {
    return(FALSE)
  }

  return(is_continuous_response(response_type) ||
         is_discrete_response(response_type))
}

is_binary_response <- function(response_type) {
  if (!has(response_type)) {
    return(FALSE)
  }

  return(response_type == RESPONSE_TYPES$BINARY)
}

is_categorical_response <- function(response_type) {
  if (!has(response_type)) {
    return(FALSE)
  }

  return(response_type == RESPONSE_TYPES$CATEGORICAL)
}

is_class_response <- function(response_type) {
  if (!has(response_type)) {
    return(FALSE)
  }

  return(
    is_binary_response(response_type) || is_categorical_response(response_type)
  )
}

# Randomness --------------------------------------------------

#' @export
get_rand_state <- function() {
  # Using `get0()` here to have `NULL` output in case object doesn't
  # exist. Also using `inherits = FALSE` to get value exactly from
  # global environment and not from one of its parent.
  get0(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
}

#' @export
set_rand_state <- function(state) {
  # Assigning `NULL` state might lead to unwanted consequences
  if (!is.null(state)) {
    assign(".Random.seed", state, envir = .GlobalEnv, inherits = FALSE)
  }
}
