get_length <- function(x) {
  return(if (dim(x) != NULL) nrow(x) else length(x))
}

is_windows_os <- function() {
  return(.Platform$OS.type == "windows")
}

is_unix_os <- function() {
  return(.Platform$OS.type == "unix")
}

#' Hide code output
#'
#' @param code (\code{function}) The code to be evaluated
#' @param all (\code{logical}) If \code{TRUE}, suppress also warnings and the
#'        result of the code. \code{FALSE} by default.
#'
#' @return If \emph{all} is \code{FALSE}, return whatever the evaluated code
#'         returns, nothing otherwise.
hush <- function(code, all=FALSE) {
  file <- ifelse(is_unix_os(), "/dev/null", "NUL")

  if (all) {
    capture.output(suppressWarnings(suppressMessages(temp <- code)),
                   file=file,
                   append=TRUE)
  } else {
    capture.output(suppressMessages(temp <- code),
                   file=file,
                   append=TRUE)
  }

  if (all) {
    return(invisible(temp))
  } else {
    return(temp)
  }
}

#' Create a directory if does not exist, always recursively
mkdir <- function(directory) {
  if (!dir.exists(directory)){
    dir.create(directory, recursive=TRUE)
  }
}

rmdir <- function(directory) {
  unlink(directory, recursive = TRUE)
}

is_square <- function(Matrix) {
  return(nrow(Matrix) == ncol(Matrix))
}

is_empty <- function(x) {
  return(length(x) == 0)
}

is_empty_dir <- function(directory) {
  if (!dir.exists(directory)) {
    return(FALSE)
  }

  return(
    is_empty(list.files(directory, recursive = TRUE)) &&
      length(list.dirs(directory, recursive = TRUE)) <= 1
  )
}

#' @title nonull
#' @description Get the first value that is not NULL.
nonull <- function(...) {
  params <- list(...)

  for (param in params) {
    if (!is.null(param)) {
      return(param)
    }
  }

  return(NULL)
}

get_rand_state <- function() {
  # Using `get0()` here to have `NULL` output in case object doesn't
  # exist. Also using `inherits = FALSE` to get value exactly from
  # global environment and not from one of its parent.
  get0(".Random.seed", envir=.GlobalEnv, inherits=FALSE)
}

set_rand_state <- function(state) {
  # Assigning `NULL` state might lead to unwanted consequences
  if (!is.null(state)) {
    assign(".Random.seed", state, envir=.GlobalEnv, inherits=FALSE)
  }
}

get_tabs <- function(num=1) {
  return(paste0(rep("\t", num), collapse=""))
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
#' @param Data (\code{matrix}) The data
#'
#' @return The cholesky matrix.
#'
#' @export
cholesky <- function(Data) {
  data_names <- colnames(Data)

  tryCatch({
    result <- t(chol(Data))
  }, error=function(error_condition) {
      result <<- cholesky_no_definite(Data)
  })

  rownames(result) <- data_names
  colnames(result) <- data_names

  return(result)
}

# Given a formula, get the name of the response varibles
get_response <- function(formula, data=NULL) {
  tt <- terms(formula, data=data)
  ## [1] is the list call
  vars <- as.character(attr(tt, "variables"))[-1]
  # index of response var
  response <- attr(tt, "response")
  response <- vars[response]

  return(trimws(strsplit(response, "\\+")[[1]]))
}

not_implemented_function <- function() {
  stop("Not yet simplemented")
}

replace_str <- function(original, str_to_replace, regex) {
  return(gsub(regex, str_to_replace, original))
}

regex_match <- function(text, regex) {
  # Accepts looking behind and forward
  match <- regmatches(text, regexec(regex, text, perl=TRUE))
  match <- sapply(match, function(x) ifelse(identical(x, character(0)), NA, x))

  return(match)
}

regex_contains <- function(regex, text) {
  return(!is.na(regex_match(text, regex)))
}

has_str <- function(base_str, substring) {
  return(grepl(substring, base_str, fixed=TRUE))
}

# Get the value that appears more times. If there are more than 1
# value that appears more times (multimodal), return the lowest value.
mode <- function(data) {
  return(names(sort(summary(as.factor(data)), decreasing=TRUE)[1]))
}

is_number <- function(value) {
  if (is.factor(value)) {
    return(FALSE)
  }

  suppressWarnings(value <- as.numeric(value))

  return(!is.na(value))
}

is_int <- function(number) {
  return(number %% 1 == 0)
}

is_discrete <- function(number) {
  if (all(is.na(number)) || !all(is_int(number), na.rm = TRUE)) {
    return(FALSE)
  }

  return(all(number >= 0, na.rm = TRUE))
}

get_response_type <- function(y) {
  if (is.factor(y)) {
    type <- RESPONSE_TYPES$CATEGORICAL

    if (length(levels(y)) == 2) {
      type <- RESPONSE_TYPES$BINARY
    }
  } else {
    if (all(is_number(y)) || anyNA(y)) {
      type <- RESPONSE_TYPES$CONTINUOUS

      if (all(is_discrete(y))) {
        type <- RESPONSE_TYPES$DISCRETE
      }
    } else {
      stop("Invalid response varible's type. If the response variable is categorical, send it as factor")
    }
  }

  return(type)
}

is_continuous_response <- function(response_type) {
  return(response_type == RESPONSE_TYPES$CONTINUOUS)
}

is_discrete_response <- function(response_type) {
  return(response_type == RESPONSE_TYPES$DISCRETE)
}

is_numeric_response <- function(response_type) {
  return(is_continuous_response(response_type) ||
         is_discrete_response(response_type))
}

is_binary_response <- function(response_type) {
  return(response_type == RESPONSE_TYPES$BINARY)
}

is_categorical_response <- function(response_type) {
  return(response_type == RESPONSE_TYPES$CATEGORICAL)
}

is_class_response <- function(response_type) {
  return(is_binary_response(response_type) ||
         is_categorical_response(response_type))
}

remove_no_variance_cols <- function(Data) {
  cols_variances <- apply(Data, 2, var)
  no_zero_variances_cols <- which(cols_variances > 0)

  return(Data[, no_zero_variances_cols])
}

get_same_from_set <- function(string, set_strings) {
  if (identical(string, character(0)) || !is.character(string)) {
    return(string)
  }

  new_string <- set_strings[which(tolower(string) == tolower(set_strings))[1]]

  return(ifelse(is.na(new_string), string, new_string))
}

set_collapse <- function(values) {
  return(paste0(shQuote(values), collapse=", "))
}

has_year_in_env <- function(env) {
  return(regex_contains("[0-9]{4}_.+", as.character(env)))
}

split_env_year <- function(envs) {
  splitted_envs <- strsplit(as.character(envs), "_")

  return(list(years=as.factor(sapply(splitted_envs, function(x) x[1])),
              envs=as.factor(sapply(splitted_envs, function(x) x[2]))))
}

is_time_serie_like_cv_type <- function(cv_class) {
  return(cv_class %in% c("CV0Custom", "CV0OrderedCV", "CV0OrderedCustomCV"))
}

needs_prepare_predictor_in_fold <- function(cv_class) {
  return(is_time_serie_like_cv_type(cv_class) ||
         cv_class == "CustomCV")
}

close_all_devices <- function() {
  invisible(sapply(dev.list(), dev.off))
}

shead <- function(Data, n = 5) {
  return(Data[1:min(n, nrow(Data)), 1:min(n, ncol(Data))])
}

stail <- function(Data, n = 5) {
  n_rows <- nrow(Data)
  n_cols <- ncol(Data)
  return(Data[max(1, n_rows - n):n_rows, max(1, n_cols - n):n_cols])
}

lm_intercept <- function(x, y) {
  if (all(is.na(x)) || all(is.na(y))) {
    return(NaN)
  }
  Data <- data.frame(x = x, y = y)

  return(lm(y ~ x, data = Data)$coefficients[1])
}

lm_slope <- function(x, y) {
  if (all(is.na(x)) || all(is.na(y))) {
    return(NaN)
  }
  Data <- data.frame(x = x, y = y)

  return(lm(y ~ x, data = Data)$coefficients[2])
}

char_at <- function(string, index = 1) {
  return(substr(string, index, index))
}

str_join <- function(string1, string2) {
  joined_strings <- paste0(string1, string2)

  nas_strings <- which(is.na(string1) | is.na(string2))

  joined_strings[nas_strings] <- NA

  return(joined_strings)
}

has_dims <- function(x, dim_num = 2) length(dim(x)) == dim_num

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

  return(invisible(x))
}
