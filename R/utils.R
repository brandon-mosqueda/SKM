#' @importFrom reticulate py_suppress_warnings py_capture_output
#' @importFrom data.table fwrite

#' @include globals.R
#' @include validator.R

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
#' @description
#' Compute the Choleski factorization of a real symmetric positive-definite
#' square matrix. If it fails, compute the Cholesky factorization of a non-real
#' symmetric positive-definite square matrix.
#'
#' @param x (`matrix`) The data to compute the cholesky.
#'
#' @return The cholesky `matrix`.
#'
#' @examples
#' \dontrun{
#' # Import the genomic data
#' data(Maize)
#'
#' Cholesky <- cholesky(to_matrix(Maize$Geno[, -1]))
#' dim(Cholesky)
#' head(Cholesky[, 5])
#' }
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

replace_at_list <- function(original, new_values) {
  for (field in names(new_values)) {
    original[[field]] <- new_values[[field]]
  }

  return(original)
}

write_csv <- function(data,
                      file,
                      quote = FALSE,
                      na = "NA",
                      ...) {
  return(fwrite(
    data,
    file = file,
    quote = quote,
    na = na,
    ...
  ))
}

get_cols_names <- function(x) {
  cols_names <- colnames(x)

  if (is.null(cols_names)) {
    cols_names <- paste0("x", seq(ncol(x)))
  } else {
    cols_no_name <- which(cols_names == "")

    if (!is_empty(cols_no_name)) {
      cols_names[cols_no_name] <- paste0("x", seq_along(cols_no_name))
    }
  }

  return(cols_names)
}

as_tf_rates <- function(confusion_matrix) {
  # These values are considered because in most functions and in this package
  # the second level is considered as the reference level, that is, the TRUE or
  # 1 class and confusion matrix is created putting in the first row/cols the
  # FALSE class (first level of a factor)
  return(list(
    tp = as.numeric(confusion_matrix[2, 2]),
    tn = as.numeric(confusion_matrix[1, 1]),
    fp = as.numeric(confusion_matrix[1, 2]),
    fn = as.numeric(confusion_matrix[2, 1])
  ))
}

#' @title Convert data to matrix
#'
#' @description
#' Given an object creates a design matrix.
#'
#' @param x (`any`) The object to be converted to matrix.
#' @param with_intercept (`logical(1)`) Should be the `"(Intercept)"` column
#'   added at the beginning? `FALSE` by default.
#' @param remove_na (`logical(1)`) Should `NA` values be removed?. `FALSE` by
#'   default.
#'
#' @details
#' The following rules are applied when converting the object to matrix
#' depending the object's type:
#'
#' * `numeric` vector: It is converted to a one column matrix.
#' * `character`, `logical` or `factor` vectors: All of these are considered
#'    to be categorical variables and therefore a dummy matrix is created with
#'    all categories (or unique values) without the firts one, so a
#'   `n x (categories_number - 1)` matrix is created.
#' * `data.frame`: All columns are included but `character`, `logical` or
#'   `factor` are included in the same way as described for vectors.
#'
#' For all the columns a name is assigned if they has no one.
#' The intercept is always added at the first column with the name
#' `"(Intercept)"`.
#'
#' @return
#' A matrix.
#'
#' @examples
#' \dontrun{
#' to_matrix(iris)
#' to_matrix(1:10)
#' to_matrix(c("a", "b", "c"))
#' to_matrix(data.frame(a = c("a", "b"), b = c("c", "d")))
#' }
#'
#' @export
to_matrix <- function(x, with_intercept = FALSE, remove_na = FALSE) {
  if (is.null(x)) {
    return(NULL)
  } else if (na.rm) {
    x <- na.omit(x)
  }

  if (is.vector(x)) {
    if (is.character(x) || is.logical(x)) {
      x <- data.frame(x = factor(x))
    } else {
      x <- matrix(x)
    }
  } else if (is.factor(x)) {
    x <- data.frame(x)
  } else if (is.matrix(x) && !is.numeric(x)) {
    x <- data.frame(x)
    x[] <- lapply(x, factor)
  }

  if (is.matrix(x)) {
    colnames(x) <- get_cols_names(x)

    if (with_intercept) {
      x <- cbind(1, x)
      colnames(x)[1] <- "(Intercept)"
    }
  } else if (is.data.frame(x)) {
    colnames(x) <- get_cols_names(x)

    current_na_state <- options()$na.action
    if (!na.rm) {
      options(na.action = "na.pass")
    }

    x <- model.matrix(~., x)

    options(na.action = current_na_state)

    if (!with_intercept) {
      x <- x[, -1, drop = FALSE]
    }
  }

  return(x)
}

#' @title Convert data to data.frame
#'
#' @description
#' Given an object convert it to `data.frame`.
#'
#' @param x (`any`) The object to be converted to `data.frame`.
#' @param remove_na (`logical(1)`) Should `NA` values be removed?. `FALSE` by
#'   default.
#'
#' @details
#' The following rules are applied when converting the object to matrix
#' depending the object's type:
#'
#' * `numeric` vector: It is converted to a one column `data.frame`.
#' * `character`, `logical` and `factor` vectors: All of these are considered
#'    to be categorical variables and therefore a coerced to `factor` in a one
#'    column `data.frame`.
#' * `matrix`: If it is `character`, `logical` or `factor` are included in the
#'   same way as described for vectors and only coerced to `data.frame` for
#'   `numeric` matrices.
#'
#' For all the columns a name is assigned if they has no one.
#'
#' @return
#' A `data.frame`.
#'
#' @examples
#' \dontrun{
#' to_data_frame(iris)
#' to_data_frame(1:10)
#' to_data_frame(c("a", "b", "c"))
#' to_data_frame(data.frame(a = c("a", "b"), b = c("c", "d")))
#' }
#'
#' @export
to_data_frame <- function(x, remove_na = FALSE) {
  x <- as.data.frame(x, check.names = FALSE)

  x[] <- lapply(x, function(x) {
    return(if (is.character(x) || is.logical(x)) factor(x) else x)
  })

  colnames(x) <- get_cols_names(x)

  if (remove_na) {
    x <- na.omit(x)
  }

  return(x)
}

#' @export
remove_no_variance_cols <- function(x) {
  if (!has_dims(x)) {
    stop("x must be a data.frame or a matrix")
  }

  cols_variances <- apply(x, 2, function(x) var(x, na.rm = TRUE))
  zero_variances_cols <- which(is.na(cols_variances) | cols_variances == 0)
  names(zero_variances_cols) <- NULL

  if (!is_empty(zero_variances_cols)) {
    x <- x[, -zero_variances_cols]
    attr(x, "removed_cols") <- zero_variances_cols
  }

  return(x)
}

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

which_is_na <- function(x) {
  if (has_dims(x)) {
    indices <- which(is.na(x), arr.ind = TRUE)
    if (!is_empty(indices)) {
      indices <- indices[, "row"]
    } else {
      indices <- NULL
    }
  } else {
    indices <- which(is.na(x))
    if (is_empty(indices)) {
      indices <- NULL
    }
  }

  return(indices)
}

nas_indices <- function(x, y) {
  x_nas <- which_is_na(x)
  y_nas <- which_is_na(y)

  return(union(x_nas, y_nas))
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

  return(invisible(temp))
}

py_hush <- function(code) {
  py_capture_output(py_suppress_warnings(temp <- code))

  return(temp)
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

get_all_levels <- function(x, y) {
  all_levels <- NULL

  if (is.factor(x)) {
    all_levels <- levels(x)
  }
  if (is.factor(y)) {
    all_levels <- c(all_levels, levels(y))
  }

  all_levels <- na.omit(union(all_levels, union(x, y)))

  return(all_levels)
}

get_levels <- function(x, y, all_levels = NULL, positive_class = NULL) {
  if (is.null(all_levels)) {
    all_levels <- get_all_levels(x, y)
  }

  if (length(all_levels) == 1) {
    all_levels <- c("OtherClass", all_levels)
  }

  if (
    length(all_levels) == 2 &&
    !is.null(positive_class) &&
    all_levels[2] != positive_class
  ) {
    if (positive_class != all_levels[1]) {
      stop(sprintf(paste0(
        "Your data has any element of positive_class (%s), try to set ",
        "all_levels parameter with it."
      )), positive_class)
    }
    # The second level in binary responses is always the positive level
    all_levels <- c(all_levels[2], positive_class)
  }

  return(all_levels)
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
echo <- function(format, ..., end = "\n", file = "", append = TRUE) {
  if (is.null(format)) {
    format <- "NULL"
  } else if (is.na(format)) {
    format <- "NA"
  }

  invisible(cat(
    sprintf(format, ...),
    end,
    sep = "",
    file = file,
    append = append
  ))
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

is_binary_loss <- function(loss_function) {
  valid_binary_loss_functions <- c(
    TUNE_BINARY_LOSS_FUNCTIONS,
    TUNE_CATEGORICAL_LOSS_FUNCTIONS
  )

  return(loss_function %in% valid_binary_loss_functions)
}

is_numeric_loss <- function(loss_function) {
  return(loss_function %in% TUNE_NUMERIC_LOSS_FUNCTIONS)
}

is_categorical_loss <- function(loss_function) {
  return(loss_function %in% TUNE_CATEGORICAL_LOSS_FUNCTIONS)
}

need_invert_loss <- function(loss_function) {
  return(loss_function %in% NEED_INVERT_LOSS)
}

# Cross validation --------------------------------------------------

#' @title K-fold cross validation folds generation
#'
#' @description
#' Generates folds for the classic k-fold cross validation where k mutually
#' exclusive folds are generated and the training phase is done using k − 1
#' folds and the testing with the remaining one, which ensures all individuals
#' are part of the testing once.
#'
#' @param records_number (`numeric(1)`) The expected number of elements to be
#'   included in the folds.
#' @param k (`numeric(1)`) The number of folds. 5 by default.
#'
#' @return
#' A `list` with `k` elements where each element is a named `list` with the
#' elements `training` wich includes the indices of those records to be part of
#' the training set and `testing` wich includes the indices of those records to
#' be part of the testing set. Training and testing sets of each fold are
#' exhaustive and mutually exclusive.
#'
#' @examples
#' \dontrun{
#' # Generates 5 folds of 2 elements (10 / 5) in testing set
#' folds <- cv_kfold(10, 5)
#' # Indices of training set in fold 1
#' folds[[1]]$training
#' # Indices of testing set in fold 1
#' folds[[1]]$testing
#' folds[[2]]$training
#' folds[[2]]$testing
#'
#' folds <- cv_kfold(100, 30)
#' # List with indices of training and testing of fold 1
#' folds[[1]]
#' # List with indices of training and testing of fold 2
#' folds[[2]]
#' folds[[3]]
#' # ...
#' folds[[30]]
#' }
#'
#' @export
cv_kfold <- function(records_number, k = 5) {
  assert_cv_kfold(records_number, k)

  cross_validator <- KFoldCV$new(
    folds_number = k,
    records_number = records_number
  )

  return(cross_validator$get_folds())
}

#' @title Stratified K-fold cross validation folds generation
#'
#' @description
#' Generates folds for the stratified k-fold cross validation where k mutually
#' exclusive folds are generated and the training phase is done using k − 1
#' folds and the testing with the remaining one, which ensures all individuals
#' are part of the testing once. Given a categorical variable this type of cross
#' validation ensures each fold contains the same proportion of elements of each
#' class, so it is a good option for balanced folds.
#'
#' @param data (`factor`) The categorical data considered to stratify the folds.
#' @param k (`numeric(1)`) The number of folds. 5 by default.
#'
#' @return
#' A `list` with `k` elements where each element is a named `list` with the
#' elements `training` wich includes the indices of those records to be part of
#' the training set and `testing` wich includes the indices of those records to
#' be part of the testing set. Training and testing sets of each fold are
#' exhaustive and mutually exclusive.
#'
#' @examples
#' \dontrun{
#' # Generates 5 folds of 2 elements (10 / 5) in testing set
#' data <- factor(c(rep("A", 10), rep("B", 20), rep("C", 30)))
#' folds <- cv_kfold_strata(data, 5)
#' # Indices of training set in fold 1
#' folds[[1]]$training
#' # Indices of testing set in fold 1
#' folds[[1]]$testing
#' # Verify fold 1 is balanced in training
#' table(data[folds[[1]]$training])
#' # Verify fold 1 is balanced in testing
#' table(data[folds[[1]]$testing])
#' #' # Verify fold 2 is balanced in training
#' table(data[folds[[2]]$training])
#' # Verify fold 2 is balanced in testing
#' table(data[folds[[2]]$testing])
#'
#' folds <- cv_kfold_strata(iris$Species, 30)
#' # List with indices of training and testing of fold 1
#' folds[[1]]
#' # List with indices of training and testing of fold 2
#' folds[[2]]
#' folds[[3]]
#' # ...
#' folds[[30]]
#' }
#'
#' @export
cv_kfold_strata <- function(data, k = 5) {
  assert_cv_kfold_strata(data, k)
  data <- droplevels(data)

  cross_validator <- KFoldStrataCV$new(
    folds_number = k,
    data = data
  )

  return(cross_validator$get_folds())
}

#' @title Random cross validation folds generation
#'
#' @description
#' Generates folds for cross validation where you specify the number of folds
#' and the proportion of testing. In each fold a sample without replacement of
#' the specified proportion of testing individuals is taken to be the testing
#' set and all the remaining ones to be the training set.
#'
#' @param records_number (`numeric(1)`) The expected number of elements to be
#'   included in the folds.
#' @param folds_number (`numeric(1)`) The number of folds. 5 by default.
#' @param testing_proportion (`numeric(1)`) The proportion of elements to be
#'   included in the testing set in each fold. 0.2 by default.
#'
#' @return
#' A `list` with `folds_number` elements where each element is a named `list`
#' with the elements `training` wich includes the indices of those records to be
#' part of the training set and `testing` wich includes the indices of those
#' records to be part of the testing set. Training and testing sets of each fold
#' are exhaustive and mutually exclusive.
#'
#' @examples
#' \dontrun{
#' # Generates 5 folds of 2 elements (10 / 5) in testing set
#' folds <- cv_random(10, 5, 0.2)
#' # Indices of training set in fold 1
#' folds[[1]]$training
#' # Indices of testing set in fold 1
#' folds[[1]]$testing
#' folds[[2]]$training
#' folds[[2]]$testing
#'
#' # Generates 30 folds with 30 elements in testing set
#' folds <- cv_kfold(100, 30, 0.3)
#' # List with indices of training and testing of fold 1
#' folds[[1]]
#' # List with indices of training and testing of fold 2
#' folds[[2]]
#' folds[[3]]
#' # ...
#' folds[[30]]
#' }
#'
#' @export
cv_random <- function(records_number,
                      folds_number = 5,
                      testing_proportion = 0.2) {
  assert_cv_random(
    records_number = records_number,
    folds_number = folds_number,
    testing_proportion = testing_proportion
  )

  cross_validator <- RandomCV$new(
    folds_number = folds_number,
    records_number = records_number,
    testing_proportion = testing_proportion
  )

  return(cross_validator$get_folds())
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
