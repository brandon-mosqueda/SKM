#' @param tune_cv_type (`character(1)`) (case not sensitive) The type of cross
#'   validation to tune the model. The options are `"K_fold"` and
#'   `"Random"`. `"K_fold"` by defaul.
#' @param tune_folds_number (`numeric(1)`) The number of folds to tune each
#'   combination in the grid (k in k-fold cross validation). 5 by default.
#' @param tune_testing_proportion (`numeric(1)`) A number > 0 and < 1 to
#'   specify the proportion of records to use as validation set when
#'   `tune_cv_type` is `"Random"`. 0.2 by default.
#' @param tune_grid_proportion (`numeric(1)`) A number > 0 and <= 1 to specify
#'   the proportion of combinations to sample from the grid and evaluate in
#'   tuning (useful when the grid is big). 1 by default.
