#' @param tune_cv_type (`character(1)`) (case not sensitive) The type of cross
#'   validation to tune the model. The options are `"K_fold"` and
#'   `"Random"`. `"K_fold"` by defaul.
#' @param tune_folds_number (`numeric(1)`) The number of folds to tune each
#'   combination in the grid (k in k-fold cross validation). 5 by default.
#' @param tune_testing_proportion (`numeric(1)`) A number > 0 and < 1 to
#'   specify the proportion of records to use as validation set when
#'   `tune_cv_type` is `"Random"`. 0.2 by default.
#' @param tune_folds (`list`) Custom folds for tuning. It must be a `list` of
#'   `list`'s where each entry will represent a fold. Each inner `list` has to
#'   contain the fields `"training"` and `"testing"` with numeric vectors of
#'   indices of those entries to be used as training and testing in each fold.
#'   Note that when this parameter is set `tune_cv_type`, `tune_folds_number`
#'   and `tune_testing_proportion` are ignored. `NULL` by default.
#' @param tune_loss_function (`character(1)`) (case not sensitive) The loss
#'   function to use in tuning. The options are `"mse"`, `"maape"`, `"mae"`,
#'   `"nrmse"` or `"rmse"` when `y` is a numeric response variable,
#'   `"accuracy"` or `"kappa_coeff"` when `y` is a categorical response
#'   variable (including binary) and `"f1_score"`, `"roc_auc"` or `"pr_auc"`
#'   when `y` is a binary response variable. `NULL` by default which uses
#'   `"mse"` for numeric variables and `"accuracy"` for categorical variables.
#' @param tune_grid_proportion (`numeric(1)`) A number > 0 and <= 1 to specify
#'   the proportion of combinations to sample from the grid and evaluate in
#'   tuning (useful when the grid is big). 1 by default.
