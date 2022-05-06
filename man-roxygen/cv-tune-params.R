#' @param tune_type (`character(1)`) (case not sensitive) The type of tuning to
#'   perform. The options are `"Grid_search"` and "`Bayesian_optimization`".
#'   `"Grid_search"` by default.
#' @param tune_cv_type (`character(1)`) (case not sensitive) The type of cross
#'   validation to tune the model. The options are `"K_fold"` and
#'   `"Random"`. `"K_fold"` by defaul.
#' @param tune_folds_number (`numeric(1)`) The number of folds to tune each
#'   hyperparameter combination (k in k-fold cross validation). 5 by default.
#' @param tune_testing_proportion (`numeric(1)`) A number between (0, 1) to
#'   specify the proportion of records to use as validation set when
#'   `tune_cv_type` is `"Random"`. 0.2 by default.
#' @param tune_folds (`list`) Custom folds for tuning. It must be a `list` of
#'   `list`'s where each entry will represent a fold. Each inner `list` has to
#'   contain the fields `"training"` and `"testing"` with numeric vectors of
#'   indices of those entries to be used as training and testing in each fold.
#'   Note that when this parameter is set, `tune_cv_type`, `tune_folds_number`
#'   and `tune_testing_proportion` are ignored. `NULL` by default.
#' @param tune_loss_function (`character(1)`) (case not sensitive) The loss
#'   function to use in tuning. The options are `"mse"`, `"maape"`, `"mae"`,
#'   `"nrmse"` or `"rmse"` when `y` is a numeric response variable,
#'   `"accuracy"` or `"kappa_coeff"` when `y` is a categorical response
#'   variable (including binary) and `"f1_score"`, `"roc_auc"` or `"pr_auc"`
#'   when `y` is a binary response variable. `NULL` by default which uses
#'   `"mse"` for numeric variables and `"accuracy"` for categorical variables.
#' @param tune_grid_proportion (`numeric(1)`) Only when `tune_type` is
#'   `"Grid_search"`, a number between (0, 1] to specify the proportion of
#'   hyperparameters combinations to sample from the grid and evaluate in
#'   tuning (useful when the grid is big). 1 by default (full grid).
#' @param tune_bayes_samples_number (`numeric(1)`) Only when `tune_type` is
#'  `"Bayesian_optimization"`, the number of initial random hyperparameters
#'  combinations to evalute before the Bayesian optimization process. 10 by
#'  default.
#' @param tune_bayes_iterations_number (`numeric(1)`) Only when `tune_type` is
#'  `"Bayesian_optimization"`, the number of optimization iterations to
#'  evaluate after the initial random samples specified in
#'  `tune_bayes_samples_number`. 10 by default.
