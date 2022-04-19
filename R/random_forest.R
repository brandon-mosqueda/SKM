#' @include utils.R
#' @include validator.R
#' @include random_forest_model.R

#' @title Fit a Random Forest Model
#'
#' @templateVar ClassName RandomForestModel
#' @templateVar XType `matrix`
#' @templateVar YType `vector` or `data.frame`
#' @templateVar refFunction randomForestSRC::rfsrc()
#'
#' @description
#' `random_forest()` is a wrapper of the [randomForestSRC::rfsrc()] function to
#' fit a random forest model with the ability to tune the hyperparameters with
#' grid search or bayesian optimization in a simple way. You can fit univariate
#' or multivariate models for numeric and/or categorical response variables.
#' @template tunable-description
#'
#' @template x-matrix-param
#' @param y (`data.frame` | `vector` | `matrix`) The response (dependent)
#'   variable(s). If it is a `data.frame` or a `matrix` with 2 or more columns,
#'   a multivariate model is assumed, a univariate model otherwise. If `y` is
#'   (or contains some columns) `character`, `logical` or `factor`, a
#'   categorical variable is assumed, numeric otherwise. In multivariate models
#'   categorical and numeric variables can be combined for a mixed model.
#' @param trees_number (`numeric`) (__tunable__) Number of trees. 500 by
#'   default.
#' @param node_size (`numeric`) (__tunable__) Minimum size of terminal
#'   nodes. 5 by default.
#' @param node_depth (`numeric`) (__tunable__) Maximum depth to which a tree
#'   should be grown. `NULL` (ignored) by default.
#' @param sampled_x_vars_number (`numeric`) (__tunable__) Also known as `mtry`.
#'   Number of variables randomly selected as candidates for splitting a node.
#'   You can specify values between (0, 1] with the proportion of variables in `x`
#'   or directly the number of variables to use or a combination of both. `NULL`
#'   by default, which uses `p / 3` with numeric response variables or `sqrt(p)`
#'   otherwise, where `p` is the number of variables in `x`.
#' @template cv-tune-params
#' @param split_rule (`character(1)`) (case not sensitive) Splitting rule. The
#'   available options are `"mse"`, `"gini"`, `"auc"`, `"entropy"`. `NULL` by
#'   default (which selects the best depending on the type of response
#'   variable. For more information, see Details section below).
#' @param splits_number (`numeric(1)`) Non-negative integer value for number of
#'   random splits to consider for each candidate splitting variable. 10 by
#'   default.
#' @param x_vars_weights (`numeric`) Vector of non-negative weights (does not
#'   have to sum to 1) representing the probability of selecting a variable for
#'   splitting. `NULL` by default (uniform weights).
#' @param records_weights (`numeric`) Vector of non-negative weights (does not
#'   have to sum to 1) for sampling cases. Observations with larger weights will
#'   be selected with higher probability in the bootstrap (or subsampled)
#'   samples. `NULL` by default (uniform weights).
#' @param na_action (`character(1)`) (case not sensitive) Action taken if the
#'   data contains `NA`'s. The available options are `"omit"` (remove all
#'   records with `NA`'s) and `"impute"` (impute missing values). `"omit"` by
#'   default.
#' @template other-base-params
#'
#' @template details-tuning
#' @template details-uni-loss-functions
#' @details
#' ## split_rule
#'
#' * `"mse"`: Implements weighted Mean Squared Error splitting for numeric
#'   response variables.
#' * `"gini"`: Implements Gini index splitting for categorical response
#'   variables.
#' * `"auc"`: AUC (area under the ROC curve) splitting for both two-class and
#'   multiclass setttings. AUC splitting is appropriate for imbalanced data.
#' * `"entropy"`: entropy splitting for categorical response variables.
#' * Multivariate analysis: When one or both numeric and categorical
#'   responses are detected, a multivariate normalized composite split rule of
#'   Mean Squared Error and Gini index splitting is invoked.
#'
#' @template return-model
#'
#' @seealso [predict.Model()], [coef.Model()]
#' @family models
#'
#' @examples
#' \dontrun{
#' # Use all default hyperparameters (no tuning) -------------------------------
#' x <- to_matrix(iris[, -5])
#' y <- iris$Species
#' model <- random_forest(x, y)
#'
#' # Obtain the variables importance
#' coef(model)
#'
#' # Predict using the fitted model
#' predictions <- predict(model, x)
#' # Obtain the predicted values
#' predictions$predicted
#' # Obtain the predicted probabilities
#' predictions$probabilities
#'
#' # Tune with grid search -----------------------------------------------------
#' x <- to_matrix(iris[, -1])
#' y <- iris$Sepal.Length
#' model <- random_forest(
#'   x,
#'   y,
#'   trees_number = c(100, 200, 300),
#'   node_size = c(1, 2),
#'   node_depth = c(10, 15),
#'   tune_type = "grid_search",
#'   tune_cv_type = "k_fold",
#'   tune_folds_number = 5
#' )
#'
#' # Obtain the whole grid with the loss values
#' model$hyperparams_grid
#' # Obtain the hyperparameters combination with the best loss value
#' model$best_hyperparams
#'
#' # Predict using the fitted model
#' predictions <- predict(model, x)
#' # Obtain the predicted values
#' predictions$predicted
#'
#' # Tune with Bayesian optimization -------------------------------------------
#' x <- to_matrix(iris[, -1])
#' y <- iris$Sepal.Length
#' model <- random_forest(
#'   x,
#'   y,
#'   trees_number = list(min = 100, max = 500),
#'   node_size = list(min = 1, max = 10),
#'   tune_type = "bayesian_optimization",
#'   tune_bayes_samples_number = 5,
#'   tune_bayes_iterations_number = 5,
#'   tune_cv_type = "random",
#'   tune_folds_number = 4
#' )
#'
#' # Obtain the whole grid with the loss values
#' model$hyperparams_grid
#' # Obtain the hyperparameters combination with the best loss value
#' model$best_hyperparams
#'
#' # Predict using the fitted model
#' predictions <- predict(model, x)
#' # Obtain the predicted values
#' predictions$predicted
#'
#' # Obtain the variables importance
#' coef(model)
#'
#' # Obtain the execution time taken to tune and fit the model
#' model$execution_time
#'
#' # Multivariate analysis -----------------------------------------------------
#' x <- to_matrix(iris[, -c(1, 5)])
#' y <- iris[, c(1, 5)]
#' model <- random_forest(x, y, trees_number = 100)
#'
#' # Predict using the fitted model
#' predictions <- predict(model, x)
#' # Obtain the predicted values of the first response
#' predictions$Sepal.Length$predicted
#' # Obtain the predicted values and probabilities of the second response
#' predictions$Species$predicted
#' predictions$Species$probabilities
#'
#' # Obtain the predictions in a data.frame not in a list
#' predictions <- predict(model, x, format = "data.frame")
#' head(predictions)
#' }
#'
#' @export
random_forest <- function(x, y,

                          trees_number = 500,
                          node_size = 5,
                          node_depth = NULL,
                          sampled_x_vars_number = NULL,

                          tune_type = "Grid_search",
                          tune_cv_type = "K_fold",
                          tune_folds_number = 5,
                          tune_testing_proportion = 0.2,
                          tune_folds = NULL,
                          tune_loss_function = NULL,
                          tune_grid_proportion = 1,
                          tune_bayes_samples_number = 10,
                          tune_bayes_iterations_number = 10,

                          split_rule = NULL,
                          splits_number = 10,
                          x_vars_weights = NULL,
                          records_weights = NULL,
                          na_action = "omit",

                          validate_params = TRUE,
                          seed = NULL,
                          verbose = TRUE) {
  assert_logical(validate_params, len = 1, any.missing = FALSE)

  is_multivariate <- NCOL(y) > 1

  if (validate_params) {
    validate_random_forest(
      x = x,
      y = y,
      is_multivariate = is_multivariate,

      trees_number = trees_number,
      node_size = node_size,
      node_depth = node_depth,
      sampled_x_vars_number = sampled_x_vars_number,

      tune_type = tune_type,
      tune_cv_type = tune_cv_type,
      tune_folds_number = tune_folds_number,
      tune_testing_proportion = tune_testing_proportion,
      tune_folds = tune_folds,
      tune_loss_function = tune_loss_function,
      tune_grid_proportion = tune_grid_proportion,
      tune_bayes_samples_number = tune_bayes_samples_number,
      tune_bayes_iterations_number = tune_bayes_iterations_number,

      split_rule = split_rule,
      splits_number = splits_number,
      x_vars_weights = x_vars_weights,
      records_weights = records_weights,
      na_action = na_action,

      seed = seed,
      verbose = verbose
    )
  }

  old_random_state <- NULL
  if (!is.null(seed)) {
    old_random_state <- get_rand_state()

    set.seed(seed)
  }
  on.exit(set_rand_state(old_random_state))

  start_time <- Sys.time()

  model <- RandomForestModel$new(
    x = x,
    y = y,
    is_multivariate = is_multivariate,

    trees_number = trees_number,
    node_size = node_size,
    node_depth = node_depth,
    sampled_x_vars_number = sampled_x_vars_number,

    tune_type = tune_type,
    tune_cv_type = tune_cv_type,
    tune_folds_number = tune_folds_number,
    tune_testing_proportion = tune_testing_proportion,
    tune_folds = tune_folds,
    tune_loss_function = tune_loss_function,
    tune_grid_proportion = tune_grid_proportion,
    tune_bayes_samples_number = tune_bayes_samples_number,
    tune_bayes_iterations_number = tune_bayes_iterations_number,

    split_rule = split_rule,
    splits_number = splits_number,
    x_vars_weights = x_vars_weights,
    records_weights = records_weights,
    na_action = na_action
  )

  wrapper_function <- get_verbose_function(verbose)
  wrapper_function(model$fit())

  end_time <- Sys.time()
  model$execution_time <- difftime(end_time, start_time)

  wrapper_function(print_model_time_execution(model$execution_time))

  return(model)
}
