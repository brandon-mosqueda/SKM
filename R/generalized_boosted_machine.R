#' @include utils.R
#' @include validator.R
#' @include random_forest_model.R

#' @title Fit a Generalized Boosted Machine (GBM)
#'
#' @templateVar ClassName GeneralizedBoostedMachineModel
#' @templateVar XType matrix
#' @templateVar YType `vector`
#' @templateVar refFunction gbm::gbm.fit()
#'
#' @description
#' `generalized_boosted_machine()` is a wrapper of the [gbm::gbm()] function
#' with the ability to tune the hyperparameters (grid search) in a simple way.
#' It fits univariate models for continuous, count, binary and categorical
#' response variables.
#' @template tunable-description
#'
#' @template x-data-frame-param
#' @param y (`data.frame` | `vector` | `matrix`) The response (dependent)
#'   variable. If it is a `data.frame` or a `matrix` it must have only one
#'   column. If `y` is `character`, `logical` or `factor` a categorical response
#'   is assumed. When the response is categorical with only two classes a binary
#'   distribution is assumed, with more than two classes a multinomial
#'   distribution. When the response variable is numeric with only integers
#'   values greater or equals than zero a poisson distribution is assumed,
#'   gaussian distribution otherwise.
#' @param trees_number (`numeric`) (__tunable__) Number of trees. This is
#'   equivalent to the number of iterations and the number of basis functions in
#'   the additive expansion. 500 by default.
#' @param max_depth (`numeric`) (__tunable__) Maximum depth of each tree
#'   (the highest level of variable interactions allowed). A value of 1 implies
#'   an additive model, a value of 2 implies a model with up to 2-way
#'   interactions, etc. 1 by default.
#' @param node_size (`numeric`) (__tunable__) Also known as `n.minobsinnode`.
#'   Minimum number of observations in the terminal nodes of the trees. 10 by
#'   default.
#' @param shrinkage (`numeric`) (__tunable__) A shrinkage parameter applied to
#'   each tree in the expansion. Also known as the learning rate or step-size
#'   reduction. 0.001 to 0.1 usually work, but a smaller learning rate typically
#'   requires more trees. 0.1 by default.
#' @param sampled_records_proportion (`numeric`) (__tunable__) Also known as
#'   `bag.fraction`. The fraction of the training set observations randomly
#'   selected to propose the next tree in the expansion. This introduces
#'   randomnesses into the model fit. 0.5 by default.
#' @template cv-tune-params
#' @param predictors_relationship (`numeric`) Also known as `var.monotone`.
#'   A vector with the same length as the number of predictors, indicating which
#'   variables have a monotone increasing (+1), decreasing (-1), or arbitrary
#'   (0) relationship with the outcome. `NULL` by default.
#' @template other-base-params
#'
#' @template details-remove-nas
#' @template details-tuning
#' @template details-uni-loss-functions
#'
#' @template return-model
#'
#' @seealso [predict.Model()]
#' @family models
#'
#' @examples
#' \dontrun{
#' # Fit with all default parameters
#' model <- generalized_boosted_machine(to_matrix(iris[, -5]), iris$Species)
#'
#' # Tune 3 hyperparameters
#' model <- generalized_boosted_machine(
#'   to_matrix(iris[, -1]),
#'   iris$Sepal.Length,
#'   trees_number = c(100, 200, 300),
#'   node_size = c(5, 10),
#'   shrinkage = c(0.001, 0.01, 0.1),
#'   sampled_records_proportion = c(0.5, 0.75)
#' )
#'
#' predictions <- predict(model, iris)
#' predictions$predicted
#'
#' # See the whole grid
#' model$hyperparams_grid
#' }
#'
#' @export
generalized_boosted_machine <- function(x, y,

                                        trees_number = 500,
                                        max_depth = 1,
                                        node_size = 10,
                                        shrinkage = 0.1,
                                        sampled_records_proportion = 0.5,

                                        tune_cv_type = "K_fold",
                                        tune_folds_number = 5,
                                        tune_testing_proportion = 0.2,
                                        tune_grid_proportion = 1,

                                        predictors_relationship = NULL,

                                        validate_params = TRUE,
                                        seed = NULL,
                                        verbose = TRUE) {
  assert_logical(validate_params, len = 1, any.missing = FALSE)

  if (validate_params) {
    validate_generalized_boosted_machine(
      x = x,
      y = y,

      trees_number = trees_number,
      max_depth = max_depth,
      node_size = node_size,
      shrinkage = shrinkage,
      sampled_records_proportion = sampled_records_proportion,

      tune_cv_type = tune_cv_type,
      tune_folds_number = tune_folds_number,
      tune_testing_proportion = tune_testing_proportion,
      tune_grid_proportion = tune_grid_proportion,

      predictors_relationship = predictors_relationship,

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

  model <- GeneralizedBoostedMachineModel$new(
    x = x,
    y = y,

    trees_number = trees_number,
    max_depth = max_depth,
    node_size = node_size,
    shrinkage = shrinkage,
    sampled_records_proportion = sampled_records_proportion,

    tune_cv_type = tune_cv_type,
    tune_folds_number = tune_folds_number,
    tune_testing_proportion = tune_testing_proportion,
    tune_grid_proportion = tune_grid_proportion,

    predictors_relationship = predictors_relationship
  )

  wrapper_function <- get_verbose_function(verbose)
  wrapper_function(model$fit())

  end_time <- Sys.time()
  model$execution_time <- difftime(end_time, start_time)

  wrapper_function(print_model_time_execution(model$execution_time))

  return(model)
}
