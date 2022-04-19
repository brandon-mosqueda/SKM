#' @include utils.R
#' @include validator.R
#' @include generalized_linear_model_model.R

#' @title Fit a Penalized Generalized Linear Model
#'
#' @templateVar ClassName GeneralizedLinearModel
#' @templateVar XType matrix
#' @templateVar YType `vector` or `matrix`
#' @templateVar refFunction glmnet::glmnet()
#'
#' @description
#' `generalized_linear_model()` is a wrapper of the [glmnet::glmnet()] function
#' to fit a generalized linear model with the ability to tune the
#' hyperparameters with grid search or bayesian optimization in a simple way.
#' You can fit univariate models for continuous, count, binary and categorical
#' response variables and multivariate models for numeric responses only.
#' @template tunable-description
#'
#' @template x-matrix-param
#' @param y (`data.frame` | `vector` | `matrix`) The response (dependent)
#'   variable(s). If it is a `data.frame` or a `matrix` with 2 or more columns,
#'   a multivariate model is assumed, a univariate model otherwise. In
#'   univariate models if `y` is `character`, `logical` or `factor` a
#'   categorical response is assumed. When the response is categorical with only
#'   two classes a logistic regression is assumed, with more than two classes a
#'   multinomial regression. When the response variable is numeric with only
#'   integers values greater or equals than zero a poisson regression is
#'   assumed, multiple regression otherwise. In multivariate models all
#'   responses are coerced to numeric and a multi-response gaussian regression
#'   is assumed.
#' @param alpha (`numeric`) (__tunable__) The elasticnet mixing parameter, with
#'   0 <= `alpha` <= 1. The penalty is defined as:
#'
#'   ![](glmnet_penalty.png "(1 - \alpha)/2 ||\beta||_2^2 + \alpha ||\beta||_1")
#'
#'   `alpha = 0` is the lasso penalty, `alpha = 1` is the ridge penalty and `0 <
#'   alpha < 1` is the elasticnet penalty. 1 by default.
#' @template cv-tune-params
#' @param lambdas_number (`numeric(1)`) The number of lambda values to be
#'   generated and evaluated in tuning. If `lambda` is provided, this parameter
#'   is ignored. 100 by default.
#' @param records_weights (`numeric`) Observation weights. `NULL` by default (1
#'   for each observation).
#' @param standardize (`logical(1)`) Should the `x` variables be standardized?
#'   The coefficients are always returned on the original scale. If variables
#'   are in the same units already, you might not wish to standardize. `TRUE` by
#'   default.
#' @param fit_intercept (`logical(1)`) Should intercept be fitted? `TRUE` by
#'   default.
#' @template other-base-params
#'
#' @template details-no-variance
#' @template details-remove-nas
#' @template details-tuning
#' @details
#' For efficiency tunning is made using the [glmnet::cv.glmnet()] function which
#' uses k-fold cross validation, so `tune_cv_type` and `tune_testing_proportion`
#' parameters are not included in this function.
#' @template details-uni-loss-functions
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
#' model <- generalized_linear_model(x, y)
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
#' # Tune with grid search --------------------------------------------------------
#' x <- to_matrix(iris[, -1])
#' y <- iris$Sepal.Length
#' model <- generalized_linear_model(
#'   x,
#'   y,
#'   alpha = c(0, 0.3, 0.6, 1),
#'   tune_type = "grid_search",
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
#' model <- generalized_linear_model(
#'   x,
#'   y,
#'   alpha = list(min = 0, max = 1),
#'   tune_type = "bayesian_optimization",
#'   tune_bayes_samples_number = 5,
#'   tune_bayes_iterations_number = 5
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
#' x <- to_matrix(iris[, -c(1, 2)])
#' y <- iris[, c(1, 2)]
#' model <- generalized_linear_model(x, y, alpha = 1)
#'
#' # Predict using the fitted model
#' predictions <- predict(model, x)
#' # Obtain the predicted values of the first response variable
#' predictions$Sepal.Length$predicted
#' # Obtain the predicted values of the second response variable
#' predictions$Sepal.Width$predicted
#'
#' # Obtain the predictions in a data.frame not in a list
#' predictions <- predict(model, x, format = "data.frame")
#' head(predictions)
#' }
#'
#' @export
generalized_linear_model <- function(x, y,

                                     alpha = 1,

                                     tune_type = "Grid_search",
                                     tune_folds_number = 5,
                                     tune_folds = NULL,
                                     tune_loss_function = NULL,
                                     tune_grid_proportion = 1,
                                     tune_bayes_samples_number = 10,
                                     tune_bayes_iterations_number = 10,

                                     lambdas_number = 100,
                                     records_weights = NULL,
                                     standardize = TRUE,
                                     fit_intercept = TRUE,

                                     validate_params = TRUE,
                                     seed = NULL,
                                     verbose = TRUE) {
  assert_logical(validate_params, len = 1, any.missing = FALSE)

  is_multivariate <- NCOL(y) > 1

  if (validate_params) {
    validate_generalized_linear_model(
      x = x,
      y = y,
      is_multivariate = is_multivariate,

      alpha = alpha,

      tune_type = tune_type,
      tune_cv_type = "K_fold",
      tune_folds_number = tune_folds_number,
      tune_testing_proportion = 0.2,
      tune_folds = tune_folds,
      tune_loss_function = tune_loss_function,
      tune_grid_proportion = tune_grid_proportion,
      tune_bayes_samples_number = tune_bayes_samples_number,
      tune_bayes_iterations_number = tune_bayes_iterations_number,

      lambdas_number = lambdas_number,
      records_weights = records_weights,
      standardize = standardize,
      fit_intercept = fit_intercept,

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

  model <- GeneralizedLinearModel$new(
    x = x,
    y = y,
    is_multivariate = is_multivariate,

    alpha = alpha,

    tune_type = tune_type,
    tune_cv_type = "K_fold",
    tune_folds_number = tune_folds_number,
    tune_testing_proportion = 0.2,
    tune_folds = tune_folds,
    tune_loss_function = tune_loss_function,
    tune_grid_proportion = tune_grid_proportion,
    tune_bayes_samples_number = tune_bayes_samples_number,
    tune_bayes_iterations_number = tune_bayes_iterations_number,

    lambdas_number = lambdas_number,
    records_weights = records_weights,
    standardize = standardize,
    fit_intercept = fit_intercept
  )

  wrapper_function <- get_verbose_function(verbose)
  wrapper_function(model$fit())

  end_time <- Sys.time()
  model$execution_time <- difftime(end_time, start_time)

  wrapper_function(print_model_time_execution(model$execution_time))

  return(model)
}
