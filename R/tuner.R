#' @importFrom R6 R6Class

#' @include utils.R
#' @include model_helpers.R
#' @include metrics.R

Tuner <- R6Class(
  classname = "Tuner",
  public = list(
    # Properties --------------------------------------------------

    x = NULL,
    y = NULL,
    responses = NULL,
    is_multivariate = NULL,
    hyperparams = NULL,
    fit_params = NULL,

    training_function = NULL,
    predict_function = NULL,
    loss_function = NULL,

    cross_validator = NULL,
    cv_type = NULL,
    folds_number = NULL,
    testing_proportion = NULL,

    all_combinations = NULL,
    combinations_number = NULL,
    best_combination = NULL,

    tabs_number = NULL,

    # Constructor --------------------------------------------------

    initialize = function(...,
                          x,
                          y,
                          responses,
                          is_multivariate,
                          training_function,
                          predict_function,
                          hyperparams,
                          fit_params,
                          cv_type,
                          folds_number,
                          testing_proportion,
                          loss_function = NULL,
                          tabs_number = 0) {
      self$x <- x
      self$y <- y

      remove_indices <- union(which_is_na(self$x), which_is_na(self$y))
      if (!is_empty(remove_indices)) {
        self$x <- get_records(self$x, -remove_indices)
        self$y <- get_records(self$y, -remove_indices)
      }

      self$training_function <- training_function
      self$predict_function <- predict_function
      self$hyperparams <- hyperparams
      self$cv_type <- cv_type
      self$folds_number <- folds_number
      self$testing_proportion <- testing_proportion
      self$responses <- responses
      self$is_multivariate <- is_multivariate
      self$loss_function <- nonull(
        loss_function,
        get_loss_function(self$responses, self$is_multivariate)
      )
      self$tabs_number <- tabs_number
      self$fit_params <- fit_params

      self$cross_validator <- get_cross_validator(
        type = self$cv_type,
        records_number = nrow(self$x),
        folds_number = self$folds_number,
        testing_proportion = self$testing_proportion
      )
    },
    eval_one_fold = function(fold, combination) {
      hyperparams <- replace_at_list(self$fit_params, combination)

      x_training <- get_records(self$x, fold$training)
      y_training <- get_records(self$y, fold$training)
      x_testing <- get_records(self$x, fold$testing)
      y_testing <- get_records(self$y, fold$testing)

      model <- self$training_function(
        x = x_training,
        y = y_training,
        fit_params = hyperparams
      )
      predictions <- self$predict_function(
        model = model,
        x = x_testing,
        responses = self$responses,
        fit_params = hyperparams
      )

      if (self$is_multivariate) {
        loss <- self$loss_function(
          observed = y_testing,
          predicted = predictions,
          responses = self$responses
        )
      } else {
        loss <- self$loss_function(
          observed = y_testing,
          predicted = predictions$predicted
        )
      }

      return(loss)
    },
    tune = function() stop("Tuner::tune error: Not implemented function")
  )
)
