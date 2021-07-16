#' @importFrom R6 R6Class

#' @include utils.R
#' @include model_helpers.R
#' @include metrics.R

multivariate_loss <- function(observed, predicted, responses) {
  all_metrics <- c()

  for (response_name in names(responses)) {
    response_type <- responses[[response_name]]$type

    loss_function <- pccc
    if (is_numeric_response(response_type)) {
      loss_function <- maape
    }
    current_value <- loss_function(
      observed[[response_name]],
      predicted[[response_name]]$predicted
    )
    all_metrics <- c(all_metrics, current_value)
  }

  return(mean(all_metrics, na.rm = TRUE))
}

get_loss_function <- function(responses, is_multivariate) {
  if (is_multivariate) {
    return(multivariate_loss)
  } else if (is_class_response(responses[[1]]$type)) {
    return(pccc)
  } else {
    return(mse)
  }
}

Tuner <- R6Class(
  classname = "Tuner",
  public = list(
    # Properties --------------------------------------------------

    x = NULL,
    y = NULL,
    responses = NULL,
    is_multivariate = NULL,
    hyperparams = NULL,
    other_params = NULL,

    training_function = NULL,
    predict_function = NULL,
    loss_function = NULL,

    cross_validator = NULL,
    cv_type = NULL,
    folds_number = NULL,
    testing_proportion = NULL,

    tabs_number = NULL,

    all_combinations = NULL,
    combinations_number = NULL,
    best_combination = NULL,

    # Constructor --------------------------------------------------

    initialize = function(...,
                          x,
                          y,
                          responses,
                          is_multivariate,
                          training_function,
                          predict_function,
                          hyperparams,
                          other_params,
                          cv_type,
                          folds_number,
                          testing_proportion,
                          loss_function = NULL,
                          tabs_number = 0) {
      self$x <- x
      self$y <- y
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
        get_loss_function(responses, is_multivariate)
      )
      self$tabs_number <- tabs_number
      self$other_params <- other_params

      self$cross_validator <- get_cross_validator(
        type = cv_type,
        records_number = nrow(x),
        folds_number = folds_number,
        testing_proportion = testing_proportion
      )
      self$all_combinations <- expand.grid(hyperparams)
      self$combinations_number <- nrow(self$all_combinations)
    },

    # Methods --------------------------------------------------

    tune = function() {
      echo("%s*** Tuning ***", get_tabs(self$tabs_number))
      echo(
        "%sTotal combinations: %s",
        get_tabs(self$tabs_number),
        self$combinations_number
      )

      folds <- self$cross_validator$get_folds()
      self$all_combinations$loss <- as.numeric(NA)

      for (combination_i in 1:self$combinations_number) {
        echo(
          "%sCombination: %s / %s",
          get_tabs(self$tabs_number + 1),
          combination_i,
          self$combinations_number
        )

        combination <- self$all_combinations[combination_i, ]

        loss_values <- c()

        for (fold_i in 1:self$cross_validator$folds_number) {
          echo(
            "%s%s: %s / %s",
            get_tabs(self$tabs_number + 2),
            self$cross_validator$name,
            fold_i,
            self$cross_validator$folds_number
          )

          fold <- folds[[fold_i]]
          x_training <- get_records(self$x, fold$training)
          y_training <- get_records(self$y, fold$training)
          x_testing <- get_records(self$x, fold$testing)
          y_testing <- get_records(self$y, fold$testing)

          model <- self$training_function(
            x = x_training,
            y = y_training,
            hyperparams = combination,
            other_params = self$other_params
          )
          predictions <- self$predict_function(
            model = model,
            x = x_testing,
            responses = self$responses,
            hyperparams = combination,
            other_params = self$other_params
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
          loss_values <- c(loss_values, loss)
        }

        self$all_combinations[combination_i, "loss"] <- mean(
          loss_values,
          na.rm = TRUE
        )
      }

      self$all_combinations <- arrange(self$all_combinations, -loss)
      self$best_combination <- as.list(self$all_combinations[1, ])
    }
  )
)