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
      self$other_params <- other_params

      self$cross_validator <- get_cross_validator(
        type = self$cv_type,
        records_number = nrow(self$x),
        folds_number = self$folds_number,
        testing_proportion = self$testing_proportion
      )
      self$all_combinations <- expand.grid(self$hyperparams)
      self$combinations_number <- nrow(self$all_combinations)
    },

    # Methods --------------------------------------------------

    eval_one_fold = function(fold, combination) {
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

      return(loss)
    },
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
          loss <- self$eval_one_fold(fold = fold, combination = combination)
          loss_values <- c(loss_values, loss)
        }

        self$all_combinations[combination_i, "loss"] <- mean(
          loss_values,
          na.rm = TRUE
        )
      }

      self$all_combinations <- arrange(self$all_combinations, loss)
      self$best_combination <- as.list(self$all_combinations[1, ])
      attr(self$best_combination, "out.attrs") <- NULL
    }
  )
)
