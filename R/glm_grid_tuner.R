#' @importFrom R6 R6Class

#' @include utils.R
#' @include model_helpers.R
#' @include grid_tuner.R

GLMGridTuner <- R6Class(
  classname = "GLMGridTuner",
  inherit = GridTuner,
  public = list(
    # Properties --------------------------------------------------

    best_model = NULL,
    best_loss = Inf,

    # Methods --------------------------------------------------

    eval_one_fold = function(folds, combination, cv_folds_number = NULL) {
      hyperparams <- replace_at_list(self$fit_params, combination)
      hyperparams$cv_folds_number <- cv_folds_number
      hyperparams$folds <- folds

      model <- self$training_function(x = x, y = y, fit_params = hyperparams)
      loss <- model$cvm[model$index["min", ]]

      if (loss < self$best_loss) {
        self$best_model <- model
      }

      return(loss)
    },
    tune = function() {
      if (self$combinations_number == 1) {
        self$best_combination <- as.list(self$all_combinations)

        self$eval_one_fold(
          NULL,
          self$best_combination,
          cv_folds_number = self$cross_validator$folds_number
        )

        self$best_combination$model <- self$best_model

        return(invisible(self$best_combination))
      }

      echo("%s*** Grid Search Tuning ***", get_tabs(self$tabs_number))
      echo(
        "%sTotal combinations: %s",
        get_tabs(self$tabs_number),
        self$combinations_number
      )

      folds <- self$cross_validator$get_folds()
      folds <- format_glmnet_folds(folds)

      for (combination_i in seq(self$combinations_number)) {
        echo(
          "%sCombination: %s / %s",
          get_tabs(self$tabs_number + 1),
          combination_i,
          self$combinations_number
        )

        combination <- as.list(self$all_combinations[
          combination_i, ,
          drop = FALSE
        ])

        loss <- self$eval_one_fold(folds = folds, combination = combination)

        self$all_combinations[combination_i, "loss"] <- mean(
          loss,
          na.rm = TRUE
        )
      }

      self$all_combinations <- self$all_combinations[
        order(self$all_combinations$loss), ,
        drop = FALSE
      ]
      self$best_combination <- as.list(self$all_combinations[1, , drop = FALSE])
      self$best_combination$model <- self$best_model

      return(invisible(self$best_combination))
    }
  )
)
