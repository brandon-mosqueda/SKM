#' @importFrom R6 R6Class

#' @include utils.R
#' @include tuner.R
#' @include bayesian_tuner.R

GLMBayesianTuner <- R6Class(
  classname = "GLMBayesianTuner",
  inherit = BayesianTuner,
  public = list(
    # Properties --------------------------------------------------

    best_model = NULL,
    best_loss = Inf,
    cv_folds_number = NULL,

    # Methods --------------------------------------------------

    model_evaluation_wrapper = function (...) {
      hyperparams <- list(...)
      hyperparams <- replace_at_list(self$fit_params, hyperparams)

      if (!is.null(self$folds)) {
        hyperparams$folds <- format_glmnet_folds(self$folds)
      }
      hyperparams$cv_folds_number <- self$cv_folds_number

      echo(
        "%sCombination: %s",
        get_tabs(self$tabs_number + 1),
        self$current_combination
      )
      self$current_combination <- self$current_combination + 1

      model <- self$training_function(x = x, y = y, fit_params = hyperparams)
      loss <- model$cvm[model$index["min", ]]

      if (loss < self$best_loss) {
        self$best_model <- model
      }

      return(list(Score = -mean(loss), Pred = 0))
    },
    tune = function() {
      if (!is.list(self$hyperparams$alpha)) {
        self$best_combination <- self$hyperparams
        self$best_combination$loss <- as.numeric(NA)

        self$all_combinations <- as.data.frame(self$best_combination)

        self$cv_folds_number <- self$cross_validator$folds_number

        self$model_evaluation_wrapper(alpha = self$hyperparams$alpha)

        self$best_combination$model <- self$best_model

        return(invisible(self$best_combination))
      }

      super$tune()
      self$best_combination$model <- self$best_model

      return(invisible(self$best_combination))
    }
  )
)
