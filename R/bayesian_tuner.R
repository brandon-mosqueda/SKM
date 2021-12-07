#' @importFrom R6 R6Class
#' @importFrom rBayesianOptimization BayesianOptimization

#' @include utils.R
#' @include tuner.R

BayesianTuner <- R6Class(
  classname = "BayesianTuner",
  inherit = Tuner,
  public = list(
    # Properties --------------------------------------------------

    folds = NULL,
    current_combination = 1,
    samples_number = NULL,
    iterations_number = NULL,

    # Constructor --------------------------------------------------

    initialize = function(...) {
      super$initialize(...)
      self$samples_number <- samples_number
      self$iterations_number <- iterations_number
    },

    # Methods --------------------------------------------------

    model_evaluation_wrapper = function (...) {
      hyperparams <- list(...)
      loss_values <- c()

      echo(
        "%sCombination: %s",
        get_tabs(self$tabs_number + 1),
        self$current_combination
      )
      self$current_combination <- self$current_combination + 1

      for (fold_i in seq_along(self$folds)) {
        echo(
          "%s%s: %s / %s",
          get_tabs(self$tabs_number + 2),
          self$cross_validator$name,
          fold_i,
          self$cross_validator$folds_number
        )

        fold <- self$folds[[fold_i]]
        loss <- self$eval_one_fold(fold = fold, combination = hyperparams)
        loss_values <- c(loss_values, loss)
      }

      return(list(Score = -mean(loss_values), Pred = 0))
    },
    tune = function() {
      self$folds <- self$cross_validator$get_folds()

      echo("%s*** Bayesian Optimization Tuning ***", get_tabs(self$tabs_number))
      optimizer <- BayesianOptimization(
        FUN = self$model_evaluation_wrapper,
        bounds = self$hyperparams,
        init_points = self$samples_number,
        init_grid_dt = NULL,
        n_iter = self$iterations_number,
        acq = "ucb",
        verbose = FALSE
      )

      self$all_combinations <- as.data.frame(optimizer$History)
      self$all_combinations$Round <- NULL
      colnames(self$all_combinations)[ncol(self$all_combinations)] <- "loss"

      self$combinations_number <- nrow(self$all_combinations)

      self$best_combination <- as.list(optimizer$Best_Par)
      self$best_combination$loss <- optimizer$Best_Value

      return(invisible(self$best_combination))
    }
  )
)
