#' @importFrom R6 R6Class

#' @include utils.R
#' @include tuner.R
#' @include RBO_BayesianOptimization.R

#' @export
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

    initialize = function(..., samples_number = 10, iterations_number = 10) {
      super$initialize(...)
      self$samples_number <- samples_number
      self$iterations_number <- iterations_number
    },

    # Methods --------------------------------------------------

    model_evaluation_wrapper = function (...) {
      hyperparams <- list(...)
      loss_values <- c()

      echo(
        "%sCombination: %s / %s",
        get_tabs(self$tabs_number + 1),
        self$current_combination,
        self$combinations_number
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

      # All loss functions used in eval_one_fold return a value to be minimized
      # and this functions needs to return a value to be maximized.
      return(list(Score = -mean(loss_values), Pred = 0))
    },
    tune = function() {
      self$folds <- self$cross_validator$get_folds()
      self$combinations_number <- self$samples_number + self$iterations_number

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
      self$all_combinations$loss <- self$all_combinations$loss * -1
      self$all_combinations <- self$all_combinations[
        order(self$all_combinations$loss), ,
        drop = FALSE
      ]

      self$combinations_number <- nrow(self$all_combinations)

      self$best_combination <- as.list(optimizer$Best_Par)
      self$best_combination$loss <- optimizer$Best_Value * -1

      if (need_invert_loss(self$loss_function_name)) {
        self$all_combinations$loss <- self$all_combinations$loss * -1
        self$best_combination$loss <- self$best_combination$loss * -1
      }
      self$all_combinations[[self$loss_function_name]] <-
        self$all_combinations$loss
      self$best_combination[[self$loss_function_name]] <-
        self$best_combination$loss
      self$all_combinations$loss <- NULL
      self$best_combination$loss <- NULL

      return(invisible(self$best_combination))
    }
  )
)
