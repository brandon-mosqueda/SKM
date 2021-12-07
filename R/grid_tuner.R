#' @importFrom R6 R6Class

#' @include utils.R
#' @include tuner.R

GridTuner <- R6Class(
  classname = "GridTuner",
  inherit = Tuner,
  public = list(
    # Properties --------------------------------------------------

    grid_proportion = NULL,

    # Constructor --------------------------------------------------

    initialize = function(...,
                          hyperparams,
                          grid_proportion) {
      super$initialize(..., hyperparams = hyperparams)
      self$grid_proportion <- grid_proportion

      all_combinations <- expand.grid(
        self$hyperparams,
        KEEP.OUT.ATTRS = FALSE,
        stringsAsFactors = FALSE
      )
      combinations_number <- nrow(all_combinations)

      combinations_indices <- sample(
        combinations_number,
        ceiling(combinations_number * self$grid_proportion)
      )
      all_combinations <- all_combinations[combinations_indices, ]
      combinations_number <- nrow(all_combinations)
      all_combinations$loss <- as.numeric(NA)

      self$all_combinations <- all_combinations
      self$combinations_number <- combinations_number
    },

    # Methods --------------------------------------------------

    tune = function() {
      if (self$combinations_number == 1) {
        self$best_combination <- as.list(self$all_combinations)

        return(invisible(self$best_combination))
      }

      echo("%s*** Tuning ***", get_tabs(self$tabs_number))
      echo(
        "%sTotal combinations: %s",
        get_tabs(self$tabs_number),
        self$combinations_number
      )

      folds <- self$cross_validator$get_folds()

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

      self$all_combinations <- self$all_combinations[
        order(self$all_combinations$loss),
      ]
      self$best_combination <- as.list(self$all_combinations[1, ])

      return(invisible(self$best_combination))
    }
  )
)
