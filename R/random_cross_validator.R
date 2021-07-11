#' @importFrom R6 R6Class

#' @include cross_validator.R

RandomCV <- R6Class(
  classname = "RandomCV",
  inherit = CrossValidator,
  public = list(
    # Constructor --------------------------------------------------

    initialize = function(...) {
      super$initialize(..., name = "RandomCV")
    },

    # Methods --------------------------------------------------

    get_folds = function() {
      folds <- list()
      records <- seq(self$records_number)

      for (fold_num in 1:self$folds_number) {
        current_fold <- list()
        current_fold$testing <- sample(
          records,
          self$records_number * self$testing_proportion
        )
        current_fold$training <- records[-current_fold$testing]

        folds[[fold_num]] <- current_fold
      }

      return(folds)
    }
  )
)