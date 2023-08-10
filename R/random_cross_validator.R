#' @importFrom R6 R6Class

#' @include cross_validator.R

RandomCV <- R6Class(
  classname = "RandomCV",
  inherit = CrossValidator,
  public = list(
    # Constructor --------------------------------------------------

    initialize = function(folds_number,
                          records_number,
                          testing_proportion,
                          name = "RandomCV") {
      super$initialize(
        folds_number = folds_number,
        records_number = records_number,
        testing_proportion = testing_proportion,
        name = name
      )
    },

    # Methods --------------------------------------------------

    get_folds = function() {
      folds <- list()
      records <- seq(self$records_number)

      for (fold_num in 1:self$folds_number) {
        current_fold <- list(num = fold_num)
        current_fold$testing <- sample(
          records,
          ceiling(self$records_number * self$testing_proportion)
        )
        current_fold$training <- records[-current_fold$testing]

        folds[[fold_num]] <- current_fold
      }

      return(folds)
    }
  )
)
