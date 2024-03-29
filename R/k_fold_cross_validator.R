#' @importFrom R6 R6Class

#' @include cross_validator.R

KFoldCV <- R6Class(
  classname = "KFoldCV",
  inherit = CrossValidator,
  public = list(
    # Constructor --------------------------------------------------

    initialize = function(folds_number, records_number, name = "KFoldCV") {
      super$initialize(
        folds_number = folds_number,
        records_number = records_number,
        name = name
      )
    },

    # Methods --------------------------------------------------

    get_folds = function() {
      folds_vector <- cut(
        sample(self$records_number, self$records_number),
        breaks = self$folds_number
      )
      folds_vector <- findInterval(folds_vector, seq(self$records_number))

      folds <- list()
      records <- seq(self$records_number)

      for (fold_num in 1:self$folds_number) {
        current_fold <- list(num = fold_num)
        current_fold$testing <- which(folds_vector == fold_num)
        current_fold$training <- records[-current_fold$testing]

        folds[[fold_num]] <- current_fold
      }

      return(folds)
    }
  )
)
