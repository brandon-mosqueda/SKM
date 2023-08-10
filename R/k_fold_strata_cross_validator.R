#' @importFrom R6 R6Class

#' @include cross_validator.R
#' @include utils.R

KFoldStrataCV <- R6Class(
  classname = "KFoldStrataCV",
  inherit = CrossValidator,
  public = list(
    # Properties --------------------------------------------------

    data = NULL,

    # Constructor --------------------------------------------------

    initialize = function(folds_number,
                          data,
                          name = "KFoldStrataCV") {
      super$initialize(
        folds_number = folds_number,
        records_number = length(data),
        name = name
      )

      self$data <- data
    },

    # Methods --------------------------------------------------

    get_folds = function() {
      classes <- levels(self$data)
      folds <- vector("list", self$folds_number)
      classes_folds <- list()

      for (class in classes) {
        records <- which(self$data == class)
        records_num <- length(records)
        folds <- cv_kfold(records_num, self$folds_number)

        classes_folds[[class]] <- list(
          records = records,
          records_num = records_num,
          folds = folds
        )
      }

      for (i in seq(folds)) {
        fold <- list(num = i)

        for (class_fold in classes_folds) {
          fold$training <- c(
            fold$training,
            class_fold$records[class_fold$folds[[i]]$training]
          )

          fold$testing <- c(
            fold$testing,
            class_fold$records[class_fold$folds[[i]]$testing]
          )
        }

        folds[[i]] <- fold
      }

      return(folds)
    }
  )
)
